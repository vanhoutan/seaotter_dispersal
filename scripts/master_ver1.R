

#####################################################
#####################################################
##### bring in needed packages, data, etc

##### read in packages
library(here)
library(tidyverse)

library(sf)
library(rgdal)
library(raster)

library(forcats)
library(lemon)
library(lubridate)

library(marmap)
library(gdistance)
library(raster)

library(MASS)
library(fitdistrplus)
library(VGAM)

library(gridExtra)
library(reshape2)


##### add plotting theme
theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title=element_blank(),
          strip.text=element_text(hjust=0) )}


themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major = element_line(colour = "transparent"), panel.grid.minor = element_line(colour = "transparent"),
        axis.line = element_blank(),
        # axis.text.x = element_text(margin = margin( 0.2, unit = "cm"), color = "black", size = 7),
        # axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm"), color = "black", size = 7),
        axis.title = element_text(color = "black", size = 8),
        axis.ticks.length=unit(-0.1, "cm"),
        axis.ticks = element_line(color = "black", size = .25),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.spacing = unit(.5, "lines"),
        #    legend.title=element_blank(),
        #   legend.position = "none",
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 7),
        strip.text=element_text(hjust=0) ,
        strip.text.x = element_blank())


######################################################################################################################
######################################################################################################################
##### Least cost path between resignts based on elevation and bathymetry

######################################################################################################################
##### bring in release data
releases <- read.csv(here::here("data/otter_releases.csv"), stringsAsFactors = FALSE)

##### bring in resights data
##### raw data
resights_daysout <- read.csv(here::here("data/release_resights.csv"), stringsAsFactors = FALSE)
resights_date <- read.csv(here::here("data/post_release_resights.csv"), stringsAsFactors = FALSE)

##### ID to character, not number
resights_daysout$seaotter <- as.character(resights_daysout$seaotter)
resights_date$seaotter <- as.character(resights_date$seaotter)

##### combine all columns
resights <- cbind(resights_date, resights_daysout$days_since_rel)
resights <- resights %>% dplyr::select(-notes, -X, -X.1, -X.2, -X.3, -X.4)

##### fix date
resights$datetime <- as.POSIXct(paste(resights$date, resights$time), format = "%m/%d/%Y %H:%M")
str(resights)

######################################################################################################################
##### resight maps

##### make a spatial sf for mapping
resights_sf <- sf::st_as_sf(resights, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                            +datum=WGS84 +no_defs')

##### make a simpler dataframe, then sf for mapping
resights_short <- resights %>%  dplyr::select(seaotter, latitude, longitude)
resights_short_sf  <- sf::st_as_sf(resights_short, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                                   +datum=WGS84 +no_defs')

##### plot resights 
plot(resights_short_sf) #simple plot to test

##### add some bells and whistles
##### basemap
cencal <- readOGR(dsn = "C:/Users/SBecker/Local-Git/otter_dispersal/data/GIS/coastline", layer = "cencal_baseline")
#https://pubs.usgs.gov/of/2007/1112/

##### bathymetry
bathy <- raster("C:/Users/SBecker/Local-Git/otter_dispersal/data/GIS//bathy/GEBCO2014_-123.9342_35.1272_-120.655_38.482_30Sec_Geotiff.tif")
#plot(bathy, interpolate = TRUE)
bathy_spdf <- as(bathy, "SpatialPixelsDataFrame") # convert to spdf
bathy_df <- as.data.frame(bathy_spdf) # convert to dataframe

##### make variable of only release locations
release_site <- subset(resights_sf, resights_daysout$days_since_rel== 0) 

##### plot resights 
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff),
               breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = resights_short_sf, aes(color = seaotter))+ #resight locations
  geom_path(data = resights, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(resights_short_sf))+ #study site
  facet_wrap(~seaotter) +
  themeo #faceted by sea otter + shoreline/contours/release locations + optional add in pathways between resights



############################################################################################################################
##### least cost path 

###### generate bathymetry data to use with marmap for transition matrix constrains
###### use same bathymetry data used for resight maps
plot(bathy) # higher res than pulling from noaa, but in wrong format for marmap

##### convert higher res raster to bathy format for marmap
bathy_bathy <- as.bathy(bathy)
#plot(bathy_bathy)

###### create transition matrices
# trans1 <- trans.mat(bathy_bathy, min.depth = 5, max.depth = -50)
# trans2 <- trans.mat(bathy_bathy, min.depth = 0, max.depth = -30)
trans3 <- trans.mat(bathy_bathy, min.depth = 5)

##############################################################################
######### generate lcp distances for each otter, each recap 

test_recap <- resights %>% group_by(seaotter, release) %>% nest()

##### create lcp distances (recaptures included!)
testmap_dist2_recap <- test_recap %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} )) 

##### test that this ran correctly 
tail(test_recap)
tail(testmap_dist2_recap)
testmap_dist2_recap$distance[29]
test_recap$data[29]
testmap_path2$distance[42][[1]]
View(testmap_dist2_recap)

##### extract diagonals with manual indexing
test_dist_diag_recap <-  test_recap %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} ),
                                               diag = map(distance, function(x){
                                                 print(x) [# print values
                                                   c(1,1 + cumsum(seq((attr(x, "Size")-1), 1))) #along the diag index
                                                   ]
                                               }))

##### test that this ran correctly
test_dist_diag_recap$diag[4]
testmap_dist2_recap$distance[4]
testmap_path2_recap$data[1]
class(testmap_path2$distance[1])

##### restructure data so that there is one row per otter (combining all recaps into one row, now that distance has been calculated for each separately)
just_diag <- test_dist_diag_recap %>% dplyr::select(seaotter, release, diag)

dist_per_otter <- just_diag %>% group_by(seaotter) %>% nest() %>% 
  mutate(dist = map(data, ~flatten(.x)), dist2 = map(dist, ~as.vector(unlist(.x))), dist3 = map(dist2, ~.x[-1])) 
# flattens, unlists, and then removes first row (first row was column name and didnt make sense)
# dist3 (final useful output) is a list of resight distance values for each otter. All releases are included, but distances are not calculated between different releases

##### take the distance calculations and remove NAs for model fitting (Na would be between end of release 1 and start of release 2, for example)
otter_resight_dist <- dist_per_otter %>% dplyr::select(seaotter, dist3) %>% mutate(dist4 = map(dist3, ~.x[!is.na(.x)])) 


###############################################################################################
##### generate and plot LCP paths

##### generate lcp paths
testmap_path2_recap <- test_recap %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "path")} ))

##### diagonal indexing to identify which #s of paths we need to keep to just look at the resight distances 
test_dist_diag_recap_index <-  test_recap %>% mutate(diag_index = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} ),
                                                     diag = map(diag_index, function(x){
                                                       c(1,1 + cumsum(seq((attr(x, "Size")-1), 1))) #along the diag index  
                                                     }))

##### check that everything is running correctly
test_dist_diag_recap_index$diag_index[1] #the diagonal shown here
test_dist_diag_recap_index$diag[1] # is this list of index #s, minus the last value
test_dist_diag_recap$diag[1] # this shows the equivalent in diag values (last = na)
testmap_path2_recap # this shows that each otter has #paths = # test_dist_diag_recap_index$diag[1] - last value

##### prepare to index paths
##### drop last unneeded value form index list
test_dist_diag_recap_index <- test_dist_diag_recap_index %>% mutate(diag2 = map(diag, ~.x[-length(.x)])) 
test_dist_diag_recap_index$diag2[1]# test if it worked (it did)

#####  make a smaller tibble with just what is needed to prepare to join into a tibble with all data needed to index
diag_paths <- test_dist_diag_recap_index %>% dplyr::select(seaotter, release, diag2) #make smaller tibble
lcp_paths <- left_join(diag_paths, testmap_path2_recap, by = c("seaotter","release")) # join

##### index all paths by just paths on the diagonal
lcp_paths <- lcp_paths %>% mutate(lcp_paths = map2(distance, diag2, ~.x[.y]))  

##### test that everything worked correctly
lcp_paths$distance[[1]][[171]] # compare
lcp_paths$lcp_paths[[1]][[18]] # I think it worked?

##### create df to use in plotting
lcp_df_unnest <- lcp_paths %>% dplyr::select(seaotter, lcp_paths) %>% unnest() 
lcp_df_unnest$lcp_paths[1] # unnested partially but lat/lon still stored in list column which ggplot wont like (or I dont like..)

##### make lat and lon into their own columns
lcp_df_unnest <- lcp_df_unnest %>% 
  mutate(long = map(lcp_paths, ~.x[c(1:((length(.x))/2))]),
         lat = map(lcp_paths, ~.x[- c(1:((length(.x))/2))]))

##### check
lcp_df_unnest$long[1]
lcp_df_unnest$lat[1]

##### final unnest so that it is a list-column free dataframe
lcp_df_unnest <- lcp_df_unnest %>% dplyr::select(seaotter, long, lat)
lcp_df_unnest <- lcp_df_unnest %>% unnest()

##### plot least cost paths, faceted for all otters. Supplemental fig1
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), 
               breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = lcp_df_unnest, aes(x=long, y = lat), color = "black")+
  geom_sf(data = release_site, color = "black", size = 0.5) + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site))+ #study site
  facet_wrap(~seaotter, ncol = 6) +
  themeo 

##### plot least cost paths, 3-5 example otters on one map

otter228 <- subset(lcp_df_unnest, seaotter == "228")
otter526 <- subset(lcp_df_unnest, seaotter == "526")
otter353 <- subset(lcp_df_unnest, seaotter == "353")

release_site_short <- subset(release_site, seaotter == c("228","353","526"))

##### fig1b
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = otter228, aes(x=long, y = lat), color = "#1b9e77")+
  geom_path(data = otter353, aes(x=long, y = lat), color = "#d95f02")+
  geom_path(data = otter526, aes(x=long, y = lat), color = "#e7298a")+
  geom_sf(data = release_site_short, color = "black") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site_short))+ #study site
  themeo 

############################################################################################################
###### export / read in useful data

##### save LEAST COST PATH results for future use
# saveRDS(testmap_path2_recap, "data_output/testmap_path2_recap.rds") # paths (lat/lon) of lcp 
# saveRDS(testmap_dist2_recap, "data_output/testmap_dist2_recap.rds") # distance calculations (km) from lcp
# saveRDS(test_dist_diag_recap, "data_output/test_dist_diag_recap.rds") # lcp distance including diagonal values AKA resights
# saveRDS(otter_resight_dist, "data_output/otter_resight_dist.rds") # tibble of seaotter and diagonal values without NA
# saveRDS(test_dist_diag_recap_index, "data_output/rtest_dist_diag_recap_index.rds") # includes indexing values for diagonals/resights to use in indexing only resight paths 
# write.csv(lcp_df_unnest, "data_output/lcp_df_unnest.csv") # lat/lon dataframe of lcp PATHS for mapping in ggplot

##### read LEAST COST PATH data back in (can run this only instead of regenerating code after first time)
testmap_path2_recap <- readRDS( "data_output/testmap_path2_recap.rds") # paths (lat/lon) of lcp
testmap_dist2_recap <- readRDS( "data_output/testmap_dist2_recap.rds") # distance calculations (km) from lcp
test_dist_diag_recap <- readRDS("data_output/test_dist_diag_recap.rds")  # lcp distance including diagonal values AKA resights
otter_resight_dist <- readRDS("data_output/otter_resight_dist.rds") # tibble of seaotter and diagonal values without NA
test_dist_diag_recap_index <- readRDS("data_output/rtest_dist_diag_recap_index.rds") # includes indexing values for diagonals/resights to use in indexing only resight paths
lcp_df_unnest <- read.csv("data_output/lcp_df_unnest.csv") # lat/lon dataframe of lcp PATHS for mapping in ggplot



##############################################################################################################################
##############################################################################################################################
##### fit models
#############################################################################################################################

#######################################################################################
##### cauchy
##### try for one and go from there
test_cauchy <- fitdistr(otter_resight_dist$dist4[[1]],"cauchy", lower= c(0.01, 0.01))

##### map to all otters except 249
otters_n249 <- otter_resight_dist[-6,] # woruldnt run with 249 included, so fit for all others first
resight_models <- otters_n249 %>%  mutate(cauchy = map(dist4, ~ fitdistr(.x,"cauchy", lower= c(0.01, 0.01)) ))
resight_models$cauchy[41] # test this ran correctly

##### add estimates and parameters
resight_models_est_par <- resight_models %>%  
  mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
         scale = map(cauchy, ~.x[1][[1]][[2]]),
         est_cauchy = map2(dist4, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))

##### fit 249 separately
otter249 <- otter_resight_dist[6,]
resight_models_249 <- otter249 %>%  mutate(cauchy = map(dist4, ~ fitdistr(.x,"cauchy", lower= c(0.1, 0.1)) ))


##### add estimates and parameters for 249
resight_models_est_par_249 <- resight_models_249 %>%  
  mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
         scale = map(cauchy, ~.x[1][[1]][[2]]),
         est_cauchy = map2(dist4, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))


##### add 249 in with rest of cauchy models
resight_models_est_par <- rbind(resight_models_est_par, resight_models_est_par_249)
resight_models_est_par$est_cauchy[3] # test this ran correctly

##### save CAUCHY MODEL FITTING data for future use 
#saveRDS(resight_models_est_par, "data_output/resight_models_est_par.rds")

##### read in CAUCHY MODEL FITTING data (can just run this instead of the above code after first run)
#resight_models_est_par <- readRDS("data_output/resight_models_est_par.rds")


################################################################################################
#### visualize model fitting 
##################################################################################################

##### make plots of each dist histogram overlaid with estimates from cauchy model
### first needs to be a dataframe
dist_cauchy_plotting_df <- resight_models_est_par %>% dplyr::select(seaotter, dist4, est_cauchy) # select needed columns 
dist_cauchy_plotting_df<- dist_cauchy_plotting_df %>% unnest() #unnest will only work if all columns but nesting index column have lists of same length

#### save df for future use
# write.csv(dist_cauchy_plotting_df, "data_output/dist_cauchy_plotting_df.csv")
# dist_cauchy_plotting_df<- read.csv("data_output/dist_cauchy_plotting_df.csv")


######## plot lcp histograms and cauchy model fits (to check it ran correctly and create visualizations)
####### test several methods to decide on visualization

### geom histogram, raw values
hist_raw <- ggplot(dist_cauchy_plotting_df)+
  geom_histogram(aes(x = dist4, y = ..density..), binwidth = 3, fill = "grey", color = "grey")+
  geom_line(aes(x = dist4, y = est_cauchy),stat = "identity", color = "black")+
  facet_rep_wrap(facets = "seaotter", scales = "free_y", ncol = 6, repeat.tick.labels = FALSE)+
  coord_cartesian(xlim = NULL, ylim = c(0,.35), expand = c(0,0))+
  xlab("least cost path distance (km)")+
  themeo

### geom histogram, logged x values to better see what is going on
hist_log <- ggplot(dist_cauchy_plotting_df)+
  geom_histogram(aes(x = log(dist4), y = ..density..), binwidth = .35, fill = "grey", color = "grey")+
  geom_line(aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  facet_rep_wrap(facets = "seaotter", scales = "free_y", ncol = 6, repeat.tick.labels = FALSE)+
  coord_cartesian(xlim = NULL, ylim = c(0,2), expand = c(0,0))+
  xlab("least cost path distance (km)")+
  themeo

#### create a true histogram with no gap, and height adjusted based on width
### create scaled height values and plot for all islands
dist_cauchy_plotting_df_scaledbins <- dist_cauchy_plotting_df %>% 
  group_by(seaotter, dist4) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n/sum(n)) )%>%
  mutate(interval = dist4 - lag(dist4, default = -1)) %>% 
  mutate(width = lead(interval, default = 1)) %>% 
  mutate(height = freq/interval) 

#### plot to see if height is adjusting, will fill values in later
ggplot(dist_cauchy_plotting_df_scaledbins, aes(x = dist4,y = min(round(height, digits = 3)), size = 2.5,  color = "dark gray"))+
  geom_segment(aes(xend = dist4,yend = height), color = "dark gray")+
  facet_wrap(~seaotter)+
  themeo     #There will still be gaps right now as heights

exp_count <- dist_cauchy_plotting_df_scaledbins %>% group_by(seaotter) %>% tidyr::expand(dist4=0:40) %>% ungroup()

### do the join to add in scaled height bins
allcountstest <- left_join(exp_count, dist_cauchy_plotting_df_scaledbins, by = c("seaotter", "dist4")) %>% arrange(seaotter, dist4)

### fill in missing values per island
allcountstest_nona <- allcountstest %>% 
  group_by(seaotter) %>% 
  tidyr::fill(height, .direction = "up") %>% 
  replace_na(list(n = 0, freq = 0, height =0))

#write.csv(allcountstest_nona, "data_output/allcountstest_nona.csv")

# scaled height and logged x
scaled_log <- ggplot(allcountstest_nona)+
  geom_segment(aes(xend = log(dist4), x = log(dist4),y = min(round(height, digits = 4)), yend = height), 
               size = 4,color = "dark gray")+
  geom_line(data= dist_cauchy_plotting_df, aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.7), xlim = c(0,3.5))+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("log dispersal distance (km2)")+
  ylab("")+
themeo #ok, they look good still


######## ok, lets go with scaled logged version
######## fig1a
ggplot(allcountstest_nona)+
  geom_segment(aes(xend = log(dist4), x = log(dist4),y = min(round(height, digits = 4)), yend = height), 
               size = 4, color = "grey")+
  geom_line(data= dist_cauchy_plotting_df, aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.7), xlim = c(0,3.5))+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("log dispersal distance (km2)")+
  scale_color_gradient( low = "#90abc9", high = "#eda621")+
  themeo #ok, they look good still



##### quick sum stats for lcp distances and scale
max(dist_cauchy_plotting_df$dist4) #53
min(dist_cauchy_plotting_df$dist4) #0
mean(dist_cauchy_plotting_df$dist4) #3.098592
median(dist_cauchy_plotting_df$dist4) #1

sd(dist_cauchy_plotting_df$dist4) #5.417875

lcp_max_otter <- dist_cauchy_plotting_df %>% group_by(seaotter) %>% mutate(max_lcp = max(dist4)) %>% summarise(max_lcp = first(max_lcp))
max(lcp_max_otter$max_lcp) #53
min(lcp_max_otter$max_lcp) #2
mean(lcp_max_otter$max_lcp) #13.7619
median(lcp_max_otter$max_lcp) #9
sd(lcp_max_otter$max_lcp) #13.334

lcp_min_otter <- dist_cauchy_plotting_df %>% group_by(seaotter) %>% mutate(min_lcp = min(dist4)) %>% summarise(min_lcp = first(min_lcp))
max(lcp_min_otter$min_lcp) #1
min(lcp_min_otter$min_lcp) #0
mean(lcp_min_otter$min_lcp) #0.04761905
median(lcp_min_otter$min_lcp) #0
sd(lcp_min_otter$min_lcp) #0.2155403

lcp_mean_otter <- dist_cauchy_plotting_df %>% group_by(seaotter) %>% mutate(mean_lcp = mean(dist4)) %>% summarise(mean_lcp = first(mean_lcp))
max(lcp_mean_otter$mean_lcp) #13.1875
min(lcp_mean_otter$mean_lcp) #0.3076923
mean(lcp_mean_otter$mean_lcp) #3.0566
median(lcp_mean_otter$mean_lcp) #2.155556
sd(lcp_mean_otter$mean_lcp) #2.640861

lcp_median_otter <- dist_cauchy_plotting_df %>% group_by(seaotter) %>% mutate(median_lcp = median(dist4)) %>% summarise(median_lcp = first(median_lcp))
max(lcp_median_otter$median_lcp) #8.5
min(lcp_median_otter$median_lcp) #0
mean(lcp_median_otter$median_lcp) #1.785714
median(lcp_median_otter$median_lcp) #1
sd(lcp_median_otter$median_lcp) #1.693398


######################################################################################################################################################################################
######################################################################################################################################################################################
#### Bring in various drivers
##################################################################################################################################################################################

##### read in data
resight_models_est_par <- readRDS(here::here("data_output/resight_models_est_par.rds"))
resights <- read.csv(here::here("data_output/resights_dist_recap.csv"), stringsAsFactors = FALSE)

##### clean up variable names and data type for merging later on
colnames(releases)[colnames(releases)=="ottername"] <- "seaotter"
releases$seaotter <- as.character(releases$seaotter)

###### Combine release data with model fitting data
# Use lcp distance based model fits 
full_otters_dist <- left_join(resight_models_est_par, releases, by = "seaotter")
full_otters_dist

#bring in release datetime for looking at temporal patterns
release_date <- subset(resights, days_since_rel== 0) #make variable of only release dates 
release_date <- release_date %>% dplyr::select(seaotter, date)

release_date$seaotter <- as.character(release_date$seaotter)
release_date$rel_date <- release_date$date

scale_date <- left_join(full_otters_dist, release_date, by = "seaotter")
scale_date

###### create object of all release affiliated metrics
release_metrics <- scale_date %>% dplyr::select(seaotter, scale, rel_date, sex, relnum, relyear, relmonth, relsiteATOS, relage_d, strandage_d, strandsiteATOS )
release_metrics <- release_metrics %>% group_by(seaotter) %>% 
  summarise(scale = first(scale), 
            rel_date = first(rel_date),
            sex = first(sex),
            relnum = first(relnum), 
            relyear = first(relyear), 
            relmonth= first(relmonth), 
            relsiteATOS= first(relsiteATOS),
            relage_d = first(relage_d), 
            strandage_d= first(strandage_d), 
            strandsiteATOS = first(strandsiteATOS))

release_metrics$rel_date_real <- lubridate::mdy(release_metrics$rel_date)


############ fig1c
#plot scale by release date

scale_plot_no225_plain <- ggplot(release_metrics, aes(x = rel_date_real, y = scale))+
  geom_smooth(color = "darkgrey")+
  # geom_line()+
  geom_point()+
  #  geom_point(aes(color = scale),size = 3)+
  #geom_point()+
  xlab("release date")+
  ylab("scale")+
  #  scale_color_gradient( low = "blue4", high = "gold")+
  coord_cartesian(xlim = NULL, ylim = c(-.25,3.25), expand =0)+
  themeo

scale_plot_all_plain <- ggplot(release_metrics, aes(x = rel_date_real, y = scale))+
  geom_smooth(color = "darkgrey")+
  #geom_line()+
  geom_point()+
 # geom_text(aes(label = seaotter))+
  #geom_point(aes(color = scale),size = 3)+
  xlab("release date")+
  ylab("scale")+
  # scale_color_gradient( low = "blue4", high = "gold")+
  coord_cartesian(xlim = NULL, ylim = c(-.25,6.25), expand =0)+
  themeo


############################################################################################################################################
########### Bring in environmental data and generate plots for fig3

######################################################### SST 
ehs_temp <- read.csv("C:/Users/SBecker/Local-Git/otter_dispersal/data/EHS_Temp/ehs_raw_temp.csv") #read in temp data
head(ehs_temp) # check col names
ehs_temp <- ehs_temp %>% dplyr::select(DateTimeStamp, Temp) # select datetime and temp

ehs_temp$DateTimeStamp <- mdy_hm(ehs_temp$DateTimeStamp) # make date a date (POSIXct) format
ehs_temp$date <- date(ehs_temp$DateTimeStamp) # create a date variable from datetime

ehd_temp_daily_mean <- ehs_temp %>% group_by(date) %>% summarise(mean_temp = mean(Temp)) # mean daily temp
ehd_temp_daily_mean_nona <- ehd_temp_daily_mean %>% tidyr::fill(mean_temp) # fill NA with previous value (?)

temp_plot <- ggplot(ehd_temp_daily_mean_nona, aes(x = date, y = mean_temp))+
  geom_smooth(color = "#66a47b", fill = "#66a47b", alpha =0.5, span = 0.6)+
  geom_path()+
  xlab("")+
  ylab("temperature (C)")+
  coord_cartesian( expand = 0)+
  themeo

ehd_temp_daily_mean_nona$year <- year(ehd_temp_daily_mean_nona$date)


############################################################ ENSO
### consider using index instead, for now, use rank
ENSO_rank <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/ENSO_rank.txt", skip = 1)
ENSO_rank_names <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/ENSO_rank.txt", nrow = 1, stringsAsFactors = FALSE)

### combine names and values
names(ENSO_rank) <- ENSO_rank_names 

### reshape data into tidy format
ENSO <- gather(ENSO_rank, "DECJAN", "JANFEB", "FEBMAR", "MARAPR", "APRMAY" ,
               "MAYJUN", "JUNJUL" ,"JULAUG" ,"AUGSEP", "SEPOCT" ,"OCTNOV", "NOVDEC",
               key = "month", value = "rank")

ENSO_2002_2018 <- ENSO %>% filter(YEAR >= 2002) # select only years in our survey

### create month dataframe to add back in to data
month<- as.character(c("DECJAN", "JANFEB", "FEBMAR", "MARAPR", "APRMAY" ,
                       "MAYJUN", "JUNJUL" ,"JULAUG" ,"AUGSEP", "SEPOCT" ,"OCTNOV", "NOVDEC"))
month_val <- as.numeric(c(1:12))
month_rank <- as.data.frame(cbind(month, month_val), stringsAsFactors = FALSE)
month_rank$month_val <- as.numeric(month_rank$month_val)

### add month back in and arrange by month 
ENSO_2002_2018 <- left_join(ENSO_2002_2018, month_rank, by = "month" ) # add in month with a join
ENSO_2002_2018 <-  arrange(ENSO_2002_2018, YEAR, month_val) # arrane 
ENSO_2002_2018 <- ENSO_2002_2018 %>% dplyr::select(YEAR, month, rank, month_val) # drop unneeded columns
ENSO_2002_2018$yearmon<- paste(ENSO_2002_2018$YEAR,ENSO_2002_2018$month_val) # add in year-month to one column
ENSO_2002_2018$yearmon <- parse_date_time(ENSO_2002_2018$yearmon, "ym") # give this a date value

### plot by date!
enso_plot<- ggplot(ENSO_2002_2018, aes(x = yearmon, y= rank))+
  geom_smooth(color = "#66a47b", fill = "#66a47b", alpha =0.5, span = 0.6)+
  geom_line()+
  xlab("")+
  ylab("MEI rank")+
  coord_cartesian( expand = 0)+
  themeo

########################################## PDO (last few months of 2018 not available)
PDO2 <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/PDO2.txt", skip = 1)
PDO2_names <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/PDO2.txt", nrow = 1, stringsAsFactors = FALSE)

### combine names and values
names(PDO2) <- PDO2_names 
PDO2$YEAR <- as.character(PDO2$YEAR)

### reshape data into tidy format
PDO2 <- gather(PDO2, "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT" ,"NOV", "DEC",
               key = "month", value = "rank")

PDO2_2002_2018 <- PDO2 %>% filter(YEAR >= "2002") # select only years in survey

### clean up data
data_clean <- function(x) sapply (strsplit(x , '[*]' ), `[` , 1)
PDO2_2002_2018$YEAR<- data_clean(PDO2_2002_2018$YEAR)

PDO2_2002_2018$Date <- paste(PDO2_2002_2018$YEAR, PDO2_2002_2018$month, "15", sep="-") # create a year month day column (day chosen at random)
PDO2_2002_2018$Date <- ymd(PDO2_2002_2018$Date) # make it a date object

pdo2_plot<- ggplot(PDO2_2002_2018, aes(x = Date, y= rank))+
  geom_smooth(color = "#66a47b", fill = "#66a47b", alpha =0.5, span = 0.6)+
  geom_line()+
  xlab("year")+
  ylab("PDO rank")+
  coord_cartesian( expand = 0)+
  themeo

################################################## visualize environmental drivers
#### fig3 
grid.arrange( temp_plot, enso_plot, pdo2_plot,
              layout_matrix = rbind(c(1,1),
                                    c(2,2),
                                    c(3,3)))

############################################################################################################################
####### read in and format additional drivers: population, demographic, surrogacy


################### Local otter population density
#elkhorn slough population density for only survey years
es_pop_dens <- read.csv(here::here("data/censusES1985_2018.csv"), stringsAsFactors = FALSE)
density_2002_2018 <- es_pop_dens %>% dplyr::filter(year >= 2002) # select only years in our survey

#fill 2011 na value
#density_2002_2018_nona <- density_2002_2018 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))

library(imputeTS)
density_2002_2018_nona<- na_ma(density_2002_2018, k = 1)


#create a df for just elkhorn slough otters
es_otters_dist <- release_metrics %>% dplyr::filter(relsiteATOS == "321")

# calulate median/mean dispersal rate per year for elkhorn slough otters
es_disp_dist_med <- es_otters_dist %>% group_by(relyear) %>% summarize(med_scale = median(scale))
es_disp_dist_mean <- es_otters_dist %>% group_by(relyear) %>% summarize(mean_scale = mean(scale))

#drop  2016
#density_2002_2018_nona <- density_2002_2018_nona[-15,]


########################
## body condition

body_condition <- read.csv(here::here("data/body_condition.csv"), stringsAsFactors = FALSE)

body_condition$reldate <- mdy(body_condition$reldate)
body_condition$relweightdate <- mdy(body_condition$relweightdate)
body_condition$bcindxdate <- mdy(body_condition$bcindxdate)

body_condition$w_v_l_date <- body_condition$relweightdate - body_condition$bcindxdate

### calculate and add in a body condition metric
body_condition$condition <- log(body_condition$weightkg.1) / log(body_condition$lengthcm)

body_condition_rel1 <- body_condition %>% filter(relnum == "1")

body_condition_rel1$seaotter <- body_condition_rel1$otter
body_condition_rel1$seaotter <- as.character(body_condition_rel1$seaotter)


###################
## surrogate

surrogates <- read.csv(here::here("data/ottersurrogates.csv"), stringsAsFactors = FALSE)
colnames(surrogates)[colnames(surrogates)=="Sea.Otter"] <- "seaotter"

surrogates_short <- surrogates %>% dplyr::select("seaotter", "surrogate")
surrogates_short$seaotter <- as.character(surrogates_short$seaotter)


#####################
## diet in surrogacy
#all diet
# read in all diet files (two file types and formats so need to do this twice)
library(readxl)
file.list1 <- list.files(here::here("/data/diet"), pattern='*.xlsx', full.names = TRUE)
df.list1 <- lapply(file.list1, read_xlsx)
diet1 <- bind_rows(df.list1, .id="id") # bind into one df

file.list2 <- list.files(here::here("/data/diet"), pattern='*.xls', full.names = TRUE)
df.list2 <- lapply(file.list2, read_excel)
diet2 <- bind_rows(df.list2, .id="id") # bind into one df

colnames(diet1)[colnames(diet1)=="Amount Offered"] <- "Amount_offered_kg"
colnames(diet2)[colnames(diet2)=="Amount Offered"] <- "Amount_offered_kg"

diet1$seaotter <- coalesce(diet1$Otter, diet1$`0tter`, diet1$otter)
library(stringr)
diet2$Amount_offered_kg <- as.numeric(str_extract(diet2$Amount_offered_kg, "[0.000-9.999]+"))
diet2$Amount_offered_kg <- coalesce(diet2$`Total Offered  (kg)`, diet2$Amount_offered_kg)
diet2$seaotter <- coalesce(diet2$Otter, diet2$`0tter`, diet2$otter)
diet1$Amount_offered_kg <- as.numeric(str_extract(diet1$Amount_offered_kg, "[0.000-9.999]+"))

#make short versions with just the columns we need (and that are the same in each format)
diet1_short <- diet1 %>% dplyr::select("seaotter", "Feeding Time", "Food Type", "Amount_offered_kg")
diet2_short <- diet2 %>% dplyr::select("seaotter", "Date", "Food Type", "Amount_offered_kg")

#rename columns so that they are the same and dont contain spaces
colnames(diet1_short)[colnames(diet1_short)=="Feeding Time"] <- "Date"
colnames(diet1_short)[colnames(diet1_short)=="Food Type"] <- "FoodType"
colnames(diet2_short)[colnames(diet2_short)=="Food Type"] <- "FoodType"

#combine into one df
diet <- rbind(diet1_short, diet2_short)

#fix and add needed columns
library(stringr)
diet$Amount_offered_kg_num <- as.numeric(str_extract(diet$Amount_offered_kg, "[0.000-9.999]+"))
diet$DateTime_real <- lubridate::ymd_hms(diet$Date)
diet$year <- year(diet$DateTime_real)
diet$month <- month(diet$DateTime_real)
diet$Date_real <- date(diet$DateTime_real)

# calculate toatl food per month, total live food per month, proportion live food per month
#summarise by month
diet_date_sum <- diet %>% group_by(year, month) %>% mutate(total_food_month_kg = sum(Amount_offered_kg_num))
diet_date_sum<- diet_date_sum %>% group_by(year, month, FoodType) %>%  mutate(foodtype_mont_kg = sum(Amount_offered_kg_num))
diet_date_sum<- diet_date_sum %>% ungroup() %>% mutate(prop_foodtype_month = foodtype_mont_kg/total_food_month_kg)
diet_date_sum <- diet_date_sum %>% group_by(year, month, FoodType) %>% summarise(prop_foodtype_month_sum = first(prop_foodtype_month))

#summarise by otter
diet_seaotter_sum <- diet %>% group_by(seaotter) %>% mutate(total_food_seaotter_kg = sum(Amount_offered_kg_num))
diet_seaotter_sum<- diet_seaotter_sum %>% group_by(seaotter, FoodType) %>%  mutate(foodtype_seaotter_kg = sum(Amount_offered_kg_num))
diet_seaotter_sum<- diet_seaotter_sum %>% ungroup() %>% mutate(prop_foodtype_otter = foodtype_seaotter_kg/total_food_seaotter_kg)
diet_seaotter_sum <- diet_seaotter_sum %>% group_by(seaotter, FoodType) %>% summarise(prop_foodtype_otter_sum = first(prop_foodtype_otter))
diet_seaotter_sum_live <- diet_seaotter_sum %>% dplyr::filter(FoodType == "Live")

#summarise by date by otter
diet_date_mean <- diet %>% group_by(Date_real, seaotter) %>% mutate(total_food_day_kg = sum(Amount_offered_kg_num))
diet_date_mean<- diet_date_mean %>% group_by(Date_real, seaotter, FoodType) %>%  mutate(foodtype_day_kg = sum(Amount_offered_kg_num))
diet_date_mean<- diet_date_mean %>% ungroup() %>% group_by(Date_real, seaotter) %>% mutate(prop_foodtype_day = foodtype_day_kg/total_food_day_kg)
diet_date_mean <- diet_date_mean %>% group_by(seaotter, FoodType) %>% summarise(prop_foodtype_day_mean = mean(prop_foodtype_day))
diet_seaotter_mean_live <- diet_date_mean %>% dplyr::filter(FoodType == "Live")


#### calculate mean release number
relnum_mean <- releases %>% group_by(relyear) %>% mutate(mean_relnum=mean(relnum)) 
relnum_mean_otter_recaps <- relnum_mean %>% group_by(seaotter) %>% mutate(relyear_first = first(relyear), max_relnum = max(relnum)) 

relnum_mean_otter_recaps_sum <- relnum_mean_otter_recaps %>% group_by(seaotter) %>% 
  summarise(max_relnum=first(max_relnum) , relyear_first = first(relyear_first)) 

#### calculate days_in
release_metrics$days_in <- release_metrics$relage_d - release_metrics$strandage_d 

##############################################
### add all drivers into the orginial release_metrics dataframe, then create a master dispersal_drivers dataframe

#### daily temperature raw
release_metrics$date <- release_metrics$rel_date_real
release_metrics <- left_join(release_metrics, ehd_temp_daily_mean_nona, by = "date")

#### body condition at release 1
release_metrics <- left_join(release_metrics, body_condition_rel1, by = "seaotter")

#### surrogates
release_metrics <- left_join(release_metrics, surrogates_short, by = "seaotter" )
release_metrics$outcome <- "success"

#### diet
diet_seaotter_sum_live$seaotter <- as.factor(diet_seaotter_sum_live$seaotter)
diet_seaotter_sum_live$percentlive <- diet_seaotter_sum_live$prop_foodtype_otter_sum * 100
release_metrics <- left_join(release_metrics, diet_seaotter_sum_live, by = "seaotter")

diet_seaotter_mean_live$seaotter <- as.factor(diet_seaotter_mean_live$seaotter)
diet_seaotter_mean_live$percentlive_mean <- diet_seaotter_mean_live$prop_foodtype_day_mean * 100
release_metrics <- left_join(release_metrics, diet_seaotter_mean_live, by = "seaotter")

# write.csv(diet, "data_output/diet.csv")
# write.csv(diet_seaotter_sum_live, "data_output/diet_seaotter_sum_live.csv")
# write.csv(diet_date_sum, "data_output/diet_date_sum.csv")
# write.csv(diet_seaotter_mean_live, "data_output/diet_seaotter_mean_live.csv")

###### mean release
release_metrics <- left_join(release_metrics, relnum_mean_otter_recaps_sum, by = "seaotter")

#write.csv(release_metrics, "data_output/release_metrics.csv")    


#################################################
############### create plots for fig2

## condition
condition_plot_scatter <- ggplot(release_metrics)+
  geom_point(aes(x = fct_rev(seaotter), y = condition, color = sex), size = 2, show.legend = FALSE)+
  coord_flip()+
  xlab("")+
  ylab("log(weight)/log(length)")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo

## sex ratio
release_metrics_sexratio <- release_metrics %>% group_by(sex) %>% summarise(n = n())

sex_ratio_plot <- ggplot(release_metrics_sexratio)+ 
  geom_bar(aes(x= sex, y = n, fill = sex), stat = "identity",show.legend = FALSE)+
  scale_fill_manual(values = c("#3d60c0", "#60003a"))+
  coord_cartesian(xlim= NULL, ylim= c(0,25), expand= 0)+
  xlab("")+
  ylab("count")+
  themeo 

## surrogate
surrogacy_plot <- ggplot(release_metrics)+
  geom_segment(aes(x=strandage_d, xend = relage_d, y = fct_rev(seaotter), yend = fct_rev(seaotter)))+
  geom_point(aes(x = strandage_d, y = fct_rev(seaotter), color = sex), alpha = 0.5, size = 2, show.legend = FALSE)+
  geom_point(aes(x = relage_d, y = fct_rev(seaotter), color = sex), alpha = 1, size = 2, show.legend = FALSE)+
  geom_text(aes(x = 620, y = fct_rev(seaotter), label = surrogate), size = 2)+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  xlab("age (# days old)")+
  ylab("sea otter ID")+
  themeo 

#plot yearly es density
es_density <- ggplot(density_2002_2018_nona, aes(x = year, y = dens.sm))+ # could also look at X5yrtrend (5 year density trend)
  geom_area(fill = "#93bbba")+
  coord_cartesian(expand = 0)+
  xlab("")+
  ylab("population density")+
  scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  themeo


ggplot(density_2002_2018_nona, aes(x = year, y = dens.sm))+ # could also look at X5yrtrend (5 year density trend)
  geom_area(fill = "#93bbba")+
  geom_point(color = "#466867")+
  geom_line(color = "#466867")+
  coord_cartesian(expand = 0)+
  xlab("")+
  ylab("population density")+
  scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  themeo

## release and recaptures plot
successful_otters <- release_metrics %>%  dplyr::select("seaotter", "outcome")
surrogates$seaotter <- as.character(surrogates$seaotter)
surrogates <- left_join(surrogates, successful_otters, by = "seaotter")
surrogates[is.na(surrogates)] <- "mortality"
surrogates <- surrogates %>% mutate(date = dmy(firstreldate), year = year(date))

yearly_success <- surrogates %>% group_by(year,outcome) %>% mutate(outcome_count=n()) %>% ungroup()
yearly_success <- yearly_success %>% group_by(year) %>% mutate(yearcount = n(), outcomeratio = outcome_count/yearcount) 
yearly_success_summary <- yearly_success %>% group_by(year, outcome) %>% summarise(outcome_summary = first(outcomeratio))
yearly_success_summary <- yearly_success_summary %>% filter(outcome == "success")
yearly_success_summary_exp <-yearly_success_summary %>% ungroup() %>% tidyr::expand(year=2002:2018)
yearly_success_summary<- left_join(yearly_success_summary_exp, yearly_success_summary, by = "year") 
yearly_success_summary[is.na(yearly_success_summary)] <- 0

release_metrics_otters_year <- release_metrics %>% group_by(relyear_first) %>% summarise(n = n())
release_metrics_otters_year_exp <-release_metrics_otters_year %>% ungroup() %>% tidyr::expand(relyear_first=2002:2018)
release_metrics_otters_year<- left_join(release_metrics_otters_year_exp, release_metrics_otters_year, by = "relyear_first") 
release_metrics_otters_year[is.na(release_metrics_otters_year)] <- 0

release_metrics_otters_year_sum <- release_metrics_otters_year %>% group_by(relyear_first) %>% summarise(otters_year = sum(n))
release_metrics_otters_year_sum$year <- release_metrics_otters_year_sum$relyear_first
release_metrics_otters_successes_year_sum <- left_join(release_metrics_otters_year_sum, yearly_success_summary, by = "year")
release_metrics_otters_successes_year_sum$prop_success <- release_metrics_otters_successes_year_sum$otters_year*release_metrics_otters_successes_year_sum$outcome_summary

relnum_mean_otter_recaps <- relnum_mean %>% group_by(seaotter) %>% mutate(relyear_first = first(relyear), max_relnum = max(relnum)) 
relnum_mean_otter_recaps_sum <- relnum_mean_otter_recaps %>% group_by(seaotter) %>% 
  summarise(max_relnum=first(max_relnum) , relyear_first = first(relyear_first)) 
relnum_mean_recaps_only <- relnum_mean_otter_recaps_sum %>% group_by(relyear_first) %>% dplyr::filter(max_relnum >1)
relnum_mean_recaps_only_sum <- relnum_mean_recaps_only %>% group_by(relyear_first) %>% summarise(recap_ind = n())

releases_recaps_plot <- ggplot(release_metrics_otters_successes_year_sum)+
  geom_bar(aes(x=as.factor(year), y = otters_year), stat = "identity", fill = "#93bbba", alpha =1)+
  geom_bar(data = relnum_mean_recaps_only_sum, aes(x= as.factor(relyear_first), y = recap_ind), stat = "identity",fill = "#2c2332", alpha =0.5)+
  coord_cartesian(xlim= NULL, ylim= c(0,4.5), expand= 0)+
  xlab("")+
  ylab("count")+
  themeo 


#### diet plot
prop_live_diet_sum_plot <- ggplot(release_metrics) +
  geom_point(aes(x=fct_rev(seaotter), y = percentlive, color = sex), size =2, show.legend = FALSE)+
  coord_flip()+
  xlab("")+
  ylab("% total monthly live diet")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo


##### combine panels into grid for figure 2
grid.arrange(surrogacy_plot, prop_live_diet_sum_plot, condition_plot_scatter, releases_recaps_plot, es_density, sex_ratio_plot,
             layout_matrix = rbind(c(1,1,1,1,1,1,2,2,2,3,3,3),
                                   c(1,1,1,1,1,1,2,2,2,3,3,3),
                                   c(1,1,1,1,1,1,2,2,2,3,3,3),
                                   c(1,1,1,1,1,1,2,2,2,3,3,3),
                                   c(1,1,1,1,1,1,2,2,2,3,3,3),
                                   c(1,1,1,1,1,1,2,2,2,3,3,3),
                                   c(4,4,4,4,4,4,4,4,4,4,6,6),
                                   c(4,4,4,4,4,4,4,4,4,4,6,6),
                                   c(5,5,5,5,5,5,5,5,5,7,6,6),
                                   c(5,5,5,5,5,5,5,5,5,7,6,6)))



############################ clean up release metrics df

dispersal_drivers <- release_metrics %>% dplyr::select(seaotter, scale, sex, relyear, relmonth, relsiteATOS, relage_d, strandage_d, strandsiteATOS, percentlive,
                                                       rel_date_real, mean_temp, condition, days_in, relyear_first, surrogate, outcome, max_relnum)

colnames(dispersal_drivers)[colnames(dispersal_drivers)=="mean_temp"] <- "mean_daily_ehs_sst"
colnames(dispersal_drivers)[colnames(dispersal_drivers)=="relyear_first.y"] <- "relyear_first"

#write.csv(dispersal_drivers, "data_output/dispersal_drivers.csv")

########################################################################################################################
## bring environmental variables into dispersal drivers dataframe

library(broom)
################### extract raw and modelled variables for temp, ENSO, PDO
### SST
ehd_temp_daily_mean_nona_simple <- ehd_temp_daily_mean_nona %>% dplyr::select(date, year, mean_temp)
ehd_temp_daily_mean_nona_simple$ID <- seq.int(nrow(ehd_temp_daily_mean_nona_simple))

###### extract loess values
#Create model that will do the same thing as under the hood in ggplot2
model <- loess(mean_temp ~ ID, data = ehd_temp_daily_mean_nona_simple, span = 0.5)

# Add predicted values from model to original dataset using broom library
ehd_temp_daily_mean_nona_simple2 <- augment(model, ehd_temp_daily_mean_nona_simple)

# Plot both lines to test it is working
ggplot(data=ehd_temp_daily_mean_nona_simple2, aes(date,mean_temp)) +
  geom_line(aes(date, mean_temp), color = "black")+
  stat_smooth(method = "loess", span = 0.5) +
  geom_line(aes(date, .fitted), color = "red") +
  themeo

ehs_loess_fitted <- ehd_temp_daily_mean_nona_simple2 %>% dplyr::select(date, .fitted, .se.fit, .resid)

dispersal_drivers$date <- dispersal_drivers$rel_date_real
dispersal_drivers <- left_join(dispersal_drivers, ehs_loess_fitted, by = "date")

colnames(dispersal_drivers)[colnames(dispersal_drivers)==".fitted"] <- "sst_loess_pred"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".se.fit"] <- "sst_loess_se"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".resid"] <- "sst_loess_resid"

### PDO
PDO2_2002_2018 <- PDO2_2002_2018 %>% arrange(Date)
PDO2_2002_2018$ID <- seq.int(nrow(PDO2_2002_2018))
model_pdo <- loess(rank ~ ID, data = PDO2_2002_2018, span = 0.6)

# Add predicted values from model to original dataset using broom library
PDO2_2002_2018_2 <- augment(model_pdo, PDO2_2002_2018)

# Plot both lines
ggplot(data=PDO2_2002_2018_2, aes(Date,rank)) +
  geom_line(aes(Date,rank), color = "black")+
  stat_smooth(method = "loess", span = 0.6) +
  geom_line(aes(Date, .fitted), color = "red") +
  themeo

PDO_loess_fitted <- PDO2_2002_2018_2 %>% dplyr::select(Date, .fitted, .se.fit, .resid)
PDO_loess_fitted$date <- ymd(PDO_loess_fitted$Date)
PDO_loess_fitted$relmonth <- month(PDO_loess_fitted$date)
PDO_loess_fitted$relyear <- year(PDO_loess_fitted$date)
colnames(PDO_loess_fitted)[colnames(PDO_loess_fitted)=="date"] <- "date"

# add in loess pdo
dispersal_drivers <- left_join(dispersal_drivers, PDO_loess_fitted, by = c("relmonth", "relyear"))

#change colnames and add in raw pdo
PDO2_2002_2018$relmonth <- lubridate::month(PDO2_2002_2018$Date)
PDO2_2002_2018$relyear <- lubridate::year(PDO2_2002_2018$Date)
dispersal_drivers$year <- dispersal_drivers$relyear
dispersal_drivers$month <- dispersal_drivers$relmonth 

PDO2_2002_2018_short <- PDO2_2002_2018 %>% dplyr::select(rank, relmonth, relyear)
dispersal_drivers <- left_join(dispersal_drivers,PDO2_2002_2018_short, by = c("relmonth", "relyear")) 

colnames(dispersal_drivers)[colnames(dispersal_drivers)=="rank"] <- "pdo_index"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".fitted"] <- "pdo_loess_pred"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".se.fit"] <- "pdo_loess_se"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".resid"] <- "pdo_loess_resid"

### ENSO
ENSO_2002_2018
ENSO_2002_2018$date <- ENSO_2002_2018$yearmon
ENSO_2002_2018 <- ENSO_2002_2018 %>% arrange(date)
ENSO_2002_2018$ID <- seq.int(nrow(ENSO_2002_2018))
model_enso <- loess(rank ~ ID, data = ENSO_2002_2018, span = 0.6)

# Add predicted values from model to original dataset using broom library
ENSO_2002_2018_2 <- augment(model_enso, ENSO_2002_2018)

# Plot both lines
ggplot(data=ENSO_2002_2018_2, aes(date,rank)) +
  geom_line(aes(date,rank), color = "black")+
  stat_smooth(method = "loess", span = 0.6) +
  geom_line(aes(date, .fitted), color = "red") +
  themeo

ENSO_loess_fitted <- ENSO_2002_2018_2 %>% dplyr::select(date, .fitted, .se.fit, .resid)
ENSO_loess_fitted$relmonth <- month(ENSO_loess_fitted$date)
ENSO_loess_fitted$relyear <- year(ENSO_loess_fitted$date)


dispersal_drivers <- left_join(dispersal_drivers, ENSO_loess_fitted, by = c("relmonth", "relyear"))
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".fitted"] <- "enso_loess_pred"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".se.fit"] <- "enso_loess_se"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".resid"] <- "enso_loess_resid"

#change colnames and add in raw enso
colnames(ENSO_2002_2018)[colnames(ENSO_2002_2018)=="YEAR"] <- "year"
ENSO_2002_2018$relmonth <- lubridate::month(ENSO_2002_2018$date)
ENSO_2002_2018$relyear <- lubridate::year(ENSO_2002_2018$date)
dispersal_drivers$year <- dispersal_drivers$relyear
dispersal_drivers$month <- dispersal_drivers$relmonth 

ENSO_2002_2018_short <- ENSO_2002_2018 %>% dplyr::select(rank, relmonth, relyear)
dispersal_drivers <- left_join(dispersal_drivers, ENSO_2002_2018_short, by = c("relmonth", "relyear")) 

colnames(dispersal_drivers)[colnames(dispersal_drivers)=="rank"] <- "mei_rank"


################################################### SMOOTHED DEPENDENT VARIABLE- SCALE
## extract loess scale values as well

dispersal_drivers <- dispersal_drivers %>% arrange(rel_date_real)
dispersal_drivers$ID <- seq.int(nrow(dispersal_drivers))

#Create model that will do the same thing as under the hood in ggplot2
model_scale <- loess(scale ~ ID, data = dispersal_drivers, span = 0.6)

# Add predicted values from model to original dataset using broom library
release_metrics_test <- augment(model_scale, release_metrics)
dispersal_drivers <- augment(model_scale, dispersal_drivers)

# Plot both lines
ggplot(release_metrics_test, aes(x = rel_date_real, y = scale)) +
  geom_smooth(span = 0.6) +
  geom_line(aes(rel_date_real, scale), color = "black")+
  geom_line(aes(rel_date_real, .fitted), color = "red") +
  themeo

colnames(dispersal_drivers)[colnames(dispersal_drivers)==".fitted"] <- "scale_loess_pred"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".se.fit"] <- "scale_loess_se"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".resid"] <- "scale_loess_resid"


#################################################################################################################################
#################################################################################################################################
### Create datasets for modeling
### divide by all otters and just ehs otters
### bring in ehs density into ehs otters dataframe

#create a df for just elkhorn slough otters
dispersal_drivers_EHS <- dispersal_drivers %>% dplyr::filter(relsiteATOS == "321")
dispersal_drivers_EHS$year <- dispersal_drivers_EHS$relyear

dispersal_drivers_EHS <- left_join(dispersal_drivers_EHS, density_2002_2018_nona, by = "year")

#drop unneeded columns created by merge
colnames(dispersal_drivers)
dispersal_drivers[,c("date.x","date.y","date")] <- list(NULL)
colnames(dispersal_drivers)

colnames(dispersal_drivers_EHS)
dispersal_drivers_EHS[,c("date.x","date.y","date", "year")] <- list(NULL)
colnames(dispersal_drivers_EHS)

#write.csv(dispersal_drivers_EHS, "data_output/dispersal_drivers_EHS.csv")
#write.csv(dispersal_drivers, "data_output/dispersal_drivers.csv")




########################################################################################################################################
########################################################################################################################################
#### Random forest models
##########################################################################################################################################
############################################################################################################################################
library(randomForest)

### structure data for RF
#remove NAs as it messes up rf
# change characters to factors for RF
dispersal_drivers_nona <- na.omit(dispersal_drivers)
dispersal_drivers_nona$sex <- as.factor(dispersal_drivers_nona$sex)
dispersal_drivers_nona$surrogate <- as.factor(dispersal_drivers_nona$surrogate)

dispersal_drivers_EHS_nona <- na.omit(dispersal_drivers_EHS)
dispersal_drivers_EHS_nona$sex <- as.factor(dispersal_drivers_EHS_nona$sex)
dispersal_drivers_EHS_nona$surrogate <- as.factor(dispersal_drivers_EHS_nona$surrogate)

#or, replace NAs 
dispersal_drivers_fillna <- dispersal_drivers %>% tidyr::fill(mean_daily_ehs_sst, condition, sst_loess_pred, sst_loess_se, sst_loess_resid)
dispersal_drivers_EHS_fillna <- dispersal_drivers_EHS %>% tidyr::fill(mean_daily_ehs_sst, condition, sst_loess_pred, sst_loess_se, sst_loess_resid,
                                                               pupratio, X5yrtrend)


##############################################
# Run several difference RF versions and compare outcomes
# calculate LOOCV sensistivity, R2, MSE, and Variable importnace for each

### LOOCV

### Variable importance
### global model with variable shortlist
new_dd_EHS_nona <- data.frame(dispersal_drivers_EHS_fillna, id=1+c(1:nrow(dispersal_drivers_EHS_fillna))%%36)
id <- unique(new_dd_EHS_nona$id)

new_dd_EHS_nona$seaotter <- as.factor(new_dd_EHS_nona$seaotter)
new_dd_EHS_nona$sex <- as.factor(new_dd_EHS_nona$sex)
new_dd_EHS_nona$surrogate <- as.factor(new_dd_EHS_nona$surrogate)
new_dd_EHS_nona$outcome <- as.factor(new_dd_EHS_nona$outcome)

######### ALL VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_av<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_av <- cbind(loo_ls_av, imp)
}

str(loo_ls_av)
imp_df_loo_ls_av <- as.data.frame(t(loo_ls_av))

str(imp_df_loo_ls_av)
imp_incMSE_ls_av <- imp_df_loo_ls_av[seq(1, nrow(imp_df_loo_ls_av),2),]
imp_IncNodePurity_ls_av <- imp_df_loo_ls_av[seq(2, nrow(imp_df_loo_ls_av),2),]

imp_rank_ls_av <- t(apply(imp_IncNodePurity_ls_av, 1, rank, ties.method = "min"))
str(imp_rank_ls_av)
head(imp_rank_ls_av)

colMeans(imp_rank_ls_av)
colMeans(imp_incMSE_ls_av)
colMeans(imp_IncNodePurity_ls_av)

ls_av_global <- randomForest(scale_loess_pred ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + 
                               strandage_d + condition + mei_rank +
                               mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                               sst_loess_pred + pdo_loess_pred + pdo_index +percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE , 
                             ntree = 1000, mtry = 2)


# variable importance plots
imp_summary_ls_av <- as.data.frame(colMeans(imp_incMSE_ls_av))
names(imp_summary_ls_av)[1] <- "PercentIncMSE"
imp_summary_ls_av$variable <- c("sex", "relmonth", "strandsite", "relage_d", "strandage_d", 
                                "condition" , "mei_rank", "mean_daily_ehs_sst",
                                "days_in", "surrogate", "max_relnum", "sst_loess_pred",
                                "pdo_less_pred", "pdo_index", "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_av)
imp_summary_ls_av$variable <- as.factor(imp_summary_ls_av$variable)

imp_summary_ls_av <- imp_summary_ls_av %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_av <- ggplot(imp_summary_ls_av, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_av, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_av <- NULL
r_squared_train_ls_av <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_av <- cbind(r_squared_test_ls_av, r2_test)  
  r_squared_train_ls_av <- cbind(r_squared_train_ls_av, r2_train)  
}

mean(r_squared_test_ls_av)
mean(r_squared_train_ls_av)
# > mean(r_squared_test_ls_av)
# [1] 0.3475286
# > mean(r_squared_train_ls_av)
# [1] 0.8730564


######## ALL VARIABLES AND RAW SCALE AS RESPONSE
loo_s_av<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_av <- cbind(loo_s_av, imp)
}

str(loo_s_av)
imp_df_loo_s_av <- as.data.frame(t(loo_s_av))

str(imp_df_loo_s_av)
imp_incMSE_s_av <- imp_df_loo_s_av[seq(1, nrow(imp_df_loo_s_av),2),]
imp_IncNodePurity_s_av <- imp_df_loo_s_av[seq(2, nrow(imp_df_loo_s_av),2),]

imp_rank_s_av <- t(apply(imp_IncNodePurity_s_av, 1, rank, ties.method = "min"))
str(imp_rank_s_av)
head(imp_rank_s_av)

colMeans(imp_rank_s_av)
colMeans(imp_incMSE_s_av)
colMeans(imp_IncNodePurity_s_av)

s_av_global <- randomForest(scale~ sex + relmonth + strandsiteATOS + relage_d + 
                              strandage_d + condition + mei_rank +
                              mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                              sst_loess_pred + pdo_loess_pred + pdo_index +percentlive +
                              enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_av <- as.data.frame(colMeans(imp_incMSE_s_av))
names(imp_summary_s_av)[1] <- "PercentIncMSE"
imp_summary_s_av$variable <- c("sex",  "relmonth", "strandsite", "relage_d", "strandage_d", 
                               "condition" , "mei_rank", "mean_daily_ehs_sst",
                               "days_in", "surrogate", "max_relnum", "sst_loess_pred",
                               "pdo_less_pred", "pdo_index", "percent_live_diet", "enso_loess_pred", 
                               "dens.sm", "x5yrtrend")

str(imp_summary_s_av)
imp_summary_s_av$variable <- as.factor(imp_summary_s_av$variable)


imp_summary_s_av <- imp_summary_s_av %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_av <- ggplot(imp_summary_s_av, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_av, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_av <- NULL
r_squared_train_s_av <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_av <- cbind(r_squared_test_s_av, r2_test)  
  r_squared_train_s_av <- cbind(r_squared_train_s_av, r2_train)  
}

mean(r_squared_test_s_av)
mean(r_squared_train_s_av)
# > mean(r_squared_test_s_av)
# [1] -0.1227526
# > mean(r_squared_train_s_av)
# [1] -0.6871564

##################### test what variables are correlated and remove them
library(corrplot)
new_dd_EHS_nona_num <- dplyr::select_if(new_dd_EHS_nona, is.numeric)
new_dd_EHS_nona_num <- new_dd_EHS_nona_num %>%  dplyr::select("scale", "relmonth", "relage_d", 
                                                              "strandage_d", "strandsiteATOS",  "mean_daily_ehs_sst", "condition", "days_in", 
                                                              "max_relnum", "sst_loess_pred", "pdo_loess_pred", "enso_loess_pred", 
                                                              "mei_rank", "pdo_index","pupratio", "X5yrtrend", "percentlive")
corplot_allvar <- cor(new_dd_EHS_nona_num)
corrplot(corplot_allvar)



######### UNCORRELATED VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_2v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_2v <- cbind(loo_ls_2v, imp)
}

str(loo_ls_2v)
imp_df_loo_ls_2v <- as.data.frame(t(loo_ls_2v))

str(imp_df_loo_ls_2v)
imp_incMSE_ls_2v <- imp_df_loo_ls_2v[seq(1, nrow(imp_df_loo_ls_2v),2),]
imp_IncNodePurity_ls_2v <- imp_df_loo_ls_2v[seq(2, nrow(imp_df_loo_ls_2v),2),]

imp_rank_ls_2v <- t(apply(imp_IncNodePurity_ls_2v, 1, rank, ties.method = "min"))
str(imp_rank_ls_2v)
head(imp_rank_ls_2v)

colMeans(imp_rank_ls_2v)
colMeans(imp_incMSE_ls_2v)

colMeans(imp_IncNodePurity_ls_2v)

ls_2v_global <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                               strandage_d + condition  + mei_rank +
                               mean_daily_ehs_sst +  surrogate + 
                               + percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE,
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_2v <- as.data.frame(colMeans(imp_incMSE_ls_2v))
names(imp_summary_ls_2v)[1] <- "PercentIncMSE"
imp_summary_ls_2v$variable <- c("sex",   "relmonth", "strandsite", "relage_d", "strandage_d", 
                                "condition" , "mei_rank", "mean_daily_ehs_sst",
                                "surrogate", 
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_2v)
imp_summary_ls_2v$variable <- as.factor(imp_summary_ls_2v$variable)

imp_summary_ls_2v <- imp_summary_ls_2v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_2v <- ggplot(imp_summary_ls_2v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_2v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_2v <- NULL
r_squared_train_ls_2v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_2v <- cbind(r_squared_test_ls_2v, r2_test)  
  r_squared_train_ls_2v <- cbind(r_squared_train_ls_2v, r2_train)  
}

mean(r_squared_test_ls_2v)
mean(r_squared_train_ls_2v)
# > mean(r_squared_test_ls_2v)
# [1] 0.2657378
# > mean(r_squared_train_ls_2v)
# [1] 0.8518921



######## UNCORRELATED VARIABLES AND RAW SCALE AS RESPONSE
loo_s_2v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex   + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_2v <- cbind(loo_s_2v, imp)
}

str(loo_s_2v)
imp_df_loo_s_2v <- as.data.frame(t(loo_s_2v))

str(imp_df_loo_s_2v)
imp_incMSE_s_2v <- imp_df_loo_s_2v[seq(1, nrow(imp_df_loo_s_2v),2),]
imp_IncNodePurity_s_2v <- imp_df_loo_s_2v[seq(2, nrow(imp_df_loo_s_2v),2),]

imp_rank_s_2v <- t(apply(imp_IncNodePurity_s_2v, 1, rank, ties.method = "min"))
str(imp_rank_s_2v)
head(imp_rank_s_2v)

colMeans(imp_rank_s_2v)
colMeans(imp_incMSE_s_2v)
colMeans(imp_IncNodePurity_s_2v)

s_2v_global <- randomForest(scale~ sex + relmonth + strandsiteATOS + relage_d + 
                              strandage_d + condition  + mei_rank +
                              mean_daily_ehs_sst +  surrogate + 
                              + percentlive +
                              enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_2v <- as.data.frame(colMeans(imp_incMSE_s_2v))
names(imp_summary_s_2v)[1] <- "PercentIncMSE"
imp_summary_s_2v$variable <- c("sex", "relmonth", "strandsite", "relage_d", "strandage_d", 
                               "condition" , "mei_rank", "mean_daily_ehs_sst",
                               "surrogate","percent_live_diet", "enso_loess_pred", 
                               "dens.sm", "x5yrtrend")

str(imp_summary_s_2v)
imp_summary_s_2v$variable <- as.factor(imp_summary_s_2v$variable)

imp_summary_s_2v <- imp_summary_s_2v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_2v <- ggplot(imp_summary_s_2v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_2v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_2v <- NULL
r_squared_train_s_2v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_2v <- cbind(r_squared_test_s_2v, r2_test)  
  r_squared_train_s_2v <- cbind(r_squared_train_s_2v, r2_train)  
}

mean(r_squared_test_s_2v)
mean(r_squared_train_s_2v)
# > mean(r_squared_test_s_2v)
# [1] -0.06537995
# > mean(r_squared_train_s_2v)
# [1] -0.6583423



######### POS VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_3v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_3v <- cbind(loo_ls_3v, imp)
}

str(loo_ls_3v)
imp_df_loo_ls_3v <- as.data.frame(t(loo_ls_3v))

str(imp_df_loo_ls_3v)
imp_incMSE_ls_3v <- imp_df_loo_ls_3v[seq(1, nrow(imp_df_loo_ls_3v),2),]
imp_IncNodePurity_ls_3v <- imp_df_loo_ls_3v[seq(2, nrow(imp_df_loo_ls_3v),2),]

imp_rank_ls_3v <- t(apply(imp_IncNodePurity_ls_3v, 1, rank, ties.method = "min"))
str(imp_rank_ls_3v)
head(imp_rank_ls_3v)

colMeans(imp_rank_ls_3v)
colMeans(imp_incMSE_ls_3v)

colMeans(imp_IncNodePurity_ls_3v)

ls_3v_global <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  
                               +percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_3v <- as.data.frame(colMeans(imp_incMSE_ls_3v))
names(imp_summary_ls_3v)[1] <- "PercentIncMSE"
imp_summary_ls_3v$variable <- c("sex",  "relage_d",  
                                "mei_rank", "mean_daily_ehs_sst",
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_3v)
imp_summary_ls_3v$variable <- as.factor(imp_summary_ls_3v$variable)

imp_summary_ls_3v <- imp_summary_ls_3v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_3v <- ggplot(imp_summary_ls_3v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_3v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_3v <- NULL
r_squared_train_ls_3v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_3v <- cbind(r_squared_test_ls_3v, r2_test)  
  r_squared_train_ls_3v <- cbind(r_squared_train_ls_3v, r2_train)  
}

mean(r_squared_test_ls_3v)
mean(r_squared_train_ls_3v)
# > mean(r_squared_test_ls_3v)
# [1] 0.4622875
# > mean(r_squared_train_ls_3v)
# [1] 0.8640189




######## POS VARIABLES AND RAW SCALE AS RESPONSE
loo_s_3v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_3v <- cbind(loo_s_3v, imp)
}

str(loo_s_3v)
imp_df_loo_s_3v <- as.data.frame(t(loo_s_3v))

str(imp_df_loo_s_3v)
imp_incMSE_s_3v <- imp_df_loo_s_3v[seq(1, nrow(imp_df_loo_s_3v),2),]
imp_IncNodePurity_s_3v <- imp_df_loo_s_3v[seq(2, nrow(imp_df_loo_s_3v),2),]

imp_rank_s_3v <- t(apply(imp_IncNodePurity_s_3v, 1, rank, ties.method = "min"))
str(imp_rank_s_3v)
head(imp_rank_s_3v)

colMeans(imp_rank_s_3v)
colMeans(imp_incMSE_s_3v)
colMeans(imp_IncNodePurity_s_3v)

s_3v_global <- randomForest(scale~ sex  +  relage_d + 
                              mei_rank +
                              mean_daily_ehs_sst +  
                              +percentlive +
                              enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_3v <- as.data.frame(colMeans(imp_incMSE_s_3v))
names(imp_summary_s_3v)[1] <- "PercentIncMSE"
imp_summary_s_3v$variable <- c("sex",  "relage_d", 
                               "mei_rank", "mean_daily_ehs_sst",
                               "percent_live_diet", "enso_loess_pred", 
                               "dens.sm", "x5yrtrend")

str(imp_summary_s_3v)
imp_summary_s_3v$variable <- as.factor(imp_summary_s_3v$variable)

imp_summary_s_3v <- imp_summary_s_3v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_3v <- ggplot(imp_summary_s_3v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_3v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_3v <- NULL
r_squared_train_s_3v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_3v <- cbind(r_squared_test_s_3v, r2_test)  
  r_squared_train_s_3v <- cbind(r_squared_train_s_3v, r2_train)  
}

mean(r_squared_test_s_3v)
mean(r_squared_train_s_3v)
# > mean(r_squared_test_s_3v)
# [1] -0.0943753
# > mean(r_squared_train_s_3v)
# [1] -0.4451316



######## POS VARIABLES AND RAW SCALE AS RESPONSE, dropping more
loo_s_4v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~    mean_daily_ehs_sst +  
                       +percentlive +
                       enso_loess_pred +  X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE , ntree = 1000, mtry = 2)
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_4v <- cbind(loo_s_4v, imp)
}

str(loo_s_4v)
imp_df_loo_s_4v <- as.data.frame(t(loo_s_4v))

str(imp_df_loo_s_4v)
imp_incMSE_s_4v <- imp_df_loo_s_4v[seq(1, nrow(imp_df_loo_s_4v),2),]
imp_IncNodePurity_s_4v <- imp_df_loo_s_4v[seq(2, nrow(imp_df_loo_s_4v),2),]

imp_rank_s_4v <- t(apply(imp_IncNodePurity_s_4v, 1, rank, ties.method = "min"))
str(imp_rank_s_4v)
head(imp_rank_s_4v)

colMeans(imp_rank_s_4v)
colMeans(imp_incMSE_s_4v)
colMeans(imp_IncNodePurity_s_4v)

s_4v_global <- randomForest(scale~ mean_daily_ehs_sst +  
                              +percentlive +
                              enso_loess_pred +  X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_4v <- as.data.frame(colMeans(imp_incMSE_s_4v))
names(imp_summary_s_4v)[1] <- "PercentIncMSE"
imp_summary_s_4v$variable <- c("mean_daily_ehs_sst",
                               "percent_live_diet", "enso_loess_pred", 
                               "x5yrtrend")

str(imp_summary_s_4v)
imp_summary_s_4v$variable <- as.factor(imp_summary_s_4v$variable)

imp_summary_s_4v <- imp_summary_s_4v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_4v <- ggplot(imp_summary_s_4v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_4v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo


#### error calculations
### R squared

r_squared_test_s_4v <- NULL
r_squared_train_s_4v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ mean_daily_ehs_sst +  
                       +percentlive +
                       enso_loess_pred +  X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree= 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_4v <- cbind(r_squared_test_s_4v, r2_test)  
  r_squared_train_s_4v <- cbind(r_squared_train_s_4v, r2_train)  
}

mean(r_squared_test_s_4v)
mean(r_squared_train_s_4v)
# > mean(r_squared_test_s_4v)
# [1] 0.0006571564
# > mean(r_squared_train_s_4v)
# [1] -0.4257987






######### POS Non-Correlated VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_4v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex +  relage_d +
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_4v <- cbind(loo_ls_4v, imp)
}

str(loo_ls_4v)
imp_df_loo_ls_4v <- as.data.frame(t(loo_ls_4v))

str(imp_df_loo_ls_4v)
imp_incMSE_ls_4v <- imp_df_loo_ls_4v[seq(1, nrow(imp_df_loo_ls_4v),2),]
imp_IncNodePurity_ls_4v <- imp_df_loo_ls_4v[seq(2, nrow(imp_df_loo_ls_4v),2),]

imp_rank_ls_4v <- t(apply(imp_IncNodePurity_ls_4v, 1, rank, ties.method = "min"))
str(imp_rank_ls_4v)
head(imp_rank_ls_4v)

colMeans(imp_rank_ls_4v)
colMeans(imp_incMSE_ls_4v)

colMeans(imp_IncNodePurity_ls_4v)

ls_4v_global <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  
                               +percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_4v <- as.data.frame(colMeans(imp_incMSE_ls_4v))
names(imp_summary_ls_4v)[1] <- "PercentIncMSE"
imp_summary_ls_4v$variable <- c("sex",  "relage_d",  
                                
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_4v)
imp_summary_ls_4v$variable <- as.factor(imp_summary_ls_4v$variable)

imp_summary_ls_4v <- imp_summary_ls_4v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_4v <- ggplot(imp_summary_ls_4v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_4v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_4v <- NULL
r_squared_train_ls_4v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_4v <- cbind(r_squared_test_ls_4v, r2_test)  
  r_squared_train_ls_4v <- cbind(r_squared_train_ls_4v, r2_train)  
}

mean(r_squared_test_ls_4v)
mean(r_squared_train_ls_4v)
# > mean(r_squared_test_ls_3v)
# [1] 0.4622875
# > mean(r_squared_train_ls_3v)
# [1] 0.8640189


grid.arrange(varimp_s_av, varimp_ls_av, varimp_s_2v, varimp_ls_2v, varimp_s_4v, varimp_ls_4v,
             layout_matrix = rbind(c(1,2),
                                   c(1,2),
                                   c(1,2),
                                   c(3,4),
                                   c(3,4),
                                   c(5,6)))


################################################################################################
# ls_4v is best

#################################################################################################
# ls_4v visualizations

library(RColorBrewer)
library(pdp)

### Raw variable plots
dens.sm_ls_p <- ggplot(dispersal_drivers_EHS_fillna)+
  geom_point(aes(x = dens.sm, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = dens.sm, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

enso_loess_pred_ls_p <- ggplot(dispersal_drivers_EHS_fillna)+
  geom_point(aes(x = enso_loess_pred, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = enso_loess_pred, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

percentlive.y_ls_p <- ggplot(dispersal_drivers_EHS_fillna)+
  geom_point(aes(x = percentlive, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = percentlive, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

relage_d_ls_p <- ggplot(dispersal_drivers_EHS_fillna)+
  geom_point(aes(x = relage_d, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = relage_d, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

X5yrtrend_ls_p <- ggplot(dispersal_drivers_EHS_fillna)+
  geom_point(aes(x = X5yrtrend, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = X5yrtrend, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

##ICE PLOT
# ice function
ranger_ice <- function(object, newdata) {
  predict(object, newdata)#$predictions
}

ice_enso_loess_d <- pdp::partial(ls_4v_global, pred.var = "enso_loess_pred", pred.fun = ranger_ice) # create ice objects

ice_enso_loess_d <- ice_enso_loess_d %>% # plot enso_loess ICE plots
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_enso_loess_d_p_cent <- ggplot(ice_enso_loess_d, aes(enso_loess_pred, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

# create ice objects
dens.sm_4v <- pdp::partial(ls_4v_global, pred.var = "dens.sm", pred.fun = ranger_ice)

# plot dens.sm ICE plots
dens.sm_4v <- dens.sm_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

dens.sm_4v_p_cent <- ggplot(dens.sm_4v, aes(dens.sm, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

# create ice objects
diet_4v <- pdp::partial(ls_4v_global, pred.var = "percentlive", pred.fun = ranger_ice)

# plot dens.sm ICE plots
diet_4v <- diet_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

diet_4v_p_cent <- ggplot(diet_4v, aes(percentlive, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

# create ice objects
X5yrtrend_4v <- pdp::partial(ls_4v_global, pred.var = "X5yrtrend", pred.fun = ranger_ice)

# plot dens.sm ICE plots
X5yrtrend_4v <- X5yrtrend_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

X5yrtrend_4v_p_cent <- ggplot(X5yrtrend_4v, aes(X5yrtrend, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

# create ice objects
relage_d_4v <- pdp::partial(ls_4v_global, pred.var = "relage_d", pred.fun = ranger_ice)

# plot dens.sm ICE plots
relage_d_4v <- relage_d_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

relage_d_4v_p_cent <- ggplot(relage_d_4v, aes(relage_d, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo


### Two way PDP

partial_enso_dens <- pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "dens.sm"), plot = F, rug = T, chull = T)
partial_enso_trend <- pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "X5yrtrend"), plot = F, rug = T, chull = T)
partial_trend_dens <- pdp::partial(ls_4v_global, pred.var = c("X5yrtrend", "dens.sm"), plot = F, rug = T, chull = T)
partial_enso_diet <- pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "percentlive"), plot = F, rug = T, chull = T)

cols <- c(brewer.pal(11,"Spectral")[c(11,10,9,8,7,6,5,4,3,2,1)])
cols2 <- c(brewer.pal(11,"Spectral")[c(11,10,9,8,7,6,5,4)])
cols3 <- c(brewer.pal(11,"Spectral")[c(10,10,9,8,7,6,5,4)])
cols4 <- c(brewer.pal(11,"Spectral")[c(10,9,8,7,6)])

MEI_dens <- ggplot()+
  geom_tile(data = partial_enso_dens, aes(x = enso_loess_pred, y = dens.sm, z = yhat, fill = yhat))+
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols2)+
  coord_cartesian(expand = 0)+
  labs(x = "MEI index", y = "density")+
  themeo


MEI_growth <- ggplot()+
  geom_tile(data = partial_enso_trend, aes(x = enso_loess_pred, y = X5yrtrend, z = yhat, fill = yhat))+
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols3)+ 
  coord_cartesian(expand = 0)+
  labs(x = "MEI index", y = "population growth")+
  themeo

MEI_diet <- ggplot()+
  geom_tile(data = partial_enso_diet, aes(x = enso_loess_pred, y = percentlive, z = yhat, fill = yhat))+
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols)+
  coord_cartesian(expand = 0)+
  labs(x = "MEI index", y = "% live diet")+
  themeo


dens_growth <- ggplot()+
  geom_tile(data = partial_trend_dens, aes(x = X5yrtrend, y = dens.sm, z = yhat, fill = yhat))+
  # scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols4)+
  coord_cartesian(expand = 0)+
  labs(x = "population growth", y = "density")+
  themeo

#### Variable importance
varimp_ls_4v <- ggplot(imp_summary_ls_4v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_4v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  labs(x = NULL, y = "mean % increase MSE")+
  coord_flip()+
  themeo


grid.arrange(enso_loess_pred_ls_p, dens.sm_ls_p, percentlive.y_ls_p, X5yrtrend_ls_p, relage_d_ls_p, 
             ice_enso_loess_d_p_cent, dens.sm_4v_p_cent, diet_4v_p_cent, X5yrtrend_4v_p_cent, relage_d_4v_p_cent,
             MEI_dens, MEI_growth, MEI_diet, dens_growth,  varimp_ls_4v, 
             layout_matrix = rbind(c(1,1,6,6,11,11,11),
                                   c(2,2,7,7,12,12,12),
                                   c(3,3,8,8,13,13,13),
                                   c(4,4,9,9,14,14,14),
                                   c(5,5,10,10,15,15,15)))


################################################################################## some sum stats for paper

#scale
max(dispersal_drivers$scale) #6.008212
min(dispersal_drivers$scale) #0.01
mean(dispersal_drivers$scale) #1.038502
median(dispersal_drivers$scale) #0.8058683
sd(dispersal_drivers$scale) #1.109983

#scale_loess
max(dispersal_drivers$scale_loess_pred) #2.602865
min(dispersal_drivers$scale_loess_pred) #0.4502193
mean(dispersal_drivers$scale_loess_pred) #1.059812
median(dispersal_drivers$scale_loess_pred) #0.8462748
sd(dispersal_drivers$scale_loess_pred) #0.5828553


#########################################################################################################################################
########################################################################################################################################
####### post-hoc analyses for manuscript


######################################################################################
####### add wind and waves into RF models and compare performance

#read in required packages
require(readxl)
library(data.table)

############# read in wind data
#create a list of the files from your target directory
file_list1 <- list.files(path=here::here("/data/wind/46042/2002_2004"), full.names = T)
file_list2 <- list.files(path=here::here("/data/wind/46042/2005_2018"), full.names = T)

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
wind1 <- data.frame()

#specify needed columns
for (i in 1:length(file_list1)){
  temp_data <- fread(file_list1[i], stringsAsFactors = F,select = c(1:10)) #read in files using the fread function from the data.table package
  wind1 <- rbindlist(list(wind1, temp_data)) #for each iteration, bind the new data to the building dataset
}

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
wind2 <- data.frame()

#specify needed columns
for (i in 1:length(file_list2)){
  temp_data <- fread(file_list2[i], stringsAsFactors = F,select = c(1:10)) #read in files using the fread function from the data.table package
  wind2 <- rbindlist(list(wind2, temp_data)) #for each iteration, bind the new data to the building dataset
}


str(wind1)
str(wind2)

wind2 <- wind2 %>% dplyr::select("YYYY", "MM", "DD", "hh", "WD", "WSPD", "WVHT")
wind1 <- wind1 %>% dplyr::select("YYYY", "MM", "DD", "hh", "WD", "WSPD", "WVHT")

wind <- rbind(wind1, wind2)

############## look at patterns in wind data
# first, give date an actual date meaning: paste together datetime into one date object
wind <- wind %>% mutate(date = paste(YYYY, MM, DD, sep = "-"), time = (hh)) %>% 
  mutate(datetime = paste(date, time, sep = " ")) %>% mutate(datetime_real = ymd_h(datetime)) %>% arrange(datetime_real)

wind$WD <- as.numeric(wind$WD)
wind$WVHT <- as.numeric(wind$WVHT)
wind$WSPD <- as.numeric(wind$WSPD)

wind <- wind %>%  arrange (datetime_real)
str(wind)

wind$WSPD[wind$WSPD >25] <- NA # replace large values (99 = NA) with actual NA
wind$WVHT[wind$WVHT >50] <- NA # replace large values (99 = NA) with actual NA

# plot by date
ggplot(wind)+
  geom_path(aes(x = datetime_real, y = WVHT))

ggplot(wind)+
  geom_path(aes(x = datetime_real, y = WSPD))

#next steps: smooth time series, extract values, extract predicted values for release date 
# (or, moving average following release), look at in relation to scale, scale loess, add into models, 
# compare models with and without, add to visualizations if it seems to make a difference.

# calculate median daily values
wind_dailymean <- wind %>% group_by(date) %>% mutate (med_d_wspd = median(WSPD), med_d_wvht = median(WVHT)) %>% 
  summarise(datetime = first(datetime), med_d_wspd = first(med_d_wspd), med_d_wvht = first(med_d_wvht))

# change to real date and arrange by date
wind_dailymean$date_rel <- ymd_h(wind_dailymean$datetime)
wind_dailymean <- wind_dailymean %>% arrange(date_rel)

# plot by date
ggplot(wind_dailymean)+
  geom_path(aes(x = date_rel, y = med_d_wspd))

ggplot(wind_dailymean)+
  geom_path(aes(x = date_rel, y = med_d_wvht))

## bring environmental variables into dispersal drivers dataframe
################### extract raw and modelled variables for wvht, wspd
### wind
wind_dailymean_simple <- wind_dailymean %>% dplyr::select(date_rel, datetime, date,  med_d_wspd, med_d_wvht)
wind_dailymean_simple$ID <- seq.int(nrow(wind_dailymean_simple))

###### extract loess values
#Create model that will do the same thing as under the hood in ggplot2
model_wspd <- loess(med_d_wspd ~ ID, data = wind_dailymean_simple, span = 0.05)

# Add predicted values from model_wspd to original dataset using broom library
wind_dailymean_simple <- augment(model_wspd, wind_dailymean_simple)

# rename new columns to logical names
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".fitted"] <- "wspd_loess_pred"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".se.fit"] <- "wspd_loess_se"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".resid"] <- "wspd_loess_resid"

# Plot both lines to test it is working
ggplot(wind_dailymean_simple) +
  geom_line(aes(date_rel, med_d_wspd), color = "black")+
  #stat_smooth(method = "loess", span = 0.5) +
  geom_line(aes(date_rel, wspd_loess_pred), color = "red") +
  theme_classic()

######## now waveheight
model_wvht<- loess(med_d_wvht ~ ID, data = wind_dailymean_simple, span = 0.05)

# Add predicted values from model_wspd to original dataset using broom library
wind_dailymean_simple <- augment(model_wvht, wind_dailymean_simple)

colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".fitted"] <- "wvht_loess_pred"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".se.fit"] <- "wvht_loess_se"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".resid"] <- "wvht_loess_resid"


# Plot both lines to test it is working
ggplot(wind_dailymean_simple) +
  geom_line(aes(date_rel, med_d_wvht), color = "black")+
  #stat_smooth(method = "loess", span = 0.5) +
  geom_line(aes(date_rel, wvht_loess_pred), color = "red") +
  theme_classic()

# read in dispersal drivers
dd <- read.csv(here::here("data_output/dispersal_drivers.csv"))

wind_dailymean_simple_fitted <- wind_dailymean_simple %>% dplyr::select(date_rel, date,
                                                                        med_d_wspd, wspd_loess_pred, wspd_loess_se, wspd_loess_resid,
                                                                        med_d_wvht, wvht_loess_pred, wvht_loess_se, wvht_loess_resid )
# fix dates and make same name for join
dd$date_rel <- dd$rel_date_real
dd$date_rel <- ymd(dd$date_rel)
wind_dailymean_simple_fitted$date_rel <- ymd(wind_dailymean_simple_fitted$date)

# join wind and wave values into dd df
dd <- left_join(dd, wind_dailymean_simple_fitted, by = "date_rel")

# plot by date
ggplot(dd)+
  geom_point(aes(x = date_rel, y = wspd_loess_pred))+
  themeo
ggplot(dd)+
  geom_point(aes(x = date_rel, y = wvht_loess_pred))+
  themeo
#plot by scale
ggplot(dd)+
  geom_point(aes(x = wspd_loess_pred, y = scale))+
  themeo
ggplot(dd)+
  geom_point(aes(x = wvht_loess_pred, y = scale))+
  themeo
# plot by loess scale 
ggplot(dd)+
  geom_point(aes(x = wspd_loess_pred, y = scale_loess_pred))+
  themeo
ggplot(dd)+
  geom_point(aes(x = wvht_loess_pred, y = scale_loess_pred))+
  themeo


# add into RF ,models to compare model performance
##############################################

### density drivers with just EHS
dd_EHS <- dd %>% dplyr::filter(relsiteATOS == "321")
dd_EHS$year <- dd_EHS$relyear

dd_EHS <- left_join(dd_EHS, density_2002_2017_nona, by = "year")

#drop NA rows
dd_EHS_nona <- na.omit(dd_EHS)
dd_EHS_nona$sex <- as.factor(dd_EHS_nona$sex)
dd_EHS_nona$surrogate <- as.factor(dd_EHS_nona$surrogate)

new_dd_EHS_nona <- data.frame(dd_EHS_nona, id=1+c(1:nrow(dd_EHS_nona))%%20)
id <- unique(new_dd_EHS_nona$id)

new_dd_EHS_nona$seaotter <- as.factor(new_dd_EHS_nona$seaotter)
new_dd_EHS_nona$sex <- as.factor(new_dd_EHS_nona$sex)
new_dd_EHS_nona$surrogate <- as.factor(new_dd_EHS_nona$surrogate)
new_dd_EHS_nona$outcome <- as.factor(new_dd_EHS_nona$outcome)


######### POS Non-Correlated VARIABLES and LOESS SCALE AS RESPONSE
#### subset for just dates we have wind and wave data for 
# aka same variables but slightly truncated dataset from best fit model above

ls_4v2<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex +  relage_d +
                       +percentlive + 
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  ls_4v2 <- cbind(ls_4v2, imp)
}

str(ls_4v2)
imp_df_ls_4v2 <- as.data.frame(t(ls_4v2))

str(imp_df_ls_4v2)
imp_incMSE_ls_4v2 <- imp_df_ls_4v2[seq(1, nrow(imp_df_ls_4v2),2),]
imp_IncNodePurity_ls_4v2 <- imp_df_ls_4v2[seq(2, nrow(imp_df_ls_4v2),2),]

imp_rank_ls_4v2 <- t(apply(imp_IncNodePurity_ls_4v2, 1, rank, ties.method = "min"))
str(imp_rank_ls_4v2)
head(imp_rank_ls_4v2)

colMeans(imp_rank_ls_4v2)
colMeans(imp_incMSE_ls_4v2)

colMeans(imp_IncNodePurity_ls_4v2)

ls_4v_global_2 <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  
                               +percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_4v2 <- as.data.frame(colMeans(imp_incMSE_ls_4v2))
names(imp_summary_ls_4v2)[1] <- "PercentIncMSE"
imp_summary_ls_4v2$variable <- c("sex",  "relage_d",  
                                
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_4v2)
imp_summary_ls_4v2$variable <- as.factor(imp_summary_ls_4v2$variable)

imp_summary_ls_4v2 <- imp_summary_ls_4v2 %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_4v2 <- ggplot(imp_summary_ls_4v2, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_4v2, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_4v2 <- NULL
r_squared_train_ls_4v2 <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_4v2 <- cbind(r_squared_test_ls_4v2, r2_test)  
  r_squared_train_ls_4v2 <- cbind(r_squared_train_ls_4v2, r2_train)  
}

mean(r_squared_test_ls_4v2)
mean(r_squared_train_ls_4v2)
# > mean(r_squared_test_ls_4v2)
# [1] 0.4454245
# > mean(r_squared_train_ls_4v2)
# [1] 0.8586368



#### now run with wind and wave height
loo_ls_5v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex +  relage_d +
                       +percentlive + wspd_loess_pred + wvht_loess_pred +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_5v <- cbind(loo_ls_5v, imp)
}

str(loo_ls_5v)
imp_df_loo_ls_5v <- as.data.frame(t(loo_ls_5v))

str(imp_df_loo_ls_5v)
imp_incMSE_ls_5v <- imp_df_loo_ls_5v[seq(1, nrow(imp_df_loo_ls_5v),2),]
imp_IncNodePurity_ls_5v <- imp_df_loo_ls_5v[seq(2, nrow(imp_df_loo_ls_5v),2),]

imp_rank_ls_5v <- t(apply(imp_IncNodePurity_ls_5v, 1, rank, ties.method = "min"))
str(imp_rank_ls_5v)
head(imp_rank_ls_5v)

colMeans(imp_rank_ls_5v)
colMeans(imp_incMSE_ls_5v)

colMeans(imp_IncNodePurity_ls_5v)

ls_5v_global <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  wspd_loess_pred + wvht_loess_pred +
                               +percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_5v <- as.data.frame(colMeans(imp_incMSE_ls_5v))
names(imp_summary_ls_5v)[1] <- "PercentIncMSE"
imp_summary_ls_5v$variable <- c("sex",  "relage_d",  
                                
                                "percent_live_diet", "wind_speed" , "wave_height" , "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_5v)
imp_summary_ls_5v$variable <- as.factor(imp_summary_ls_5v$variable)

imp_summary_ls_5v <- imp_summary_ls_5v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_5v <- ggplot(imp_summary_ls_5v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_5v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_5v <- NULL
r_squared_train_ls_5v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_5v <- cbind(r_squared_test_ls_5v, r2_test)  
  r_squared_train_ls_5v <- cbind(r_squared_train_ls_5v, r2_train)  
}

mean(r_squared_test_ls_5v)
mean(r_squared_train_ls_5v)
# > mean(r_squared_test_ls_5v)
# [1] 0.4390418
# > mean(r_squared_train_ls_5v)
# [1] 0.8576461

# compare var importance
grid.arrange(varimp_ls_4v, varimp_ls_5v,
             layout_matrix = rbind(c(1,2)))

### model performance almost identical, but SLIGHTLY LESS when including wind and wave data


###################################################################################################
######## figure S6: compare dispersal by sex
dd_EHS <- read.csv(here::here("data_output/dispersal_drivers_EHS.csv"))
dd <- read.csv(here::here("data_output/dispersal_drivers.csv"))

ggplot(dd_EHS)+
  geom_violin(aes(x = sex, y = scale_loess_pred), fill = "lightgrey")+
  themeo

ggplot(dd_EHS)+
  geom_violin(aes(x = sex, y = scale), fill = "lightgrey")+
  themeo

sum(dd$days_in)


########################################################################################
##### sum stats for tracking effort, release length
otter_resight_dist <- readRDS("data_output/otter_resight_dist.rds") # tibble of seaotter and diagonal values without NA

otter_resight_dist_dist <- otter_resight_dist %>%  dplyr::select(seaotter, dist4) # select just lcp dist and otters
otter_resight_dist_dist <- otter_resight_dist_dist %>% unnest() # expand out of list column

total_dist <- otter_resight_dist_dist %>% group_by(seaotter) %>% summarise(sum_dist = sum(dist4)) # total dist traveled per otter


post_rel_resights <- read.csv(here::here("data/post_release_resights.csv"))

post_rel_resights_short <- post_rel_resights %>% dplyr::select(seaotter, release, date, time, latitude, longitude)
post_rel_resights_short$date_rel <- lubridate::mdy(post_rel_resights_short$date)

post_rel_resights_shortest <- post_rel_resights_short %>% group_by(seaotter) %>% summarise(days_tracked = n()) # numnber days tracked per otter
sum(post_rel_resights_shortest$days_tracked) # number of days tracked TOTAL


################################################################################################################################
########## calculate length of first release
# post_rel_resights_short

# create max_recap column
post_rel_resights_short <- post_rel_resights_short %>% group_by(seaotter) %>% mutate(max_recap = max(release))

# subset just for max recap >1
post_rel_resights_short_recaps <- post_rel_resights_short %>% dplyr::filter(max_recap >1)

# create a first release vs recap_date variable
first_release_length <- post_rel_resights_short_recaps %>% dplyr::filter(release == 1)

first_release_length <- first_release_length %>% group_by(seaotter) %>% summarise(release_date = first(date), recap_date = last(date))

#make dates real dates
first_release_length$release_date <- mdy(first_release_length$release_date)
first_release_length$recap_date <- mdy(first_release_length$recap_date)

# calculate # days between first release and second release
first_release_length <- first_release_length %>% mutate(first_rel_length = recap_date - release_date)

# take average of #days in first release
mean(first_release_length$first_rel_length) # 3.388889 days
median(first_release_length$first_rel_length) # 3 days
range(first_release_length$first_rel_length) # 1- 10 days
sd(first_release_length$first_rel_length) # 2.278774 days



######################################################################################################################################
#####################################################################################################################################
##### look at recapture effect

# figure S5 - median scale by recapture
test_dist_diag_recap <- readRDS("data_output/test_dist_diag_recap.rds")  # lcp distance including diagonal values AKA resights

recap_dist <- test_dist_diag_recap %>% dplyr::select(seaotter, release, diag) # select only needed columns
recap_dist <- recap_dist %>% unnest() # unnest out of list columns

recap_dist <- as.data.frame(recap_dist) # convert to dataframe
recap_dist_nona <- na.omit(recap_dist) # remove NA (NAs occur either between otters or releases and are not real movements)

recap_dist_nona_stats <- recap_dist_nona %>% dplyr::group_by(seaotter, release) %>%
  dplyr::summarise(mean_lcp = mean(diag), med_lcp = median(diag))

recap_dist_nona_n <- recap_dist_nona %>% dplyr::group_by(release) %>%
  summarise(n_distinct(seaotter))

recap_dist_nona %>% dplyr::group_by(seaotter, release) %>% 
  dplyr::summarise(mean_lcp = mean(diag), med_lcp = median(diag)) %>% 
  ggplot()+
  geom_violin(aes(x= release, y = med_lcp, group = release), fill = "lightgrey")+
  themeo

###########################################################################################################################
######## figure 5
#### subset by days in the wild, divide data by recaptured vs single release, and fit cauchy models to each day
# refit cauchy models for days out rather than individual otter
# separate recaptures from non-recaptures

# wrangle data
lcp_dist_sum <- recap_dist_nona %>% group_by (seaotter) %>% mutate(max_recap = max(release)) # gnereate max recap column
lcp_dist_sum <- lcp_dist_sum %>% mutate(recap = ifelse(max_recap >1, "Y", "N")) # genereate column for y/n for recaptures
lcp_dist_sum <- lcp_dist_sum %>%  group_by(seaotter) %>% mutate(days_out = 1:n()) %>% ungroup() # column showing number of days in wild (recap releases pick up where previous release left off)

lcp_dist_sum_recap <- lcp_dist_sum %>% dplyr::filter(recap == "Y") # create a df for only animals that were recaptured
lcp_dist_sum_recap_short <- lcp_dist_sum_recap %>% dplyr::select("days_out", "diag") # create a short version with only necessary columns
lcp_dist_sum_Nrecap <- lcp_dist_sum %>% dplyr::filter(recap == "N") # create a df for only animals that were NOT recaptured
lcp_dist_sum_Nrecap_short <- lcp_dist_sum_Nrecap %>% dplyr::select("days_out", "diag") # create a short version with only necessary columns

lcp_dist_g_daysout_recp <- lcp_dist_sum_recap_short %>% group_by(days_out) %>% nest() # group by days out and nest
lcp_dist_g_daysout_Nrecp <- lcp_dist_sum_Nrecap_short %>% group_by(days_out) %>% nest() # group by days out and nest

lcp_dist_g_daysout_recp$data <- flatten(lcp_dist_g_daysout_recp$data) # flatten to create a simple list column rather than tibble column
lcp_dist_g_daysout_Nrecp$data <- flatten(lcp_dist_g_daysout_Nrecp$data) # flatten to create a simple list column rather than tibble column

######## cauchy

##### recaps
# fit for one test day first
test_cauchy_daysout <- fitdistr(lcp_dist_g_daysout_recp$data[[1]],"cauchy")

lcp_dist_g_daysout_recp <- lcp_dist_g_daysout_recp %>% mutate(list_length = lengths(data))
lcp_dist_g_daysout_recp_10 <- lcp_dist_g_daysout_recp %>% dplyr::filter(list_length > 9)
lcp_dist_g_daysout_recp_18 <- lcp_dist_g_daysout_recp %>% dplyr::filter(list_length > 17)


recap_daysout_models <- lcp_dist_g_daysout_recp_10 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01)) ))
recap_daysout_models$cauchy

##### add estimates and parameters
recap_daysout_models_est_par <- recap_daysout_models %>%  
  mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
         scale = map(cauchy, ~.x[1][[1]][[2]]),
         est_cauchy = map2(data, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))

plot(recap_daysout_models_est_par$days_out, recap_daysout_models_est_par$scale)

cauchy_daysout13 <- fitdistr(lcp_dist_g_daysout_recp$data[[13]],dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01))

lcp_dist_g_daysout_recp_10_15 <- lcp_dist_g_daysout_recp_10 %>% dplyr::filter(list_length < 17)
cauchy_daysout14 <- fitdistr(lcp_dist_g_daysout_recp$data[[14]],dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01))

recap_daysout_models_10_15<- lcp_dist_g_daysout_recp_10_15 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01)) ))
recap_daysout_models_10_15$cauchy[2]

######## cauchy
##### no recaps
# fit for one test day first
test_cauchy_daysout_N <- fitdistr(lcp_dist_g_daysout_Nrecp$data[[1]],"cauchy")

lcp_dist_g_daysout_Nrecp <- lcp_dist_g_daysout_Nrecp %>% mutate(list_length = lengths(data))
lcp_dist_g_daysout_Nrecp_10 <- lcp_dist_g_daysout_Nrecp %>% dplyr::filter(list_length > 9)
lcp_dist_g_daysout_Nrecp_18 <- lcp_dist_g_daysout_Nrecp %>% dplyr::filter(list_length > 17)
lcp_dist_g_daysout_Nrecp_5 <- lcp_dist_g_daysout_Nrecp %>% dplyr::filter(list_length > 5)


recap_daysout_modelsN<- lcp_dist_g_daysout_Nrecp_5 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale =0.1), lower= c(0.01,0.01)) ))
recap_daysout_modelsN$cauchy

##### add estimates and parameters
recap_daysout_models_est_parN <- recap_daysout_modelsN %>%  
  mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
         scale = map(cauchy, ~.x[1][[1]][[2]]),
         est_cauchy = map2(data, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))

plot(recap_daysout_models_est_parN$days_out, recap_daysout_models_est_parN$scale)


# prep data for plotting with ggplot
cauchy_N_recap_params <- recap_daysout_models_est_parN %>% dplyr::select(days_out, location, scale) %>% unnest()
cauchy_N_recap_params$recap <- "N"

cauchy_recap_params <- recap_daysout_models_est_par %>% dplyr::select(days_out, location, scale) %>% unnest()
cauchy_recap_params$recap <- "Y"

cauchy_est <- rbind(cauchy_N_recap_params, cauchy_recap_params)

ggplot()+
  geom_smooth(data = cauchy_est, aes(x = days_out, y = scale, color = recap))+
  geom_point(data = cauchy_est, aes(x = days_out, y = scale, color = recap))+
  coord_cartesian(expand = 0, ylim = c(0,2))+
  facet_wrap(~recap, ncol= 1)+
  themeo
