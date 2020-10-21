##### Least cost path between resignts based on elevation and bathymetry


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

##### add plotting theme
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major = element_line(colour = "transparent"), panel.grid.minor = element_line(colour = "transparent"),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm"), color = "black", size = 7),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm"), color = "black", size = 7),
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
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = resights_short_sf, aes(color = seaotter))+ #resight locations
  geom_path(data = resights, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(resights_short_sf))+ #study site
  facet_wrap(~seaotter) +
  themeo #faceted by sea otter + shoreline/contours/release locations + optional add in pathways between resights



############################################################################################################################
############################################################################################################################
##### least cost path 

###### generate bathymetry data to use with marmap for transition matrix constrains
###### use same bathymetry data used for resight maps
plot(bathy) # higher res than pulling from noaa, but in wrong format for marmap

##### convert higher res raster to bathy format for marmap
bathy_bathy <- as.bathy(bathy)
#plot(bathy_bathy)

###### create transition matrices
trans1 <- trans.mat(bathy_bathy, min.depth = 5, max.depth = -50)
trans2 <- trans.mat(bathy_bathy, min.depth = 0, max.depth = -30)
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

##### plot least cost paths, faceted for all otters
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = lcp_df_unnest, aes(x=long, y = lat), color = "black")+
  geom_sf(data = release_site, color = "black", size = 0.5) + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site))+ #study site
  facet_wrap(~seaotter, ncol = 6) +
  themeo 

##### plot least cost paths, 3-5 example otters on one map

otter228 <- subset(lcp_df_unnest, seaotter == "228")
otter315 <- subset(lcp_df_unnest, seaotter == "315")
otter379 <- subset(lcp_df_unnest, seaotter == "379")
otter587 <- subset(lcp_df_unnest, seaotter == "587")
otter716 <- subset(lcp_df_unnest, seaotter == "716")

release_site_short <- subset(release_site, seaotter == c("228","379","587","716"))

##### fig1a
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = otter228, aes(x=long, y = lat), color = "#1b9e77")+
  geom_path(data = otter379, aes(x=long, y = lat), color = "#d95f02")+
  geom_path(data = otter716, aes(x=long, y = lat), color = "#7570b3")+
  geom_path(data = otter587, aes(x=long, y = lat), color = "#e7298a")+
  geom_sf(data = release_site_short, color = "black") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site_short))+ #study site
  themeo 

############################################################################################################
###### export / read in useful data

##### save LEAST COST PATH results for future use
saveRDS(testmap_path2_recap, "data_output/testmap_path2_recap.rds") # paths (lat/lon) of lcp 
saveRDS(testmap_dist2_recap, "data_output/testmap_dist2_recap.rds") # distance calculations (km) from lcp
saveRDS(test_dist_diag_recap, "data_output/test_dist_diag_recap.rds") # lcp distance including diagonal values AKA resights
saveRDS(otter_resight_dist, "data_output/otter_resight_dist.rds") # tibble of seaotter and diagonal values without NA
saveRDS(test_dist_diag_recap_index, "data_output/rtest_dist_diag_recap_index.rds") # includes indexing values for diagonals/resights to use in indexing only resight paths 
write.csv(lcp_df_unnest, "data_output/lcp_df_unnest.csv") # lat/lon dataframe of lcp PATHS for mapping in ggplot

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
saveRDS(resight_models_est_par, "data_output/resight_models_est_par.rds")

##### read in CAUCHY MODEL FITTING data (can just run this instead of the above code after first run)
resight_models_est_par <- readRDS("data_output/resight_models_est_par.rds")

##### make plots of each dist histogram overlaid with estimates from cauchy model
##### first neds to be a dataframe
dist_cauchy_plotting_df <- resight_models_est_par %>% dplyr::select(seaotter, dist4, est_cauchy) # select needed columns 
dist_cauchy_plotting_df<- dist_cauchy_plotting_df %>% unnest() #unnest will only work if all columns but nesting index column have lists of same length

write.csv(dist_cauchy_plotting_df, "data_output/dist_cauchy_plotting_df.csv")
dist_cauchy_plotting_df<- read.csv("data_output/dist_cauchy_plotting_df.csv")


##### plot lcp histograms and cauchy model fits (to check it ran correctly and create visualizations)
hist_raw <- ggplot(dist_cauchy_plotting_df)+
  geom_histogram(aes(x = dist4, y = ..density..), binwidth = 3, fill = "grey", color = "grey")+
  geom_line(aes(x = dist4, y = est_cauchy),stat = "identity", color = "black")+
  facet_rep_wrap(facets = "seaotter", scales = "free_y", ncol = 6, repeat.tick.labels = FALSE)+
  coord_cartesian(xlim = NULL, ylim = c(0,.35), expand = c(0,0))+
  xlab("least cost path distance (km)")+
  themeo

hist_log <- ggplot(dist_cauchy_plotting_df)+
  geom_histogram(aes(x = log(dist4), y = ..density..), binwidth = .35, fill = "grey", color = "grey")+
  geom_line(aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  facet_rep_wrap(facets = "seaotter", scales = "free_y", ncol = 6, repeat.tick.labels = FALSE)+
  coord_cartesian(xlim = NULL, ylim = c(0,2), expand = c(0,0))+
  xlab("least cost path distance (km)")+
  themeo

 # create scaled height values and plot for all islands
dist_cauchy_plotting_df_scaledbins <- dist_cauchy_plotting_df %>% 
   group_by(seaotter, dist4) %>% 
   summarise(n = n()) %>% 
   mutate(freq = (n/sum(n)) )%>%
   mutate(interval = dist4 - lag(dist4, default = -1)) %>% 
   mutate(width = lead(interval, default = 1)) %>% 
   mutate(height = freq/interval) 


# plot to see if height is adjusting, will fill values in later
ggplot(dist_cauchy_plotting_df_scaledbins, aes(x = dist4,y = min(round(height, digits = 3)), size = 2.5,  color = "dark gray"))+
  geom_segment(aes(xend = dist4,yend = height), color = "dark gray")+
  facet_wrap(~seaotter)+
  themeo     #There will still be gaps right now as heights

library(reshape2)
exp_count <- dist_cauchy_plotting_df_scaledbins %>% group_by(seaotter) %>% tidyr::expand(dist4=0:40) %>% ungroup()

# do the join
allcountstest <- left_join(exp_count, dist_cauchy_plotting_df_scaledbins, by = c("seaotter", "dist4")) %>% arrange(seaotter, dist4)


# fill in missing values per island
allcountstest_nona <- allcountstest %>% 
  group_by(seaotter) %>% 
  tidyr::fill(height, .direction = "up") %>% 
  replace_na(list(n = 0, freq = 0, height =0))

write.csv(allcountstest_nona, "data_output/allcountstest_nona.csv")

# plot to see if it looks ok logged
scaled_log <- ggplot(allcountstest_nona)+
   geom_segment(aes(xend = log(dist4), x = log(dist4),y = min(round(height, digits = 4)), yend = height), 
                size = 4,color = "dark gray")+
  geom_line(data= dist_cauchy_plotting_df, aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.7), xlim = c(0,3.5))+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("log dispersal distance (km2)")+
  ylab("")
  themeo #ok, they look good still


# plot to see if it looks ok 
scaled_raw <- ggplot(allcountstest_nona)+
  geom_segment(aes(xend = dist4, x = dist4,y = min(round(height, digits = 4)), yend = height), 
                size = 2.5,color = "dark gray")+
  geom_line(data= dist_cauchy_plotting_df, aes(x = dist4, y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,1), xlim = c(0,30))+
  facet_wrap(~seaotter,ncol = 6)+
  themeo #ok, they look good still

#compare
library(gridExtra)

grid.arrange(hist_raw, hist_log, scaled_raw, scaled_log,
             layout_matrix = rbind(c(1,2),
                                   c(3,4)))


#ok, lets go with scaled logged version
#now try to color by scale 

scale <- release_metrics %>% dplyr::select(seaotter, scale)
allcountstest_nona <- left_join(allcountstest_nona, scale, by = "seaotter")

######## fig1b
ggplot(allcountstest_nona)+
  geom_segment(aes(xend = log(dist4), x = log(dist4),y = min(round(height, digits = 4)), yend = height, color = scale), 
               size = 4)+
  geom_line(data= dist_cauchy_plotting_df, aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.7), xlim = c(0,3.5))+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("log dispersal distance (km2)")+
scale_color_gradient( low = "#90abc9", high = "#eda621")+
 themeo #ok, they look good still


###########################################################################################################
##### other models
#####try for one and go from there

#test_weibull <- fitdistr(otter_resight_dist$dist4[17][[1]],"weibull")
test_rayleigh <- fitdistr(resight_models_est_par$dist4[[42]], drayleigh,start = list(scale = 1))
test_rayleigh <- fitdistr(resight_models_est_par$dist4[[42]], "gamma")
test_gamma <- fitdistr(otter_resight_dist$dist4[1][[1]], dgamma, start = list(shape = 1, rate = 1))

#not working, try with old code instead

lcp_dist <- resight_models_est_par %>% dplyr::select(seaotter, dist4)
lcp_dist <- unnest(lcp_dist)

#individual otter df
otter_238 <- lcp_dist %>% subset(seaotter == "238")
otter_238<- as.data.frame(otter_238)

#rayleigh
#fit model
fit_rayleigh_238 <- fitdistr(otter_238$dist4, drayleigh,start = list(scale = 1))
str(fit_rayleigh_238)

#gamma
#fit model
fit_gamma_238 <- fitdistr(otter_238$dist4,dgamma, start = list(shape = 5000, rate = 5))
str(fit_gamma_238)


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



### temporal plot
ggplot(dispersal_drivers)+
  geom_point(aes(x = rel_date_real, y = scale))+
  geom_smooth(aes(x = rel_date_real, y = scale))+
  #geom_text()+
  themeo
