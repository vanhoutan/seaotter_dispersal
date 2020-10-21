##### Least cost path between resignts based on elevation and bathymetry


#read in packages
library(here)
library(tidyverse)

library(sf)
library(rgdal)
library(raster)

library(forcats)
library(lemon)
library(lubridate)

# add plotting theme
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

#bring in release data
releases <- read.csv(here::here("data/otter_releases.csv"), stringsAsFactors = FALSE)


# bring in resights data
#raw data
resights_daysout <- read.csv(here::here("data/release_resights.csv"), stringsAsFactors = FALSE)

resights_date <- read.csv(here::here("data/post_release_resights.csv"), stringsAsFactors = FALSE)

#ID to character, not number
resights_daysout$seaotter <- as.character(resights_daysout$seaotter)
resights_date$seaotter <- as.character(resights_date$seaotter)

#combine all columns
resights <- cbind(resights_date, resights_daysout$days_since_rel)
resights <- resights %>% dplyr::select(-notes, -X, -X.1, -X.2, -X.3, -X.4)

#fix date
resights$datetime <- as.POSIXct(paste(resights$date, resights$time), format = "%m/%d/%Y %H:%M")
str(resights)



##### resight maps

# make a spatial sf for mapping
resights_sf <- sf::st_as_sf(resights, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                            +datum=WGS84 +no_defs')

# make a simpler dataframe, then sf for mapping
resights_short <- resights %>%  dplyr::select(seaotter, latitude, longitude)
resights_short_sf  <- sf::st_as_sf(resights_short, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                                   +datum=WGS84 +no_defs')

#plot
plot(resights_short_sf) #simple plot to test

#add some bells and whistles
#basemap
cencal <- readOGR(dsn = "C:/Users/SBecker/Local-Git/otter_dispersal/data/GIS/coastline", layer = "cencal_baseline")
#https://pubs.usgs.gov/of/2007/1112/

#bathy contours
bathy <- raster("C:/Users/SBecker/Local-Git/otter_dispersal/data/GIS//bathy/GEBCO2014_-123.9342_35.1272_-120.655_38.482_30Sec_Geotiff.tif")
#plot(bathy, interpolate = TRUE)

bathy_spdf <- as(bathy, "SpatialPixelsDataFrame")
bathy_df <- as.data.frame(bathy_spdf)

#release location
release_site <- subset(resights_sf, resights_daysout$days_since_rel== 0) #make variable of only release locations

#faceted by sea otter + shoreline/contours/release locations + optional add in pathways between resights
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = resights_short_sf, aes(color = seaotter))+ #resight locations
  geom_path(data = resights, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(resights_short_sf))+ #study site
  facet_wrap(~seaotter) +
  themeo 

#create 809 subset

#809
otter809 <- subset(resights, seaotter == "809")
otter809_short <- subset(resights_short, seaotter == "809")
otter_map_809 <- subset(resights_sf, seaotter == "809")
otter809_short_sf <- subset(resights_short_sf, seaotter == "809")

#map
map_809<- ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 5,0, -30, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  #geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otter_map_809, color = "blue")+ #resight locations
  geom_path(data = otter809, aes(x = longitude, y = latitude), color = "blue", size = 1)+ #resignt pathways
 #geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otter_map_809))+ #study site
  themeo


#least cost path for 809's resight locations
library(marmap)
library(gdistance)
library(raster)



#pull bathy from noaa# bay_bathy <- getNOAA.bathy(lon1 = -123, lon2 = -121.5,
#                         lat1 = 36, lat2 = 37.5, resolution = 1)
# plot(bay_bathy) # not very resolved

plot(bathy) # higher res, but in wrong format for marmap

#convert higher res raster to bathy format for marmap
bathy_bathy <- as.bathy(bathy)
plot(bathy_bathy)
trans1 <- trans.mat(bathy_bathy, min.depth = 5, max.depth = -50)
trans2 <- trans.mat(bathy_bathy, min.depth = 0, max.depth = -30)

otter809_short <- otter809_short %>%  dplyr::select(latitude ,longitude)
otter809_short <- otter809_short[,c(2,1)]

out1 <- lc.dist(trans1, otter809_short, res = "path")
out2 <- lc.dist(trans2, otter809_short, res = "path")

plot(bathy_bathy, xlim = c(-122.4, -121.7), ylim = c(36.5, 37.2),
       deep = c(-5000, -50 , 0), shallow = c(-50, 0, 0),
       col = c("grey", "grey", "black"), step = c(1000, 50, 1),
       lty = c(1, 1, 1), lwd = c(0.6, 0.6, 1.2),
       draw=c(FALSE, FALSE, FALSE))
points(otter809_short, pch = 21, col = "black", bg = col2alpha("black", .9),
         cex = 1.2)
#lapply(out1, lines, col = "orange", lwd = 2, lty = 1) -> dummy
lapply(out2, lines, col = "red", lwd = 1, lty = 1) -> dummy

dist1 <- lc.dist(trans1, otter809_short, res = "dist")

dist2 <- lc.dist(trans2, otter809_short, res = "dist")
#lets switch this to ggplot eventually


#do for just one resight segment
otter809_short1 <- otter809_short[c(14:15),]

out1_1 <- lc.dist(trans1, otter809_short1, res = "path")
out2_1 <- lc.dist(trans2, otter809_short1, res = "path")

plot(bathy_bathy, xlim = c(-122.4, -121.7), ylim = c(36.5, 37.2),
     deep = c(-5000, -50 , 0), shallow = c(-50, 0, 0),
     col = c("grey", "grey", "black"), step = c(1000, 50, 1),
     lty = c(1, 1, 1), lwd = c(0.6, 0.6, 1.2),
     draw=c(FALSE, FALSE, FALSE))
points(otter809_short, pch = 21, col = "black", bg = col2alpha("black", .9),
       cex = 1.2)
#lapply(out1, lines, col = "orange", lwd = 2, lty = 1) -> dummy
lapply(out2_1, lines, col = "red", lwd = 1, lty = 1) -> dummy

dist1_1 <- lc.dist(trans1, otter809_short1, res = "dist")
dist2_1 <- lc.dist(trans2, otter809_short1, res = "dist")


# try to map over all otters

test <- resights %>% group_by(seaotter) %>% nest()


trans2 <- trans.mat(bathy_bathy, min.depth = 0, max.depth = -30)
trans3 <- trans.mat(bathy_bathy, min.depth = 5)

dist2_1 <- lc.dist(trans2, otter809_short1, res = "dist")

# test$data[,c("longitude", "latitude")] # testing how to define lat and long columns
# test$data[1] # testing how to look at one otter from test df

testmap_dist <- test %>% mutate(distance = map(data, function(x) {lc.dist(trans2, x[, c("longitude", "latitude")], res = "dist")} )) 
testmap_dist2 <- test %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} )) 

testmap_path2 <- test %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "path")} ))

tail(test)
tail(testmap_path2)

testmap_dist2$distance[29]
test$data[29]

testmap_path2$distance[42][[1]]

View(testmap_dist2)

#attempt to extract diagonals with Jerry's code

test_dist_diag <-  test %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} ),
                    diag = map(distance, function(x){
                     print(x) [# print values
                        c(1,1 + cumsum(seq((attr(x, "Size")-1), 1))) #along the diag index
                        ]
                    }))


test_dist_diag$diag[1]
testmap_dist2$distance[1]
testmap_path2$data[1]

class(testmap_path2$distance[1])




#test plotting

#otters_sf_dist <- sf::st_as_sf(full_otters_dist, coords = c("longitude", "latitude"), # or use es_otters_dist
#crs =' +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# testmap_path2_plot <- testmap_path2 %>% 
#   group_by(seaotter) %>% 
#   nest() %>% 
#   mutate(gg1 = map2(distance, seaotter, ~ggplot(data =.x)+
#                  geom_point(aes(y=y, x=x)) ))
# 
# 
# testmap_path2$data[1]
# testmap_path2_plot$gg1[1]
# 
# print(testmap_path2_plot$gg1) 

# convert one list to a df and plot from there
# will want to automate this later if possible to look at all paths, but first figure out a way to get it plotting at all

test1_df <- data.frame(matrix(unlist(test$data[1]), nrow=19, byrow=F))
names(test1_df) <- c("release", "date", "time", "area",  "fix_type",
                     "fix_qual", "latitude", "longitude", "days_since_rel", "datetime"   )
test1_ll <- test1_df %>% dplyr::select("longitude", "latitude")

test42_df <- data.frame(matrix(unlist(test$data[42]), nrow=17, byrow=F), stringsAsFactors = FALSE)
names(test42_df) <- c("release", "date", "time", "area",  "fix_type",
                     "fix_qual", "latitude", "longitude", "days_since_rel", "datetime"   )
test42_ll <- test42_df %>% dplyr::select("longitude", "latitude")

test42_path <- data.frame(matrix(unlist(testmap_path2$distance[42][[1]][[133]]), byrow = F))
long42 <- test42_path[c(1:37),]
lat42 <- test42_path[c(38:74),]

path_seg_42 <- cbind(long42, lat42)
names(path_seg_42) <- c("longitude", "latitude")

#test mapping??
plot(bathy_bathy, xlim = c(-122.4, -121.7), ylim = c(36.5, 37.2),
     deep = c(-5000, -50 , 0), shallow = c(-50, 0, 0),
     col = c("grey", "grey", "black"), step = c(1000, 50, 1),
     lty = c(1, 1, 1), lwd = c(0.6, 0.6, 1.2),
     draw=c(FALSE, FALSE, FALSE))
points(test42_ll, pch = 21, col = "black", bg = col2alpha("black", .9),
       cex = 1.2)
lines(path_seg_42, pch = 10, col = "red", bg = col2alpha("red", .9),
      cex = 1.2)



plot(bathy_bathy, xlim = c(-122.4, -121.7), ylim = c(36.5, 37.2),
     deep = c(-5000, -50 , 0), shallow = c(-50, 0, 0),
     col = c("grey", "grey", "black"), step = c(1000, 50, 1),
     lty = c(1, 1, 1), lwd = c(0.6, 0.6, 1.2),
     draw=c(FALSE, FALSE, FALSE))
#points(testmap_path2$distance[], pch = 21, col = "black", bg = col2alpha("black", .9),
      # cex = 1.2)
lines(testmap_path2$distance[1][[1]][[1]], pch = 10, col = "red", bg = col2alpha("red", .9),
      cex = 1.2)

# convert all paths to points to broadly look for wayward paths
otter11 <- data.frame(matrix(unlist(testmap_path2$distance), byrow = F))

otter_latlon <- NULL
lat <- data.frame(otter11[otter11>30])
lon <- data.frame(otter11[otter11<30])

otter_latlon <- cbind(lon, lat)
head(otter_latlon)

plot(bathy_bathy, xlim = c(-122.4, -121.7), ylim = c(36.5, 37.2),
     deep = c(-5000, -50 , 0), shallow = c(-50, 0, 0),
     col = c("grey", "grey", "black"), step = c(1000, 50, 1),
     lty = c(1, 1, 1), lwd = c(0.6, 0.6, 1.2),
     draw=c(FALSE, FALSE, FALSE))
#points(testmap_path2$distance[], pch = 21, col = "black", bg = col2alpha("black", .9),
# cex = 1.2)
points(otter_latlon, pch = 1, col = "red", bg = col2alpha("red", .9),
      cex = 1.2)


paths_unnest <- testmap_path2 %>% unnest(distance)
paths_unnest$distance[2]
tail(paths_unnest)

paths_unnest$id <- 1:nrow(paths_unnest)
paths_unnest2 <- paths_unnest %>% unnest(distance, .drop = TRUE)
 # looks like all paths make some sense

####################################### 
#######################################
######### rerun everything incorporating recaps


# try to map over all otters

test_recap <- resights %>% group_by(seaotter, release) %>% nest()


# trans2 <- trans.mat(bathy_bathy, min.depth = 0, max.depth = -30)
# trans3 <- trans.mat(bathy_bathy, min.depth = 5)
# 
# dist2_1 <- lc.dist(trans2, otter809_short1, res = "dist")

# test$data[,c("longitude", "latitude")] # testing how to define lat and long columns
# test$data[1] # testing how to look at one otter from test df

#testmap_dist_recap <- test_recap %>% mutate(distance = map(data, function(x) {lc.dist(trans2, x[, c("longitude", "latitude")], res = "dist")} )) 
testmap_dist2_recap <- test_recap %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} )) 

testmap_path2_recap <- test_recap %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "path")} ))

tail(test_recap)
tail(testmap_dist2_recap)

testmap_dist2_recap$distance[29]
test_recap$data[29]

testmap_path2$distance[42][[1]]

View(testmap_dist2_recap)
saveRDS(testmap_path2_recap, "data_output/testmap_path2_recap.rds")
testmap_path2_recap <- readRDS( "data_output/testmap_path2_recap.rds")

saveRDS(testmap_dist2_recap, "data_output/testmap_dist2_recap.rds")
testmap_dist2_recap <- readRDS( "data_output/testmap_dist2_recap.rds")

#attempt to extract diagonals with Jerry's code

test_dist_diag_recap <-  test_recap %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} ),
                                   diag = map(distance, function(x){
                                     print(x) [# print values
                                       c(1,1 + cumsum(seq((attr(x, "Size")-1), 1))) #along the diag index
                                       ]
                                   }))
saveRDS(test_dist_diag_recap, "data_output/test_dist_diag_recap.rds")
test_dist_diag_recap <- readRDS("data_output/test_dist_diag_recap.rds")

# test_dist_diag_recap$diag[4]
# testmap_dist2_recap$distance[4]
# testmap_path2_recap$data[1]
# 
# class(testmap_path2$distance[1])

just_diag <- test_dist_diag_recap %>% dplyr::select(seaotter, release, diag)

# test_dist_diag_recap_otter <- just_diag %>% group_by(seaotter) %>% nest() 
# unnested_diag <- test_dist_diag_recap_otter %>% unnest()

## attempts to flatten so that there is just one row per otter
##### this is what we need!!!
dist_per_otter <- just_diag %>% group_by(seaotter) %>% nest() %>% 
       mutate(dist = map(data, ~flatten(.x)), dist2 = map(dist, ~as.vector(unlist(.x))), dist3 = map(dist2, ~.x[-1])) 
#flattens, unlists, and then removes first row (first row was column name and didnt make sense)
#dist3 (final useful output) is a list of resight distance values for each otter. All releases are included, but distances are not calculated between different releases

# this next step takes the distance calculations and removes NAs for model fitting (Na would be between end of release 1 and start of release 2, for example)
otter_resight_dist <- dist_per_otter %>% dplyr::select(seaotter, dist3) %>% mutate(dist4 = map(dist3, ~.x[!is.na(.x)])) 

saveRDS(otter_resight_dist, "data_output/otter_resight_dist.rds")
otter_resight_dist <- readRDS("data_output/otter_resight_dist.rds")

## fit models?
library(MASS)
library(fitdistrplus)
library(VGAM)

# fitdistr(sub_otter$dist,"cauchy", lower= c(0.01, 0.01))
# fitdistr(otter_473_noNA$dist,"weibull")
# fitdistr(otter_475_noNA$dist, drayleigh,start = list(scale = 1))
# fitdistr(otter_475_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)

##### cauchy
#try for one and go from there
test_cauchy <- fitdistr(otter_resight_dist$dist4[[6]],"cauchy", lower= c(0.01, 0.01))

otters_n249 <- otter_resight_dist[-6,]

#mapped
resight_models <- otters_n249 %>%  mutate(cauchy = map(dist4, ~ fitdistr(.x,"cauchy", lower= c(0.01, 0.01)) ))

resight_models$cauchy[41]

# add estimates and parameters
resight_models_est_par <- resight_models %>%  
                           mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
                           scale = map(cauchy, ~.x[1][[1]][[2]]),
                           est_cauchy = map2(dist4, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))

# fit 249 separately
otter249 <- otter_resight_dist[6,]

resight_models_249 <- otter249 %>%  mutate(cauchy = map(dist4, ~ fitdistr(.x,"cauchy", lower= c(0.1, 0.1)) ))

resight_models_est_par_249 <- resight_models_249 %>%  
  mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
         scale = map(cauchy, ~.x[1][[1]][[2]]),
         est_cauchy = map2(dist4, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))

#add 249 in with rest of cauchy models
resight_models_est_par <- rbind(resight_models_est_par, resight_models_est_par_249)

resight_models_est_par$est_cauchy[3]

saveRDS(resight_models_est_par, "data_output/resight_models_est_par.rds")
resight_models_est_par <- readRDS("data_output/resight_models_est_par.rds")

#make plots of each dist histogram overlaid with estimates from cauchy model
dist_cauchy_plotting_df <- resight_models_est_par %>% dplyr::select(seaotter, dist4, est_cauchy)

dist_cauchy_plotting_df<- dist_cauchy_plotting_df %>% unnest()
library(lemon)

#plot lcp histograms and cauchy model fits
ggplot(dist_cauchy_plotting_df)+
  geom_histogram(aes(x = dist4, y = ..density..), binwidth = 3, fill = "grey", color = "grey")+
  geom_line(aes(x = dist4, y = est_cauchy),stat = "identity", color = "black")+
  facet_rep_wrap(facets = "seaotter", scales = "free_y", ncol = 6, repeat.tick.labels = FALSE)+
  coord_cartesian(xlim = NULL, ylim = c(0,0.35), expand = c(0,0))+
  xlab("least cost path distance (km)")+
  themeo


##### other models
#try for one and go from there
#test_weibull <- fitdistr(otter_resight_dist$dist4[17][[1]],"weibull")
test_rayleigh <- fitdistr(resight_models_est_par$dist4[[2]], drayleigh,start = list(scale = resight_models_est_par$scale[[2]]))
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

# still nothing is working, take a break from this for now and move on to other things

# map of three otter paths, for figure 1

#faceted by sea otter + shoreline/contours/release locations + optional add in pathways between resights
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
 geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  # geom_sf(data = otters_short_dist_sf, aes(color = seaotter))+ #resight locations
  # geom_path(data = full_otters_dist, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(bathy_df))+ #study site
  #facet_wrap(~seaotter) +
  themeo 


# attempt to use diagonal indexing to identify which #s of paths we need to keep to just look at the resight distances 
test_dist_diag_recap_index <-  test_recap %>% mutate(diag_index = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} ),
                                               diag = map(diag_index, function(x){
                                      
                                                   c(1,1 + cumsum(seq((attr(x, "Size")-1), 1))) #along the diag index
                                                   
                                               }))
saveRDS(test_dist_diag_recap_index, "data_output/rtest_dist_diag_recap_index.rds")
test_dist_diag_recap_index <- readRDS("data_output/rtest_dist_diag_recap_index.rds")

test_dist_diag_recap_index$diag_index[1] #the diagonal shown here
test_dist_diag_recap_index$diag[1] # is this list of index #s, minus the last value
test_dist_diag_recap$diag[1] # this shows the equivalent in diag values (last = na)
testmap_path2_recap # this shows that each otter has #paths = # test_dist_diag_recap_index$diag[1] - last value
# great, so how to index for what I want????

#drop last unneeded value form index list
test_dist_diag_recap_index <- test_dist_diag_recap_index %>% mutate(diag2 = map(diag, ~.x[-length(.x)])) 
test_dist_diag_recap_index$diag2[1]# test if it worked (it did)

# make a smaller tibble with just what is needed to prepare to join into a tibble with all data needed to index
diag_paths <- test_dist_diag_recap_index %>% dplyr::select(seaotter, release, diag2) #make smaller tibble
lcp_paths <- left_join(diag_paths, testmap_path2_recap, by = c("seaotter","release")) # join

lcp_paths <- lcp_paths %>% mutate(lcp_paths = map2(distance, diag2, ~.x[.y])) # index all paths by just paths on the diagonal 

lcp_paths$distance[[1]][[171]] # compare
lcp_paths$lcp_paths[[1]][[18]] # I think it worked?

lcp_df_unnest <- lcp_paths %>% dplyr::select(seaotter, lcp_paths) %>% unnest() 
lcp_df_unnest$lcp_paths[1]

lcp_df_unnest <- lcp_df_unnest %>% 
  mutate(long = map(lcp_paths, ~.x[c(1:((length(.x))/2))]),
         lat = map(lcp_paths, ~.x[- c(1:((length(.x))/2))]))

lcp_df_unnest$long[1]
lcp_df_unnest$lat[1]

lcp_df_unnest <- lcp_df_unnest %>% dplyr::select(seaotter, long, lat)
lcp_df_unnest <- lcp_df_unnest %>% unnest()

write.csv(lcp_df_unnest, "data_output/lcp_df_unnest.csv")

# plot least cost paths, faceted for all otters
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
 # geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_path(data = lcp_df_unnest, aes(x=long, y = lat), color = "black")+
  # geom_sf(data = otters_short_dist_sf, aes(color = seaotter))+ #resight locations
  # geom_path(data = full_otters_dist, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  geom_sf(data = release_site, color = "black", size = 0.5) + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site))+ #study site
  facet_wrap(~seaotter, ncol = 6) +
  themeo 


# plot least cost paths, 3 example otters on one map

otter228 <- subset(lcp_df_unnest, seaotter == "228")
otter315 <- subset(lcp_df_unnest, seaotter == "315")
otter379 <- subset(lcp_df_unnest, seaotter == "379")
otter587 <- subset(lcp_df_unnest, seaotter == "587")
otter716 <- subset(lcp_df_unnest, seaotter == "716")


release_site_short <- subset(release_site, seaotter == c("228","379","587","716"))
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
#  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_path(data = otter228, aes(x=long, y = lat), color = "#1b9e77")+
  geom_path(data = otter379, aes(x=long, y = lat), color = "#d95f02")+
  geom_path(data = otter716, aes(x=long, y = lat), color = "#7570b3")+
  geom_path(data = otter587, aes(x=long, y = lat), color = "#e7298a")+
  # geom_sf(data = otters_short_dist_sf, aes(color = seaotter))+ #resight locations
  # geom_path(data = full_otters_dist, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  geom_sf(data = release_site_short, color = "black") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site_short))+ #study site
  themeo 
