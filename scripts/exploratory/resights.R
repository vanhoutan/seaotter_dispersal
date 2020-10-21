#add packages
library(here)
library(tidyverse)

library(sf)
library(rgdal)
library(raster)

library(MASS)
library(VGAM)
library(fitdistrplus)

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


#raw data
resights_daysout <- read.csv(here("data/release_resights.csv"), stringsAsFactors = FALSE)

resights_date <- read.csv(here("data/post_release_resights.csv"), stringsAsFactors = FALSE)

#ID to character, not number
resights_daysout$seaotter <- as.character(resights_daysout$seaotter)
resights_date$seaotter <- as.character(resights_date$seaotter)

#combine all columns
resights <- cbind(resights_date, resights_daysout$days_since_rel)
resights <- resights %>% dplyr::select(-notes, -X, -X.1, -X.2, -X.3, -X.4)
View(resights)

#fix date
resights$datetime <- as.POSIXct(paste(resights$date, resights$time), format = "%m/%d/%Y %H:%M")


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
plot(bathy, interpolate = TRUE)

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


###### calculate time and distance between resights
resights <- resights %>%
  arrange(seaotter, datetime) %>% 
  group_by(seaotter, release) %>%
  mutate(diff = datetime - lag(datetime),
         diff_hours = as.numeric(diff, units = 'hours')) 

#calculate distance between resight locations for each otter
#now try for each otter, each release


#set up loop
ind_otter <- levels(as.factor(resights$seaotter))
resights_dist_recap <- NULL

#run loop
for(i in 1:length(ind_otter)){
  
  #subset otters
  sub_otters <- subset(resights, seaotter == ind_otter[i] )
  
  recap<- levels(as.factor(sub_otters$release))
  
  for(j in 1:length(recap)){
    
    sub_recap <- subset(sub_otters, release == recap[j])
    
    #sf
    sub_otters_sf <- sf::st_as_sf(sub_recap, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                                  +datum=WGS84 +no_defs')
    
    #calculate distance between resights
    #distance <- st_distance(sub_otters_sf[-nrow(sub_otters_sf),],sub_otters_sf[-1,],by_element=TRUE) 
    distance <- st_distance(sub_otters_sf[-1,],sub_otters_sf[-nrow(sub_otters_sf),],by_element=TRUE) 
    # returns <- append(distance, median(distance), after = length(distance))
    returns <- append(distance, NA, after = 0)
    
    #calculate time between resights
    
    #save distance as a column in sub_otter
    sub_recap$dist <- returns
    
    
    #bind
    resights_dist_recap <- rbind(resights_dist_recap, sub_recap)
    
    
  }
}

#View(resights_dist_recap)

# rate of travel
resights_dist_recap$rate <- resights_dist_recap$dist/ resights_dist_recap$diff_hours # meters per hour

resights_dist_recap$km_hr <- resights_dist_recap$rate/1000 #km per hour
hist(resights_dist_recap$km_hr)


colnames(resights_dist_recap)[colnames(resights_dist_recap)=="resights_daysout$days_since_rel"] <- "days_since_rel"


ggplot(resights_dist_recap)+
  geom_histogram(aes(x=days_since_rel), binwidth = 10)+
  facet_wrap(~seaotter)+
  themeo


######Fit models for one otter as a test
#create inidividual otter df for model fitting

#individual otter df
otter_209 <- resights_dist_recap %>% subset(seaotter == "209")
otter_209 <- as.data.frame(otter_209)
otter_209_noNA <- na.omit(otter_209)

#rayleigh
#fit model
fit_rayleigh <- fitdistr(otter_209_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh)

#gamma
#fit model
fit_gamma <- fitdistr(otter_209_noNA$dist,dgamma, start = list(shape = 5000, rate = 5))
str(fit_gamma)

#weibull
#fit model
fit_weibull <- fitdistr(otter_209_noNA$dist,"weibull")
str(fit_weibull)

#cauchy
#fit model
fit_cauchy <- fitdistr(otter_209_noNA$dist,"cauchy")
str(fit_cauchy)

#estimates from models
otter_209_noNA$rayleigh <- drayleigh(otter_209_noNA$dist, scale = fit_rayleigh$estimate, log = FALSE)

otter_209_noNA$gamma <- dgamma(otter_209_noNA$dist, shape  = 14.45597, rate = 0.00268, log = FALSE)

otter_209_noNA$weibull <- dweibull(otter_209_noNA$dist, shape = 0.824, scale = 5080.39, log = FALSE)

otter_209_noNA$cauchy <- dcauchy(otter_209_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_209_noNA$cauchy2 <- dcauchy(otter_209_noNA$dist, location = fit_cauchy$estimate[1], scale = fit_cauchy$estimate[2], log = FALSE)

#plot over histogram
ggplot(otter_209_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  

# create no_NA df of all otters for looped model fitting
resights_dist_recap_noNA <- na.omit(resights_dist_recap)

# write.csv(resights_dist_recap, "data_output/resights_dist_recap.csv")
# write.csv(resights_dist_recap_noNA, "data_output/resights_dist_recap_nona.csv")
