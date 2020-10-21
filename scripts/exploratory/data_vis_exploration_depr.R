#read in packages
library(here)
library(tidyverse)

library(sf)
library(rgdal)
library(raster)

library(forcats)
library(lemon)

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

#read in and clean up data
otter_modelfit_rate <- read.csv(here("data_output/otter_model_fit_rate.csv"), stringsAsFactors = FALSE)
otter_modelfit_dist <- read.csv(here("data_output/otter_model_fit_dist.csv"), stringsAsFactors = FALSE)
releases <- read.csv(here("data/otter_releases.csv"), stringsAsFactors = FALSE)
es_pop_dens <- read.csv(here("data/censusES1985_2017.csv"), stringsAsFactors = FALSE)
cauchy <- read.csv("data_output/cauchy_scale.csv")
otter_cauchy_fits <- read.csv("data_output/otter_cauchy_fits.csv", stringsAsFactors = FALSE) #dist
  
  
str(otter_cauchy_fits)
str(otter_modelfit_rate)
str(releases)
str(es_pop_dens)

# bring in demographic data and start looking at dispersal histograms by sex, age, age at stranding, month, etc
colnames(releases)[colnames(releases)=="ottername"] <- "seaotter"


releases$seaotter <- as.character(releases$seaotter)
otter_modelfit_rate$seaotter <- as.character(otter_modelfit_rate$seaotter)
otter_modelfit_dist$seaotter <- as.character(otter_modelfit_dist$seaotter)


#RATE
full_otters_rate <- left_join(otter_modelfit_rate, releases, by = "seaotter")
str(full_otters_rate)

#fix date
full_otters_rate$datetime <- as.POSIXct(paste(full_otters_rate$date, full_otters_rate$time), format = "%m/%d/%Y %H:%M")
head(full_otters_rate)

#look only at elkhorn slough otters
es_otters <- full_otters_rate %>% dplyr::filter(relsiteATOS == "321")

#median/mean dispersal rate per year for elkhorn slough otters
es_disp_med <- es_otters %>% group_by(relyear) %>% summarize(med_rate = median(km_hr))
es_disp_mean <- es_otters %>% group_by(relyear) %>% summarize(mean_rate = mean(km_hr))

#elkhorn slough population density for only survey years
density_2002_2017 <- es_pop_dens %>% dplyr::filter(year >= 2002)

#exploratory plots
plot(density_2002_2017$dens.sm, es_disp_med$med_rate)
plot(density_2002_2017$dens.sm, es_disp_mean$mean_rate)

plot(density_2002_2017$year, density_2002_2017$dens.sm)

plot(es_disp_med$relyear,es_disp_med$med_rate)
plot(es_disp_mean$relyear,es_disp_mean$mean_rate)



library(forcats)
library(lemon)

full_otters_rate$seaotter <- as.factor(full_otters_rate$seaotter)
str(full_otters_rate)

#model fits
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.. ), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  #geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  facet_wrap(~seaotter, scales = "free")+
  #facet_rep_wrap(~(fct_reorder(full_otters_rate$seaotter,  
  #    full_otters_rate$sex,
  #   max,
  #  .desc = TRUE)),scales = "free", repeat.tick.labels = "FALSE")+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 


#map
# make a spatial sf for mapping
otters_sf <- sf::st_as_sf(full_otters_rate, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                            +datum=WGS84 +no_defs')

# make a simpler dataframe, then sf for mapping
otters_short <- full_otters_rate %>%  dplyr::select(seaotter, latitude, longitude)
otters_short_sf  <- sf::st_as_sf(otters_short, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                                   +datum=WGS84 +no_defs')

#plot
plot(otters_short_sf) #simple plot to test

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
release_site <- subset(otters_sf, days_since_rel== 0) #make variable of only release locations

#faceted by sea otter + shoreline/contours/release locations + optional add in pathways between resights
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otters_short_sf, aes(color = seaotter))+ #resight locations
  geom_path(data = full_otters_rate, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
 # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_sf))+ #study site
  facet_wrap(~seaotter) #+
  #themeo 

#select 3 example otters and create figure of maps/ model fits

#809
otter_fit_809 <- subset(full_otters_rate, seaotter == "809")

otter_map_809 <- subset(otters_short_sf, seaotter == "809")

#map
map_809<- ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otter_map_809, color = "blue")+ #resight locations
  geom_path(data = otter_fit_809, aes(x = longitude, y = latitude), color = "blue", size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_sf))+ #study site
 themeo 

#model fits
fit_809<- ggplot(otter_fit_809)+
  geom_histogram(aes(x=km_hr, y = ..density.. ), binwidth = .15)+
  #geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "red")+
  # geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  # geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  coord_cartesian(xlim = c(0,3), ylim = c(0,3))+
  themeo 

#685
otter_fit_685 <- subset(full_otters_rate, seaotter == "685")

otter_map_685 <- subset(otters_short_sf, seaotter == "685")

#map
map_685<- ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otter_map_685, color = "blue")+ #resight locations
  geom_path(data = otter_fit_685, aes(x = longitude, y = latitude), color = "blue", size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_sf))+ #study site
  themeo 

#model fits
fit_685 <- ggplot(otter_fit_685)+
  geom_histogram(aes(x=km_hr, y = ..density.. ), binwidth = .15)+
  #geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "red")+
  # geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  # geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  coord_cartesian(xlim = c(0,3), ylim = c(0,3))+
  themeo 


#475
otter_fit_475 <- subset(full_otters_rate, seaotter == "475")

otter_map_475 <- subset(otters_short_sf, seaotter == "475")

#map
map_475<- ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otter_map_475, color = "blue")+ #resight locations
  geom_path(data = otter_fit_475, aes(x = longitude, y = latitude), color = "blue", size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_sf))+ #study site
  themeo 

#model fits
fit_475<- ggplot(otter_fit_475)+
  geom_histogram(aes(x=km_hr, y = ..density.. ), binwidth = .15)+
  #geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "red")+
  # geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  # geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  coord_cartesian(xlim = c(0,3), ylim = c(0,7.5))+
  themeo 

library(gridExtra)
Fig1 <- grid.arrange(map_809, fit_809, map_685, fit_685, map_475, fit_475,
                       layout_matrix = rbind(c(1,1,1,1,2),
                                             c(1,1,1,1,2),
                                             c(1,1,1,1,2),
                                             c(1,1,1,1,2),
                                             c(3,3,3,3,4),
                                             c(3,3,3,3,4),
                                             c(3,3,3,3,4),
                                             c(3,3,3,3,4),
                                             c(5,5,5,5,6),
                                             c(5,5,5,5,6),
                                             c(5,5,5,5,6),
                                             c(5,5,5,5,6)))





#relnum
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relnum, color = relnum), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_rate$seaotter,
                               full_otters_rate$relnum,
                               max,
                               .desc = TRUE)),scales = "free", repeat.tick.labels = "FALSE")+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#relyear
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relyear, color = relyear), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_rate$seaotter,
                               full_otters_rate$relyear,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#relmonth
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relmonth, color = relmonth), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_rate$seaotter,
                               full_otters_rate$relmonth,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 





# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#relsiteATOS
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relsiteATOS, color = relsiteATOS), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  # facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_rate$seaotter,
                               full_otters_rate$relsiteATOS,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#strandage_d
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = strandage_d, color = strandage_d), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_rate$seaotter,
                               full_otters_rate$strandage_d,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#strandsiteATOS
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = strandsiteATOS, color = strandsiteATOS), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_rate$seaotter,
                               full_otters_rate$strandsiteATOS,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 




View(full_otters_rate)

#histograms by demographics

#sex
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = sex, color = sex), binwidth = .1)+
  facet_wrap(~sex)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

#relnum 
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relnum, color = relnum), binwidth = .1)+
  facet_wrap(~relnum)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relyear 
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relyear, color = relyear), binwidth = .1)+
  facet_wrap(~relyear)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relmonth 
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relmonth, color = relmonth), binwidth = .1)+
  facet_wrap(~relmonth)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relsiteATOS 
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relsiteATOS, color = relsiteATOS), binwidth = .1)+
  facet_wrap(~relsiteATOS)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relage_d 
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = relage_d, color = relage_d), binwidth = .1)+
  facet_wrap(~relage_d)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# strandage_d 
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = strandage_d, color = strandage_d), binwidth = .1)+
  facet_wrap(~strandage_d)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# strandsiteATOS
ggplot(full_otters_rate)+
  geom_histogram(aes(x=km_hr, y = ..density.., fill = strandsiteATOS, color = strandsiteATOS), binwidth = .1)+
  facet_wrap(~strandsiteATOS)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 


mean_annual_disp_rate <- full_otters_rate %>% 
  group_by(seaotter, relyear) %>% summarize(mean(km_hr))
median_annual_disp_rate <- full_otters_rate %>% 
  group_by(seaotter,relyear) %>% summarize(median(km_hr))


ggplot(median_annual_disp_rate, aes(x=relyear, y = log(`median(km_hr)`)))+
  geom_point()+
  geom_smooth()+
  themeo



#boxplots by demographics

#sex
ggplot(full_otters_rate)+
  geom_boxplot(aes(x=sex, y = log(km_hr)))+
  themeo 

#relnum 
ggplot(full_otters_rate)+
  geom_boxplot(aes(x=as.factor(relnum), y = log(km_hr)))+
  themeo 

# relyear 
ggplot(full_otters_rate)+
  # geom_boxplot(aes(x=as.factor(relyear), y = log(km_hr), fill = sex))+
  geom_boxplot(aes(x=as.factor(relyear), y = log(km_hr)))+
  themeo 

# relmonth 
ggplot(full_otters_rate)+
  geom_boxplot(aes(x=as.factor(relmonth), y = log(km_hr), fill = sex))+
  themeo 

# relsiteATOS 
ggplot(full_otters_rate)+
  geom_boxplot(aes(x=relsiteATOS, y = log(km_hr), group = cut_width(relsiteATOS , 1)))+
  themeo 

# relage_d 
ggplot(full_otters_rate)+
  geom_boxplot(aes(x=as.factor(relage_d), y = log(km_hr), group = cut_width(relage_d , 30)), width = 1)+
  themeo 

# strandage_d 
ggplot(full_otters_rate)+
  geom_boxplot(aes(x=as.factor(strandage_d), y = log(km_hr)))+
  themeo 

# strandsiteATOS
ggplot(full_otters_rate)+
  geom_boxplot(aes(x=as.factor(strandsiteATOS), y = log(km_hr)))+
  themeo 






#DIST
full_otters_dist <- left_join(otter_modelfit_dist, releases, by = "seaotter")
str(full_otters_dist)

#fix date
full_otters_dist$datetime <- as.POSIXct(paste(full_otters_dist$date, full_otters_dist$time), format = "%m/%d/%Y %H:%M")
head(full_otters_dist)

#look only at elkhorn slough otters
es_otters_dist <- full_otters_dist %>% dplyr::filter(relsiteATOS == "321")

#median/mean dispersal rate per year for elkhorn slough otters
es_disp_dist_med <- es_otters_dist %>% group_by(relyear) %>% summarize(med_rate = median(km_hr))
es_disp_dist_mean <- es_otters_dist %>% group_by(relyear) %>% summarize(mean_rate = mean(km_hr))

#elkhorn slough population density for only survey years
density_2002_2017 <- es_pop_dens %>% dplyr::filter(year >= 2002)

#exploratory plots
plot(density_2002_2017$dens.sm, es_disp_dist_med$med_rate)
plot(density_2002_2017$dens.sm, es_disp_dist_mean$mean_rate)

plot(density_2002_2017$year, density_2002_2017$dens.sm)

plot(es_disp_dist_med$relyear,es_disp_dist_med$med_rate)
plot(es_disp_dist_mean$relyear,es_disp_dist_mean$mean_rate)



library(forcats)
library(lemon)

full_otters_dist$seaotter <- as.factor(full_otters_dist$seaotter)
str(full_otters_dist)

#model fits

ggplot(otter_modelfit_dist)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 3000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  facet_wrap(~seaotter, scales = "free_y")+
  themeo 


#map
# make a spatial sf for mapping
otters_sf_dist <- sf::st_as_sf(full_otters_dist, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                          +datum=WGS84 +no_defs')

# make a simpler dataframe, then sf for mapping
otters_short_dist <- full_otters_dist %>%  dplyr::select(seaotter, latitude, longitude)
otters_short_dist_sf  <- sf::st_as_sf(otters_short_dist, coords = c("longitude", "latitude"), crs =' +proj=longlat +ellps=WGS84
                                 +datum=WGS84 +no_defs')

#plot
plot(otters_short_dist_sf) #simple plot to test


#faceted by sea otter + shoreline/contours/release locations + optional add in pathways between resights
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otters_short_dist_sf, aes(color = seaotter))+ #resight locations
  geom_path(data = full_otters_dist, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_dist_sf))+ #study site
  facet_wrap(~seaotter) #+
#themeo 

#select 3 example otters and create figure of maps/ model fits

#809
otter_fit_dist_809 <- subset(full_otters_dist, seaotter == "809")

#model fits
fit_809_dist<- ggplot(otter_fit_dist_809)+
  geom_histogram(aes(x=dist, y = ..density.. ), binwidth = 1500)+
  # geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  # geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
 # geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "red")+
 xlim(c(-1000,40000))+
  themeo 

#685
otter_fit_dist_685 <- subset(full_otters_dist, seaotter == "685")

#model fits
fit_685_dist <- ggplot(otter_fit_dist_685)+
  geom_histogram(aes(x=dist, y = ..density.. ), binwidth = 1500)+
  # geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  # geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  # geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "red")+
  #coord_cartesian(xlim = c(0,3), ylim = c(0,3))+
  xlim(c(-1000,40000))+
  themeo 


#475
otter_fit_dist_475 <- subset(full_otters_dist, seaotter == "475")

#model fits
fit_475_dist<- ggplot(otter_fit_dist_475)+
  geom_histogram(aes(x=dist, y = ..density.. ), binwidth = 1500)+
  # geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  # geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  #geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "red")+
  #coord_cartesian(xlim = c(0,3), ylim = c(0,7.5))+
  xlim(c(-1000,40000))+
  themeo 

library(gridExtra)
Fig1dist <- grid.arrange(map_809, fit_809_dist, map_685, fit_685_dist, map_475, fit_475_dist,
                     layout_matrix = rbind(c(1,1,1,1,2),
                                           c(1,1,1,1,2),
                                           c(1,1,1,1,2),
                                           c(1,1,1,1,2),
                                           c(3,3,3,3,4),
                                           c(3,3,3,3,4),
                                           c(3,3,3,3,4),
                                           c(3,3,3,3,4),
                                           c(5,5,5,5,6),
                                           c(5,5,5,5,6),
                                           c(5,5,5,5,6),
                                           c(5,5,5,5,6)))


#relnum
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relnum, color = relnum), binwidth = 1000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_dist$seaotter,
                               full_otters_dist$relnum,
                               max,
                               .desc = TRUE)),scales = "free", repeat.tick.labels = "FALSE")+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#relyear
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relyear, color = relyear), binwidth = 1000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_dist$seaotter,
                               full_otters_dist$relyear,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
 # coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#relmonth
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relmonth, color = relmonth), binwidth = 1000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_dist$seaotter,
                               full_otters_dist$relmonth,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 





# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#relsiteATOS
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relsiteATOS, color = relsiteATOS), binwidth = 1000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  # facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_dist$seaotter,
                               full_otters_dist$relsiteATOS,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#strandage_d
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = strandage_d, color = strandage_d), binwidth = 1000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_dist$seaotter,
                               full_otters_dist$strandage_d,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
 # coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# sex relnum relyear relmonth relsiteATOS relage_d strandage_d strandsiteATOS

#strandsiteATOS
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = strandsiteATOS, color = strandsiteATOS), binwidth = 1000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  #facet_wrap(~seaotter, scales = "free")+
  facet_rep_wrap(~(fct_reorder(full_otters_dist$seaotter,
                               full_otters_dist$strandsiteATOS,
                               max,
                               .desc = FALSE)),scales = "free", repeat.tick.labels = "FALSE")+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 




#View(full_otters_dist)

#histograms by demographics

#sex
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = sex, color = sex), binwidth = 1000)+
  facet_wrap(~sex)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

#relnum 
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relnum, color = relnum), binwidth = 1000)+
  facet_wrap(~relnum)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relyear 
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relyear, color = relyear), binwidth = 1000)+
  facet_wrap(~relyear)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relmonth 
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relmonth, color = relmonth), binwidth = 1000)+
  facet_wrap(~relmonth)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relsiteATOS 
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relsiteATOS, color = relsiteATOS), binwidth = 1000)+
  facet_wrap(~relsiteATOS)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# relage_d 
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = relage_d, color = relage_d), binwidth = 1000)+
  facet_wrap(~relage_d)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# strandage_d 
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = strandage_d, color = strandage_d), binwidth = 1000)+
  facet_wrap(~strandage_d)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 

# strandsiteATOS
ggplot(full_otters_dist)+
  geom_histogram(aes(x=dist, y = ..density.., fill = strandsiteATOS, color = strandsiteATOS), binwidth = 1000)+
  facet_wrap(~strandsiteATOS)+
  #coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 


mean_annual_disp_dist <- full_otters_dist %>% 
  group_by(seaotter, relyear) %>% summarize(mean(dist))
median_annual_disp_dist <- full_otters_dist %>% 
  group_by(seaotter,relyear) %>% summarize(median(dist))


ggplot(median_annual_disp_dist, aes(x=relyear, y = log(`median(dist)`)))+
  geom_point()+
  geom_smooth()+
  themeo

ggplot(mean_annual_disp_dist, aes(x=relyear, y = log(`mean(dist)`)))+
  geom_point()+
  geom_smooth()+
  themeo

#boxplots by demographics

#sex
ggplot(full_otters_dist)+
  geom_boxplot(aes(x=sex, y = log(dist)))+
  themeo 

#relnum 
ggplot(full_otters_dist)+
  geom_boxplot(aes(x=as.factor(relnum), y = log(dist)))+
  themeo 

# relyear 
ggplot(full_otters_dist)+
  # geom_boxplot(aes(x=as.factor(relyear), y = log(dist), fill = sex))+
  geom_boxplot(aes(x=as.factor(relyear), y = log(dist)))+
  themeo 

# relmonth 
ggplot(full_otters_dist)+
  geom_boxplot(aes(x=as.factor(relmonth), y = log(dist), fill = sex))+
  themeo 

# relsiteATOS 
ggplot(full_otters_dist)+
  geom_boxplot(aes(x=relsiteATOS, y = log(dist), group = cut_width(relsiteATOS , 1)))+
  themeo 

# relage_d 
ggplot(full_otters_dist)+
  geom_boxplot(aes(x=as.factor(relage_d), y = log(dist), group = cut_width(relage_d , 30)), width = 1)+
  themeo 

# strandage_d 
ggplot(full_otters_dist)+
  geom_boxplot(aes(x=as.factor(strandage_d), y = log(dist)))+
  themeo 

# strandsiteATOS
ggplot(full_otters_dist)+
  geom_boxplot(aes(x=as.factor(strandsiteATOS), y = log(dist)))+
  themeo 


#look at cauchy scale as dependent for plots
otter_cauchy_fits$seaotter <- as.character(otter_cauchy_fits$seaotter)
full_otters_dist_cauchy <- left_join(otter_cauchy_fits, releases, by = "seaotter")


mean_cauchy <- full_otters_dist_cauchy %>% 
  group_by(seaotter, relyear) %>% summarize(mean(scale))
median_cauchy <- full_otters_dist_cauchy %>% 
  group_by(seaotter,relyear) %>% summarize(median(scale))

#fill 2011 na value
density_2002_2017_nona <- density_2002_2017 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))
#not sure this is great
  shop.data %>% 
  group_by(hour) %>%
  mutate(profit= replace(profit, is.na(profit), mean(profit, na.rm=TRUE)))

p1<- ggplot(median_cauchy, aes(x=relyear, y = log(`median(scale)`)))+
  geom_point()+
  geom_smooth()+
  #geom_path(data = density_2002_2017, aes(x = year, y = log(dens.sm)))+
  themeo

p2<- ggplot(density_2002_2017, aes(x = year, y = dens.sm))+
  geom_line(color = "purple")+
 # geom_smooth()+
  themeo

p3<- ggplot(density_2002_2017, aes(x = year, y = X5yrtrend))+
  geom_line(color = "purple")+
 # geom_smooth()+
  themeo

p <- grid.arrange(p1, p2,p3,
                         layout_matrix = rbind(c(1,1),
                                               c(1,1),
                                              # c(1,1),
                                               c(2,2),
                                               c(3,3)))



ggplot(mean_cauchy, aes(x=relyear, y = log(`mean(scale)`)))+
  geom_point()+
  geom_smooth()+
  themeo

#boxplots by demographics #not usefule as many of these just have one input

#sex
ggplot(full_otters_dist_cauchy)+
  geom_boxplot(aes(x=sex, y = log(scale)))+
  themeo 

#relnum 
ggplot(full_otters_dist_cauchy)+
  geom_boxplot(aes(x=as.factor(relnum), y = log(scale)))+
  themeo 

# relyear 
ggplot(full_otters_dist_cauchy)+
   geom_boxplot(aes(x=as.factor(relyear), y = log(scale), fill = sex))+
 # geom_boxplot(aes(x=as.factor(relyear), y = log(scale)))+
  themeo 

# relmonth 
ggplot(full_otters_dist_cauchy)+
  geom_boxplot(aes(x=as.factor(relmonth), y = log(scale), fill = sex))+
  themeo 

# relsiteATOS 
ggplot(full_otters_dist_cauchy)+
  geom_boxplot(aes(x=relsiteATOS, y = log(scale), group = cut_width(relsiteATOS , 1)))+
  themeo 

# relage_d 
ggplot(full_otters_dist_cauchy)+
  geom_boxplot(aes(x=as.factor(relage_d), y = log(scale), group = cut_width(relage_d , 30)), width = 1)+
  themeo 

# strandage_d 
ggplot(full_otters_dist_cauchy)+
  geom_boxplot(aes(x=as.factor(strandage_d), y = log(scale)))+
  themeo 

# strandsiteATOS
ggplot(full_otters_dist_cauchy)+
  geom_boxplot(aes(x=as.factor(strandsiteATOS), y = log(scale)))+
  themeo 
