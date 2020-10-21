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

##### #read in and clean up data
otter_modelfit_rate <- read.csv(here("data_output/otter_model_fit_rate.csv"), stringsAsFactors = FALSE)
otter_modelfit_dist <- read.csv(here("data_output/otter_model_fit_dist.csv"), stringsAsFactors = FALSE)
releases <- read.csv(here("data/otter_releases.csv"), stringsAsFactors = FALSE)
es_pop_dens <- read.csv(here("data/censusES1985_2017.csv"), stringsAsFactors = FALSE)
cauchy <- read.csv("data_output/cauchy_scale.csv")
otter_cauchy_fits <- read.csv("data_output/otter_cauchy_fits.csv", stringsAsFactors = FALSE) #dist
resights <- read.csv(here("data_output/resights_dist_recap.csv"), stringsAsFactors = FALSE)

# clean up variable names and data type for merging later on
colnames(releases)[colnames(releases)=="ottername"] <- "seaotter"
releases$seaotter <- as.character(releases$seaotter)
otter_modelfit_rate$seaotter <- as.character(otter_modelfit_rate$seaotter)
otter_modelfit_dist$seaotter <- as.character(otter_modelfit_dist$seaotter)


###### Generate dfs for visualizations (all otters and ehs otters)
# Use distance based model fits 
# (could also sub in rate-based model fits here)
# otter_modelfit_rate

full_otters_dist <- left_join(otter_modelfit_dist, releases, by = "seaotter")
str(full_otters_dist)

#fix date
full_otters_dist$datetime <- as.POSIXct(paste(full_otters_dist$date, full_otters_dist$time), format = "%m/%d/%Y %H:%M")
head(full_otters_dist)

#create a df for just elkhorn slough otters
es_otters_dist <- full_otters_dist %>% dplyr::filter(relsiteATOS == "321")


########### Visualize model fits

#model fits
ggplot(full_otters_dist)+ # or, sub es_otters_dist
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 3000)+
  # geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  # geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  # geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  facet_wrap(~seaotter, scales = "free_y")+
  themeo 


########## Mapping
#### Prepare for maps

# make a spatial sf for mapping 
otters_sf_dist <- sf::st_as_sf(full_otters_dist, coords = c("longitude", "latitude"), # or use es_otters_dist
         crs =' +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# make a simpler dataframe, then sf for mapping
otters_short_dist <- full_otters_dist %>%  dplyr::select(seaotter, latitude, longitude)
otters_short_dist_sf  <- sf::st_as_sf(otters_short_dist, coords = c("longitude", "latitude"), 
          crs =' +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

#plot
plot(otters_short_dist_sf) #simple plot to test

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
release_site <- subset(otters_sf_dist, days_since_rel== 0) #make variable of only release locations


#faceted by sea otter + shoreline/contours/release locations + optional add in pathways between resights
ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otters_short_dist_sf, aes(color = seaotter))+ #resight locations
  geom_path(data = full_otters_dist, aes(x = longitude, y = latitude, color = seaotter), size = 1)+ #resignt pathways
  geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_dist_sf))+ #study site
  facet_wrap(~seaotter) +
  themeo 



#select 3 example otters and create figure of maps/ model fits

#809
otter_fit_dist_809 <- subset(full_otters_dist, seaotter == "809")
otter_map_809 <- subset(otters_short_dist_sf, seaotter == "809")

#model fits
fit_809_dist<- ggplot(otter_fit_dist_809)+
  geom_histogram(aes(x=dist, y = ..density.. ), binwidth = 1500)+
  # geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  # geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  # geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "red")+
  xlim(c(-1000,40000))+
  themeo 

#map
map_809<- ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otter_map_809, color = "blue")+ #resight locations
  geom_path(data = otter_fit_dist_809, aes(x = longitude, y = latitude), color = "blue", size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_dist_sf))+ #study site
  themeo


#685
otter_fit_dist_685 <- subset(full_otters_dist, seaotter == "685")
otter_map_685 <- subset(otters_short_dist_sf, seaotter == "685")

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

#map
map_685<- ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otter_map_685, color = "blue")+ #resight locations
  geom_path(data = otter_fit_dist_685, aes(x = longitude, y = latitude), color = "blue", size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_dist_sf))+ #study site
  themeo



#475
otter_fit_dist_475 <- subset(full_otters_dist, seaotter == "475")
otter_map_475 <- subset(otters_short_dist_sf, seaotter == "475")

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

#map
map_475<- ggplot()+
  geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c(-50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
  geom_path(data = cencal, aes(x = long, y = lat, group = group))+ #coastaline shapefile
  geom_sf(data = otter_map_475, color = "blue")+ #resight locations
  geom_path(data = otter_fit_dist_475, aes(x = longitude, y = latitude), color = "blue", size = 1)+ #resignt pathways
  # geom_sf(data = release_site, color = "red") + #release locations
  coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(otters_short_dist_sf))+ #study site
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


####################### Visualizing dependent variable from disp modelling
#### could use cauchy scale parameter
#### could use median cauchy dist estimate
#### For now, we will stick with cauchy scale, based on it dominating AIC values

#look at cauchy scale as dependent for plots
otter_cauchy_fits$seaotter <- as.character(otter_cauchy_fits$seaotter)
full_otters_dist_cauchy <- left_join(otter_cauchy_fits, releases, by = "seaotter")

# summarize for looking at yearly values
mean_cauchy <- full_otters_dist_cauchy %>% 
  group_by(seaotter, relyear) %>% summarize(mean(scale))
median_cauchy <- full_otters_dist_cauchy %>% 
  group_by(seaotter,relyear) %>% summarize(median(scale))


#bring in release datetime for looking at temporal patterns
release_date <- subset(resights, days_since_rel== 0) #make variable of only release dates 
release_date <- release_date %>% dplyr::select(seaotter, date)

release_date$seaotter <- as.character(release_date$seaotter)
release_date$rel_date <- release_date$date

scale_date <- left_join(full_otters_dist_cauchy, release_date, by = "seaotter")
str(scale_date)

#create object of all release affiliated metrics
release_metrics <- scale_date %>% dplyr::select(seaotter, scale, rel_date, sex, relnum, relyear, relmonth, relsiteATOS, relage_d, strandage_d, strandsiteATOS )
release_metrics <- release_metrics %>% group_by(seaotter) %>% 
         summarise(scale = first(scale), 
                    rel_date = first(rel_date),
                    sex = first(sex),
                    relnum = first(relnum), 
                    relyear = first(relyear), 
                    relmonth= first(relmonth), 
                    relsiteATOS= first(relsiteATOS),
                     relage_d = first(relsiteATOS), 
                    strandage_d= first(strandage_d), 
                    strandsiteATOS = first(strandsiteATOS))

release_metrics$rel_date_real <- lubridate::mdy(release_metrics$rel_date)

#plot scale by release date
scale_plot <- ggplot(release_metrics, aes(x = rel_date_real, y = scale))+
  geom_line()+
  geom_point()+
  geom_smooth()+
  themeo

scale_plot_out <- ggplot(release_metrics_out, aes(x = rel_date_real, y = scale))+
  geom_line()+
  geom_point()+
  geom_smooth()+
  themeo

# summarized by year
ggplot(median_cauchy, aes(x=relyear, y = log(`median(scale)`)))+ # could also use mean_caucy and mean(scale)
  geom_point()+
  geom_smooth()+
  #geom_path(data = density_2002_2017, aes(x = year, y = log(dens.sm)))+
  themeo









########################  Predictor variables

######################## Local otter population density
#elkhorn slough population density for only survey years
density_2002_2017 <- es_pop_dens %>% dplyr::filter(year >= 2002)

#fill 2011 na value
density_2002_2017_nona <- density_2002_2017 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))
#not sure this is great

# calulate median/mean dispersal rate per year for elkhorn slough otters
es_disp_dist_med <- es_otters_dist %>% group_by(relyear) %>% summarize(med_rate = median(km_hr))
es_disp_dist_mean <- es_otters_dist %>% group_by(relyear) %>% summarize(mean_rate = mean(km_hr))

#exploratory plots
plot(density_2002_2017$dens.sm, es_disp_dist_med$med_rate)
plot(density_2002_2017$dens.sm, es_disp_dist_mean$mean_rate)
plot(density_2002_2017$year, density_2002_2017$dens.sm)

#plot yearly density
ggplot(density_2002_2017, aes(x = year, y = dens.sm))+ # could also look at X5yrtrend (5 year density trend)
  geom_line(color = "purple")+
  # geom_smooth()+
  themeo

################################## SST and ENSO
ehs_temp <- read.csv("C:/Users/SBecker/Local-Git/otter_dispersal/data/EHS_Temp/ehs_raw_temp.csv") #read in temp data
head(ehs_temp) # check col names
ehs_temp <- ehs_temp %>% dplyr::select(DateTimeStamp, Temp) # select datetime and temp

ehs_temp$DateTimeStamp <- mdy_hm(ehs_temp$DateTimeStamp) # make date a date (POSIXct) format
ehs_temp$date <- date(ehs_temp$DateTimeStamp) # create a date variable from datetime

ehd_temp_daily_mean <- ehs_temp %>% group_by(date) %>% summarise(mean_temp = mean(Temp)) # mean daily temp
ehd_temp_daily_mean_nona <- ehd_temp_daily_mean %>% fill(mean_temp) # fill NA with previous value (?)

temp_plot <- ggplot(ehd_temp_daily_mean_nona, aes(x = date, y = mean_temp))+
  geom_path()+
  geom_smooth()+
  themeo

ehd_temp_daily_mean_nona$year <- year(ehd_temp_daily_mean_nona$date)

# ggplot(ehd_temp_daily_mean_nona)+
#   geom_boxplot(aes(x=as.factor(year), y = log(mean_temp)))+
#   themeo


#now read in enso rank data
# consider using index instead, for now, use rank
ENSO_rank <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/ENSO_rank.txt", skip = 1)
ENSO_rank_names <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/ENSO_rank.txt", nrow = 1, stringsAsFactors = FALSE)

str(ENSO_rank)
str(ENSO_rank_names)

#combine names and values
names(ENSO_rank) <- ENSO_rank_names 

#reshape data into tidy format
ENSO <- gather(ENSO_rank, "DECJAN", "JANFEB", "FEBMAR", "MARAPR", "APRMAY" ,
               "MAYJUN", "JUNJUL" ,"JULAUG" ,"AUGSEP", "SEPOCT" ,"OCTNOV", "NOVDEC",
               key = "month", value = "rank")

ENSO_2002_2018 <- ENSO %>% filter(YEAR >= 2002)


month<- as.character(c("DECJAN", "JANFEB", "FEBMAR", "MARAPR", "APRMAY" ,
                       "MAYJUN", "JUNJUL" ,"JULAUG" ,"AUGSEP", "SEPOCT" ,"OCTNOV", "NOVDEC"))
month_val <- as.numeric(c(1:12))

month_rank <- as.data.frame(cbind(month, month_val), stringsAsFactors = FALSE)
str(month_rank)

#month_rank$month <- as.character(month_rank$month)
month_rank$month_val <- as.numeric(month_rank$month_val)


ENSO_2002_2018 <- left_join(ENSO_2002_2018, month_rank, by = "month" )
str(ENSO_2002_2018)

#ENSO_2002_2018$month_val <- as.numeric(ENSO_2002_2018$month_val)

ENSO_2002_2018 <-  arrange(ENSO_2002_2018, YEAR, month_val)

ENSO_2002_2018 <- ENSO_2002_2018 %>% dplyr::select(YEAR, month, rank, month_val)

ENSO_2002_2018$yearmon<- paste(ENSO_2002_2018$YEAR,ENSO_2002_2018$month_val)
ENSO_2002_2018$yearmon <- parse_date_time(ENSO_2002_2018$yearmon, "ym")
plot(ENSO_2002_2018$yearmon, ENSO_2002_2018$rank)

enso_plot<- ggplot(ENSO_2002_2018, aes(x = yearmon, y= rank))+
  geom_line()+
  geom_smooth()+
  themeo


#PDO

#now read in enso rank data
# consider using index instead, for now, use rank
PDO <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/PDO.txt", skip = 1)
PDO_names <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/PDO.txt", nrow = 1, stringsAsFactors = FALSE)

str(PDO)
str(PDO_names)

#combine names and values
names(PDO) <- PDO_names 
PDO$YEAR <- as.character(PDO$YEAR)
#reshape data into tidy format
PDO <- gather(PDO, "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT" ,"NOV", "DEC",
               key = "month", value = "rank")

PDO_2002_2017 <- PDO %>% filter(YEAR >= "2002")

data_clean <- function(x) sapply (strsplit(x , '[*]' ), `[` , 1)
PDO_2002_2017$YEAR<- data_clean(PDO_2002_2017$YEAR)

str(PDO_2002_2017)

#PDO_2002_2017 %>% unite(Date, YEAR, month, sep = " ") %>% View()
PDO_2002_2017$Date <- paste(PDO_2002_2017$YEAR, PDO_2002_2017$month, "15", sep="-")
PDO_2002_2017$Date <- ymd(PDO_2002_2017$Date)

pdo_plot<- ggplot(PDO_2002_2017, aes(x = Date, y= rank))+
  geom_line()+
  geom_smooth()+
  themeo


grid.arrange(scale_plot, temp_plot, enso_plot, pdo_plot,
             layout_matrix = rbind(c(1),
                                   c(2),
                                   c(3),
                                   c(4)))


#combine SST and release_metrics by date (might need to rename some things first)
# then look for correlations 

str(ehd_temp_daily_mean_nona)
str(release_metrics)

release_metrics$date <- release_metrics$rel_date_real
release_metrics <- left_join(release_metrics, ehd_temp_daily_mean_nona, by = "date")

plot(release_metrics$mean_temp, log(release_metrics$scale))

ggplot(release_metrics, aes(x = mean_temp, y = log(scale)))+
  geom_path()

# calculate some moving averages ehd_temp_daily_mean_nona

library(zoo)
ehd_temp_daily_mean_nona <- ehd_temp_daily_mean_nona %>% 
  mutate(temp_ma_week = rollmean(x = mean_temp, 7, align = "right" , fill = NA),
         temp_ma_month = rollmean(x = mean_temp, 28, align = "right" , fill = NA),
         temp_ma_year = rollmean(x = mean_temp, 365, align = "right" , fill = NA)) 

release_metrics <- left_join(release_metrics, ehd_temp_daily_mean_nona, by = "date")

ggplot(release_metrics, aes(x = temp_ma_week.x, y = scale))+
  geom_point()

ggplot(release_metrics, aes(x = temp_ma_month, y = scale))+
  geom_point()

ggplot(release_metrics, aes(x = temp_ma_year, y = scale))+
  geom_point()

release_metrics_out<-release_metrics[!(release_metrics$seaotter=="225"),]

ggplot(release_metrics_out, aes(x = temp_ma_week.x, y = scale))+
  geom_point()+
  themeo

ggplot(release_metrics_out, aes(x = temp_ma_month, y = scale))+
  geom_point()+
  themeo


ggplot(release_metrics_out, aes(x = temp_ma_year, y = scale))+
  geom_point()+
  themeo
library(viridis)

ggplot()+
 #geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = mean_temp), color = "black")+
 #geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = temp_ma_week), color = "blue")+
  geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = temp_ma_month), color = "black")+
  #geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = temp_ma_year), color = "red")+
  geom_point(data = release_metrics_out, aes (x = date, y = temp_ma_week.x, color = scale), size = 3)+
  scale_color_viridis()+
  themeo
  

ggplot(data = release_metrics, aes(x= temp_ma_week.x, y = scale))+
  geom_point()+
   geom_smooth(method = "lm", formula = y~x)+
  themeo

ggplot(data = release_metrics, aes(x= relmonth, y = scale, group = relmonth))+
  geom_boxplot()+
  themeo

ggplot(data = release_metrics, aes(x= relyear, y = scale, group = relyear))+
  geom_boxplot()+
  # geom_smooth()+
  themeo

ggplot(data = release_metrics, aes(x= relsiteATOS, y = scale, group = relsiteATOS))+
  geom_boxplot()+
  # geom_smooth()+
  themeo

ggplot(data = release_metrics, aes(x= relage_d, y = scale))+ # these numbers are the same as ATOS site?
  geom_point()+
  # geom_smooth()+
  themeo

ggplot(data = release_metrics, aes(x= strandage_d, y = scale))+ 
  geom_point()+
  # geom_smooth()+
  themeo

day <- ggplot(data = release_metrics, aes(x= mean_temp.x, y = scale))+ 
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)+
  themeo

week <- ggplot(data = release_metrics, aes(x= temp_ma_week.y, y = scale))+ 
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)+
  themeo

month <- ggplot(data = release_metrics, aes(x= temp_ma_month, y = scale))+ 
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)+
  themeo

year <- ggplot(data = release_metrics, aes(x= temp_ma_year, y = scale))+ 
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)+
  themeo

sst_all <- grid.arrange(day, week, month, year,
             layout_matrix = rbind(c(1,2),
                                   c(3,4)))

day_out <- ggplot(data = release_metrics_out, aes(x= mean_temp.x, y = scale))+ 
  geom_point()+
  #geom_smooth(method = "lm", formula = y~x)+
  geom_smooth()+
  themeo

week_out <- ggplot(data = release_metrics_out, aes(x= temp_ma_week.y, y = scale))+ 
  geom_point()+
  #geom_smooth(method = "lm", formula = y~x)+
  geom_smooth()+
  themeo

month_out <- ggplot(data = release_metrics_out, aes(x= temp_ma_month, y = scale))+ 
  geom_point()+
  #geom_smooth(method = "lm", formula = y~x)+
  geom_smooth()+
  themeo

year_out <- ggplot(data = release_metrics_out, aes(x= temp_ma_year, y = scale))+ 
  geom_point()+
  #geom_smooth(method = "lm", formula = y~x)+
  geom_smooth()+
  themeo

sst_out <- grid.arrange(day_out, week_out, month_out, year_out,
                        layout_matrix = rbind(c(1,2),
                                              c(3,4)))

#add in es density values for year of release

body_condition <- read.csv(here::here("data/body_condition.csv"), stringsAsFactors = FALSE)

body_condition$reldate <- mdy(body_condition$reldate)
body_condition$relweightdate <- mdy(body_condition$relweightdate)
body_condition$bcindxdate <- mdy(body_condition$bcindxdate)

body_condition$w_v_l_date <- body_condition$relweightdate - body_condition$bcindxdate

#calculate and add in a body condition metric
body_condition$condition <- log(body_condition$weightkg.1) / log(body_condition$lengthcm)

body_condition_rel1 <- body_condition %>% filter(relnum == "1")
  
body_condition_rel1$seaotter <- body_condition_rel1$otter
body_condition_rel1$seaotter <- as.character(body_condition_rel1$seaotter)

release_metrics <- left_join(release_metrics, body_condition_rel1, by = "seaotter")
release_metrics_out <- left_join(release_metrics_out, body_condition_rel1, by = "seaotter")

ggplot(data = release_metrics_out, aes(x= condition, y = scale, color = sex))+ 
  geom_point()+
 #geom_smooth()+
  themeo 
