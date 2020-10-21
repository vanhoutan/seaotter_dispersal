##################################################################
#####  drivers of dispersal distance (cauchy scale parameter or median lcp dist)

##### read in packages
library(here)
library(tidyverse)

library(sf)
library(rgdal)
library(raster)

library(forcats)
library(lemon)
library(lubridate)

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

themeo2 <-theme_classic()+
  theme(strip.background = element_blank(),
        panel.grid.major = element_line(colour = "transparent"), panel.grid.minor = element_line(colour = "transparent"),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
       axis.text.x = element_text(margin = ggplot2::margin( 0.2, unit = "cm"), color = "black", size = 7),
        axis.title.x  = element_text(color = "black", size = 8),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.ticks.x = element_line(color = "black", size = .25),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.spacing = unit(.5, "lines"),
        #    legend.title=element_blank(),
        #   legend.position = "none",
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 7),
        strip.text=element_text(hjust=0) ,
        strip.text.x = element_blank())



##### read in data
releases <- read.csv(here::here("data/otter_releases.csv"), stringsAsFactors = FALSE)
es_pop_dens <- read.csv(here::here("data/censusES1985_2017.csv"), stringsAsFactors = FALSE)
resights <- read.csv(here::here("data_output/resights_dist_recap.csv"), stringsAsFactors = FALSE)
resight_models_est_par <- readRDS(here::here("data_output/resight_models_est_par.rds"))

##### clean up variable names and data type for merging later on
colnames(releases)[colnames(releases)=="ottername"] <- "seaotter"
releases$seaotter <- as.character(releases$seaotter)

###### Generate dfs for visualizations (all otters and ehs otters)
# Use lcp distance based model fits 
# (could also sub in rate-based model fits here)
# resight_models_est_par

full_otters_dist <- left_join(resight_models_est_par, releases, by = "seaotter")
full_otters_dist

#bring in release datetime for looking at temporal patterns
release_date <- subset(resights, days_since_rel== 0) #make variable of only release dates 
release_date <- release_date %>% dplyr::select(seaotter, date)

release_date$seaotter <- as.character(release_date$seaotter)
release_date$rel_date <- release_date$date

scale_date <- left_join(full_otters_dist, release_date, by = "seaotter")
scale_date

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
            relage_d = first(relage_d), 
            strandage_d= first(strandage_d), 
            strandsiteATOS = first(strandsiteATOS))

release_metrics$rel_date_real <- lubridate::mdy(release_metrics$rel_date)

############ fig3
#plot scale by release date
scale_plot_no225 <- ggplot(release_metrics, aes(x = rel_date_real, y = scale))+
  geom_smooth(color = "darkgrey")+
  geom_line(color = "grey")+
#  geom_point(aes(color = scale),size = 3)+
  geom_point()+
  xlab("release date")+
  ylab("scale")+
#  scale_color_gradient( low = "blue4", high = "gold")+
  coord_cartesian(xlim = NULL, ylim = c(0,3.25))+
  themeo

scale_plot_all <- ggplot(release_metrics, aes(x = rel_date_real, y = scale))+
  geom_smooth(color = "darkgrey")+
  geom_line(color = "grey")+
  geom_point(aes(color = scale),size = 3)+
  xlab("release date")+
  ylab("scale")+
  scale_color_gradient( low = "blue4", high = "gold")+
  themeo


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
  geom_text(aes(label = seaotter))+
  #geom_point(aes(color = scale),size = 3)+
  xlab("release date")+
  ylab("scale")+
  # scale_color_gradient( low = "blue4", high = "gold")+
  coord_cartesian(xlim = NULL, ylim = c(-.25,6.25), expand =0)+
  themeo


library(gridExtra)
grid.arrange(scale_plot_no225_plain, scale_plot_all_plain,
             layout_matrix = rbind(c(1),
                                   c(2)))


########################  Predictor variables: fig 2

######################## Local otter population density
#elkhorn slough population density for only survey years
es_pop_dens <- read.csv(here::here("data/censusES1985_2017.csv"), stringsAsFactors = FALSE)
density_2002_2017 <- es_pop_dens %>% dplyr::filter(year >= 2002)

#fill 2011 na value
density_2002_2017_nona <- density_2002_2017 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))
#not sure this is great

#create a df for just elkhorn slough otters
es_otters_dist <- release_metrics %>% dplyr::filter(relsiteATOS == "321")

# calulate median/mean dispersal rate per year for elkhorn slough otters
es_disp_dist_med <- es_otters_dist %>% group_by(relyear) %>% summarize(med_scale = median(scale))
es_disp_dist_mean <- es_otters_dist %>% group_by(relyear) %>% summarize(mean_scale = mean(scale))

#drop  2016
#density_2002_2017_nona <- density_2002_2017_nona[-15,]

#exploratory plots
# plot(density_2002_2017_nona$dens.sm, es_disp_dist_med$med_scale)
# plot(density_2002_2017_nona$dens.sm, es_disp_dist_mean$mean_scale)
# plot(density_2002_2017_nona$year, density_2002_2017_nona$dens.sm)
# plot(density_2002_2017_nona$year, es_disp_dist_med$med_scale)

  
#plot yearly density
es_density <- ggplot(density_2002_2017_nona, aes(x = year, y = dens.sm))+ # could also look at X5yrtrend (5 year density trend)
  geom_area(fill = "#93bbba")+
  coord_cartesian(expand = 0)+
  xlab("")+
  ylab("population density")+
  themeo


# ### now for monterey
# # years are weird with this one
# mon_pop_dens <- read.csv(here::here("data/censusMON.csv"), stringsAsFactors = FALSE)
# mon_density_2002_2018 <- mon_pop_dens %>% dplyr::filter(year >= 2002)
# 
# #fill 2011 na value
# #mon_density_2002_2017_nona <- mon_density_2002_2017 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))
# #not sure this is great
# 
# #create a df for just monterey slough otters
# mon_otters_dist <- release_metrics %>% dplyr::filter(relsiteATOS != "321")
# 
# # calulate median/mean dispersal rate per year for monterey otters
# mon_disp_dist_med <- mon_otters_dist %>% group_by(relyear) %>% summarize(med_scale = median(scale))
# mon_disp_dist_mean <- mon_otters_dist %>% group_by(relyear) %>% summarize(mean_scale = mean(scale))
# 
# #drop  2016
# #mon_density_2002_2017 <- mon_density_2002_2017[-15,]
# 
# mon_density <- ggplot(mon_density_2002_2018, aes(x = year, y = dens_sm))+ # could also look at X5yrtrend (5 year density trend)
#   geom_area(fill = "#66a47b")+
#   coord_cartesian(expand = 0)+
#   xlab("")+
#   ylab("population density")+
#   themeo


################################## SST and ENSO
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
  geom_smooth(color = "#66a47b", fill = "#66a47b", alpha =0.5, span = 0.6)+
  geom_line()+
  xlab("")+
  ylab("MEI rank")+
  coord_cartesian( expand = 0)+
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
  geom_smooth(color = "#66a47b", fill = "#66a47b", alpha =0.5, span = 0.6)+
  geom_line()+
  xlab("year")+
  ylab("PDO rank")+
  coord_cartesian( expand = 0)+
  themeo
                                 

#### try PDO with all data, last few months of 2018 NA

PDO2 <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/PDO2.txt", skip = 1)
PDO2_names <- read.table("C:/Users/SBecker/Local-Git/otter_dispersal/data/PDO2.txt", nrow = 1, stringsAsFactors = FALSE)

str(PDO2)
str(PDO2_names)

#combine names and values
names(PDO2) <- PDO2_names 
PDO2$YEAR <- as.character(PDO2$YEAR)
#reshape data into tidy format
PDO2 <- gather(PDO2, "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT" ,"NOV", "DEC",
              key = "month", value = "rank")

PDO2_2002_2018 <- PDO2 %>% filter(YEAR >= "2002")

data_clean <- function(x) sapply (strsplit(x , '[*]' ), `[` , 1)
PDO2_2002_2018$YEAR<- data_clean(PDO2_2002_2018$YEAR)

str(PDO2_2002_2018)

#PDO_2002_2017 %>% unite(Date, YEAR, month, sep = " ") %>% View()
PDO2_2002_2018$Date <- paste(PDO2_2002_2018$YEAR, PDO2_2002_2018$month, "15", sep="-")
PDO2_2002_2018$Date <- ymd(PDO2_2002_2018$Date)

pdo2_plot<- ggplot(PDO2_2002_2018, aes(x = Date, y= rank))+
  geom_smooth(color = "#66a47b", fill = "#66a47b", alpha =0.5, span = 0.6)+
  geom_line()+
  xlab("year")+
  ylab("PDO rank")+
  coord_cartesian( expand = 0)+
  themeo

#combine SST and release_metrics by date (might need to rename some things first)
# then look for correlations 

str(ehd_temp_daily_mean_nona)
str(release_metrics)

release_metrics$date <- release_metrics$rel_date_real
release_metrics <- left_join(release_metrics, ehd_temp_daily_mean_nona, by = "date")

plot(release_metrics$mean_temp, log(release_metrics$scale))


temp_plot_no225 <- ggplot(release_metrics, aes(x = mean_temp, y = scale))+
  geom_line(color = "grey")+
  geom_point(aes(color = scale), size = 3)+
  geom_smooth(method = "lm", color = "darkgrey")+
  #geom_smooth(color = "darkgrey")+
  scale_color_gradient( low = "blue4", high = "gold")+
  coord_cartesian(xlim = NULL, ylim = c(0,3))+
  themeo

temp_plot_all <- ggplot(release_metrics, aes(x = mean_temp, y = scale))+
  geom_line(color = "grey")+
  geom_point(aes(color = scale), size = 3)+
 geom_smooth(method = "lm", color = "darkgrey")+
  #geom_smooth(color = "darkgrey")+
  scale_color_gradient( low = "blue4", high = "gold")+
  themeo


grid.arrange(scale_plot_all_plain, temp_plot, enso_plot, pdo_plot,
             layout_matrix = rbind(c(1),
                                   c(2),
                                   c(3),
                                   c(4)))

#plot year by mean relnum

relnum_mean <- releases %>% group_by(relyear) %>% mutate(mean_relnum=mean(relnum)) 
scale <- release_metrics %>% dplyr::select(seaotter, scale)

relnum_mean <- left_join(relnum_mean, scale, by = "seaotter")
relnum_mean <- relnum_mean %>% group_by(relyear) %>%  mutate(mean_scale = mean(scale))

ggplot(relnum_mean, aes(x = relyear, y = mean_relnum))+
  geom_line(color = "grey")+
  geom_point(aes(color = mean_scale), size = 3)+
  geom_smooth(color = "darkgrey")+
  scale_color_gradient( low = "blue4", high = "gold")+
  themeo

relnum_plain <- ggplot(relnum_mean, aes(x = relyear, y = mean_relnum))+
  #geom_line(color = "grey")+
  geom_point()+
  geom_smooth(color = "darkgrey")+
 # scale_color_gradient( low = "blue4", high = "gold")+
  xlab("year")+
  ylab("mean number of releases per otter")+
  themeo

grid.arrange(scale_plot_no225_plain, relnum_plain, temp_plot, enso_plot, pdo_plot,
             layout_matrix = rbind(c(1),
                                   c(2),
                                   c(3),
                                   c(4),
                                   c(5)))
scale_plot_all_plain
#histograms by demographics

#sex
ggplot(release_metrics)+
  geom_histogram(aes(x=scale, y = ..density.., fill = sex, color = sex), binwidth = .5)+
  facet_wrap(~sex)+
  themeo 

#relnum 
ggplot(relnum_mean)+
  geom_histogram(aes(x=scale, y = ..density.., fill = relnum, color = relnum), binwidth = .5)+
  facet_wrap(~relnum)+
  themeo 

# relyear 
ggplot(release_metrics)+
  geom_histogram(aes(x=scale, y = ..density.., fill = relyear, color = relyear), binwidth = .3)+
  facet_wrap(~relyear)+
  themeo 

# relmonth 
ggplot(release_metrics)+
  geom_histogram(aes(x=scale, y = ..density.., fill = relmonth, color = relmonth), binwidth = .5)+
  facet_wrap(~relmonth)+
  themeo 

# relsiteATOS 
ggplot(release_metrics)+
  geom_histogram(aes(x=scale, y = ..density.., fill = relsiteATOS, color = relsiteATOS), binwidth = .5)+
  facet_wrap(~relsiteATOS)+
  themeo 

# relage_d 
ggplot(release_metrics)+
  geom_histogram(aes(x=scale, y = ..density.., fill = relage_d, color = relage_d), binwidth = .25)+
  facet_wrap(~relage_d)+
  themeo 

# strandage_d 
ggplot(release_metrics)+
  geom_histogram(aes(x=scale, y = ..density.., fill = strandage_d, color = strandage_d), binwidth = .5)+
  facet_wrap(~strandage_d)+
  themeo 

# strandsiteATOS
ggplot(release_metrics)+
  geom_histogram(aes(x=scale, y = ..density.., fill = strandsiteATOS, color = strandsiteATOS), binwidth = .5)+
  facet_wrap(~strandsiteATOS)+
  themeo 




#boxplots by demographics

#sex
ggplot(release_metrics)+
  geom_boxplot(aes(x=sex, y = scale))+
  themeo 

#relnum 
ggplot(relnum_mean)+
  geom_boxplot(aes(x=as.factor(relnum), y = scale))+
  themeo 

# relyear 
ggplot(release_metrics)+
 # geom_boxplot(aes(x=as.factor(relyear), y = scale, fill = sex))+
  geom_boxplot(aes(x=as.factor(relyear), y = scale))+
 coord_cartesian(xlim = NULL, ylim = c(0,3))+
  themeo 

# relmonth 
ggplot(release_metrics)+
 # geom_boxplot(aes(x=as.factor(relmonth), y = scale, fill = sex))+
 geom_boxplot(aes(x=as.factor(relmonth), y = scale))+
  #geom_violin(aes(x=as.factor(relmonth), y = scale))+
#  coord_cartesian(xlim = NULL, ylim = c(0,3))+
  themeo 

# relsiteATOS
ggplot(release_metrics)+
  geom_boxplot(aes(x=relsiteATOS, y = scale, group = cut_width(relsiteATOS , 1)))+
  themeo

# relage_d
ggplot(release_metrics)+
  geom_boxplot(aes(x=as.factor(relage_d), y = scale, group = cut_width(relage_d , 30)), width = 1)+
  themeo

# strandage_d 
ggplot(release_metrics)+
  geom_boxplot(aes(x=as.factor(strandage_d), y = scale))+
  themeo 




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
#release_metrics_out <- left_join(release_metrics_out, body_condition_rel1, by = "seaotter")


ggplot(release_metrics)+ 
  geom_histogram(aes(x= condition, y = ..density.., fill = sex), binwidth = .05)+
  #geom_point(aes(x = condition, y = scale)) +
  #geom_smooth(aes(x = condition, y = scale),method = "lm", color = "darkgrey")+
  # coord_cartesian(xlim = NULL, ylim = c(0,3))+
  facet_wrap(~sex)+
  themeo 











########################## Figure 2

condition_plot <- ggplot(release_metrics)+ 
  geom_histogram(aes(x= condition, y = ..density.., fill = sex), binwidth = .05)+
 facet_wrap(~sex)+
  scale_fill_manual(values = c("#90abc9","#eda621"))+
  themeo 

condition_plot_density <- ggplot(release_metrics)+
  geom_density(aes(x=condition), fill = "grey", color = "grey", alpha = 0.5)+
  geom_density(aes(x=condition, group = sex,color = sex, fill = sex), alpha = 0.35, show.legend = FALSE)+
  scale_fill_manual(values = c("#90abc9","#eda621"))+
  scale_color_manual(values = c("#90abc9","#eda621"))+
  xlab("body condition index")+
  ylab("density")+
  coord_cartesian(xlim= NULL, ylim= c(0,16), expand= 0)+
  themeo

condition_plot_scatter <- ggplot(release_metrics)+
  geom_point(aes(x = fct_rev(seaotter), y = condition, color = sex), size = 2, show.legend = FALSE)+
  coord_flip()+
  xlab("")+
  ylab("log(weight)/log(length)")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo2

strand_age_plot <- ggplot(release_metrics)+ 
 geom_histogram(aes(x= strandage_d, y = ..density..), binwidth = 15)+
 # facet_wrap(~sex)+
  themeo 

rel_age_plot <- ggplot(release_metrics)+ 
geom_histogram(aes(x= relage_d, y = ..density..), binwidth = 100)+
#  facet_wrap(~sex)+
  themeo 
 
release_metrics$days_in <- release_metrics$relage_d - release_metrics$strandage_d     

days_in_plot <- ggplot(release_metrics)+ 
  geom_histogram(aes(x= days_in, y = ..density..), binwidth = 75)+
  #facet_wrap(~sex)+
  themeo 


release_metrics_age <- release_metrics %>% 
  mutate(seaotter = fct_reorder(seaotter, date))

age_plot<- ggplot(release_metrics_age)+
   geom_segment(aes(x=strandage_d, xend = relage_d, y = fct_rev(seaotter), yend = fct_rev(seaotter)))+
  geom_point(aes(x = strandage_d, y = fct_rev(seaotter)), color = "#90abc9", size = 2)+
  geom_point(aes(x = relage_d, y = fct_rev(seaotter)), color = "#eda621", size = 2)+
  xlab("age (# days old)")+
  ylab("sea otter ID")+
  themeo 

age_plot2<- ggplot(release_metrics_age)+
  geom_segment(aes(x=strandage_d, xend = relage_d, y = fct_rev(seaotter), yend = fct_rev(seaotter)))+
  #geom_point(aes(x = strandage_d, y = fct_rev(seaotter)), color = "#fca239", size = 2)+
  geom_point(aes(x = strandage_d, y = fct_rev(seaotter)), color = "#9c9592", size = 2)+
  geom_point(aes(x = relage_d, y = fct_rev(seaotter)), color = "#60003a", size = 2)+
  xlab("age (# days old)")+
  ylab("sea otter ID")+
  themeo 



grid.arrange(strand_age_plot, rel_age_plot, days_in_plot,
                         layout_matrix = rbind(c(1,1),
                                               c(2,2),
                                               c(3,3)))


relnum_plot <- ggplot(relnum_mean) + 
  geom_histogram(aes(x= relnum, y = ..density..), binwidth = 1)+
  themeo 



release_metrics_sexratio <- release_metrics %>% group_by(sex) %>% summarise(n = n())

sex_ratio_plot <- ggplot(release_metrics_sexratio)+ 
  geom_bar(aes(x= sex, y = n, fill = sex), stat = "identity",show.legend = FALSE)+
  #scale_fill_manual(values = c("#90abc9","#eda621"))+
  scale_fill_manual(values = c("#3d60c0", "#60003a"))+
  coord_cartesian(xlim= NULL, ylim= c(0,25), expand= 0)+
  xlab("")+
  ylab("count")+
  themeo 

release_metrics <- release_metrics %>% group_by(seaotter) %>% 
  mutate(relyear_first = first(relyear))

release_metrics_otters_year <- release_metrics %>% group_by(relyear_first) %>% summarise(n = n())

release_metrics_otters_year_exp <-release_metrics_otters_year %>% ungroup() %>% tidyr::expand(relyear_first=2002:2018)
release_metrics_otters_year<- left_join(release_metrics_otters_year_exp, release_metrics_otters_year, by = "relyear_first") 
release_metrics_otters_year[is.na(release_metrics_otters_year)] <- 0

release_metrics_otters_year_sum <- release_metrics_otters_year %>% group_by(relyear_first) %>% summarise(otters_year = sum(n))

otters_year_plot <- ggplot(release_metrics_otters_year)+ 
  geom_bar(aes(x= as.factor(relyear_first), y = n), stat = "identity")+
  xlab("")+
  ylab("# otters released")+
  themeo 

otters_year_plot2 <- ggplot()+ 
  geom_area(data = release_metrics_otters_year_sum, aes(x= relyear_first, y = otters_year))+
  xlab("")+
  ylab("# otters released")+
  themeo 

release_metrics_relnum_year <- relnum_mean %>% group_by(seaotter) %>% 
  mutate(relyear_first = first(relyear), max_relnum = max(relnum)) %>% 
  group_by(relyear_first) %>% summarise(mean_relnum_year = mean(max_relnum))

release_metrics_relnum_year_exp <-release_metrics_relnum_year %>% ungroup() %>% tidyr::expand(relyear_first=2002:2018)
release_metrics_relnum_year<- left_join(release_metrics_relnum_year_exp, release_metrics_relnum_year, by = "relyear_first") 
release_metrics_relnum_year[is.na(release_metrics_relnum_year)] <- 0



relnum_mean_otter_recaps <- relnum_mean %>% group_by(seaotter) %>% mutate(relyear_first = first(relyear), max_relnum = max(relnum)) 

relnum_mean_otter_recaps_sum <- relnum_mean_otter_recaps %>% group_by(seaotter) %>% 
  summarise(max_relnum=first(max_relnum) , relyear_first = first(relyear_first)) 

relnum_mean_recaps_only <- relnum_mean_otter_recaps_sum %>% group_by(relyear_first) %>% dplyr::filter(max_relnum >1)

relnum_mean_recaps_only_sum <- relnum_mean_recaps_only %>% group_by(relyear_first) %>% summarise(recap_ind = n())


relnum_year_plot <- ggplot(release_metrics_relnum_year)+ 
  geom_bar(aes(x= as.factor(relyear_first), y = mean_relnum_year), stat = "identity")+
  xlab(" ")+
  ylab("mean # recaptures")+
  themeo 

relnum_ind_year_plot <- ggplot(relnum_mean_recaps_only_sum)+ 
  geom_area(aes(x= relyear_first, y = recap_ind))+
  xlab(" ")+
  ylab("# individuals recaptured")+
  themeo 


release_metrics_strandsite <- release_metrics %>% group_by(strandsiteATOS) %>% summarise(n = n())

release_metrics_strandsite_exp <- release_metrics_strandsite %>% tidyr::expand(strandsiteATOS=180:925) %>% ungroup()
release_metrics_strandsite_exp <- left_join(release_metrics_strandsite_exp, release_metrics_strandsite, by = "strandsiteATOS")

strandATOS_plot <- ggplot(release_metrics_strandsite_exp)+ 
  geom_bar(aes(x= as.factor(strandsiteATOS), y = n), stat = "identity")+
  themeo 


release_metrics_relsite <- release_metrics %>% group_by(relsiteATOS) %>% summarise(n = n())

release_metrics_relsite_exp <- release_metrics_relsite %>% tidyr::expand(relsiteATOS=180:925) %>% ungroup()
release_metrics_relsite_exp <- left_join(release_metrics_relsite_exp, release_metrics_relsite, by = "relsiteATOS")
release_metrics_relsite_exp[is.na(release_metrics_relsite_exp)] <- 0

relATOS_plot <- ggplot(release_metrics_relsite_exp)+ 
  geom_bar(aes(x= as.factor(relsiteATOS), y = log(n)), stat = "identity")+
  themeo 

strand_rel_ATOS_plot <- ggplot()+ 
  geom_bar(data = release_metrics_relsite_exp, aes(x= as.factor(relsiteATOS), y = n), stat = "identity", color = "blue")+
  geom_bar(data = release_metrics_strandsite_exp, aes(x= as.factor(strandsiteATOS), y = n), stat = "identity", color = "red")+
  themeo 


surrogates <- read.csv(here::here("data/ottersurrogates.csv"), stringsAsFactors = FALSE)
colnames(surrogates)[colnames(surrogates)=="Sea.Otter"] <- "seaotter"

surrogates_short <- surrogates %>% dplyr::select("seaotter", "surrogate")
surrogates_short$seaotter <- as.character(surrogates_short$seaotter)

release_metrics <- left_join(release_metrics, surrogates_short, by = "seaotter" )
release_metrics$outcome <- "success"

release_metrics_surrogates <- release_metrics %>% group_by(surrogate) %>% summarise(n=n()) %>%
  mutate(surrogate = fct_reorder(surrogate, n))

# 
#  c("Abby"="AB", "Gidget"="GD", "Ivy"="IV", "Joy"="JY", "Kit"="KT", "Mae"="MA", "Maggie"="MG", 
#                                       "Rosa"="RS", "Selka"="SL", "Toola"="TL"))


mom_plot <- ggplot(release_metrics_surrogates)+ 
  geom_bar(aes(x= as.factor(surrogate), y = n), stat = "identity", fill = "#420101")+
  xlab("")+
  ylab("# pups")+
  coord_flip(expand = 0)+
  themeo 


surrogacy_plot <- ggplot(release_metrics)+
  geom_segment(aes(x=strandage_d, xend = relage_d, y = fct_rev(seaotter), yend = fct_rev(seaotter)))+
 # geom_point(aes(x = strandage_d, y = fct_rev(seaotter)), color = "#9c9592", size = 2)+
 # geom_point(aes(x = relage_d, y = fct_rev(seaotter)), color = "#60003a", size = 2)+
  geom_point(aes(x = strandage_d, y = fct_rev(seaotter), color = sex), alpha = 0.5, size = 2, show.legend = FALSE)+
  geom_point(aes(x = relage_d, y = fct_rev(seaotter), color = sex), alpha = 1, size = 2, show.legend = FALSE)+
  geom_text(aes(x = 620, y = fct_rev(seaotter), label = surrogate), size = 2)+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  xlab("age (# days old)")+
  ylab("sea otter ID")+
  themeo 


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

success_year_plot <- ggplot(yearly_success_summary)+ 
  geom_bar(aes(x= as.factor(year), y = outcome_summary), stat = "identity")+
  xlab(" ")+
  ylab("% successful releases")+
  themeo 

release_metrics_otters_year_sum$year <- release_metrics_otters_year_sum$relyear_first

release_metrics_otters_successes_year_sum <- left_join(release_metrics_otters_year_sum, yearly_success_summary, by = "year")
release_metrics_otters_successes_year_sum$prop_success <- release_metrics_otters_successes_year_sum$otters_year*release_metrics_otters_successes_year_sum$outcome_summary


releases_successes_plot <- ggplot(release_metrics_otters_successes_year_sum)+
  geom_area(aes(x=year, y = otters_year), fill = "#eda621", alpha =0.25)+
  geom_area(aes(x=year, y = prop_success), fill = "#90abc9", alpha =0.5)+
  geom_area(data = relnum_mean_recaps_only_sum, aes(x= relyear_first, y = recap_ind), fill = "grey", alpha =0.75)+
  coord_cartesian(xlim= NULL, ylim= c(0,4.5), expand= 0)+
  xlab("")+
  ylab("count")+
  themeo 


releases_successes_plot_bar <- ggplot(release_metrics_otters_successes_year_sum)+
  geom_bar(aes(x=as.factor(year), y = otters_year), stat = "identity", fill = "#eda621", alpha =0.5)+
   geom_bar(aes(x=as.factor(year), y = prop_success),stat = "identity", fill = "#90abc9", alpha =0.5)+
   geom_bar(data = relnum_mean_recaps_only_sum, aes(x= as.factor(relyear_first), y = recap_ind), stat = "identity",fill = "grey", alpha =0.5)+
  coord_cartesian(xlim= NULL, ylim= c(0,4.5), expand= 0)+
  xlab("")+
  ylab("count")+
  themeo 


releases_successes_plot_bar2 <- ggplot(release_metrics_otters_successes_year_sum)+
  geom_bar(aes(x=as.factor(year), y = otters_year), stat = "identity", fill = "#93bbba", alpha =1)+
  geom_bar(aes(x=as.factor(year), y = prop_success),stat = "identity", fill = "#454d62", alpha =0.5)+
  geom_bar(data = relnum_mean_recaps_only_sum, aes(x= as.factor(relyear_first), y = recap_ind), stat = "identity",fill = "#2c2332", alpha =0.5)+
  coord_cartesian(xlim= NULL, ylim= c(0,4.5), expand= 0)+
  xlab("")+
  ylab("count")+
  themeo 

releases_recaps_plot <- ggplot(release_metrics_otters_successes_year_sum)+
  geom_bar(aes(x=as.factor(year), y = otters_year), stat = "identity", fill = "#93bbba", alpha =1)+
 # geom_bar(aes(x=as.factor(year), y = otters_year), stat = "identity", fill = "#90abc9", alpha =0.5)+
  geom_bar(data = relnum_mean_recaps_only_sum, aes(x= as.factor(relyear_first), y = recap_ind), stat = "identity",fill = "#2c2332", alpha =0.5)+
 # geom_bar(data = relnum_mean_recaps_only_sum, aes(x= as.factor(relyear_first), y = recap_ind), stat = "identity",fill = "#eda621", alpha =0.5)+
  coord_cartesian(xlim= NULL, ylim= c(0,4.5), expand= 0)+
  xlab("")+
  ylab("count")+
  themeo 

relnum_ind_year_plot <- ggplot(relnum_mean_recaps_only_sum)+ 
  geom_area(aes(x= relyear_first, y = recap_ind))+
  xlab(" ")+
  ylab("# individuals recaptured")+
  themeo 

#diet
# diet_228 <- read.csv(here::here("data/228feedings102002.csv"), stringsAsFactors = FALSE, skip =1)
# diet_228_short <- diet_228 %>%  dplyr::select(Date, Food.Type, Food.Item, Total.Offered...kg.)
# diet_228_short$seaotter <- as.character("228")
# 
# diet_209 <- read.csv(here::here("data/209feedings022002.csv"), stringsAsFactors = FALSE, skip =1)
# diet_209_short <- diet_228 %>%  dplyr::select(Date, Food.Type, Food.Item, Total.Offered...kg.)
# diet_209_short$seaotter <- as.character("209")
# 
# diet<- rbind(diet_209_short, diet_228_short)
# 
# diet_species_date_plot <- ggplot(diet, aes(fill=Food.Item, y=Total.Offered...kg., x=Date)) + 
#   geom_bar( stat="identity", position="fill", width = 1)+
#   themeo
# 
# 
# diet_live_date_plot <- ggplot(diet, aes(fill=Food.Type, y=Total.Offered...kg., x=Date)) + 
#   geom_bar( stat="identity", position="fill", width =1)+
#   scale_fill_manual(values = c("#90abc9","#eda621"))+
#   coord_cartesian(expand= 0)+
#   xlab("")+
#   ylab("proportion diet")+
#   themeo 
# 
# diet_live_date_plot2 <- ggplot(diet, aes(fill=Food.Type, y=Total.Offered...kg., x=Date)) + 
#   geom_bar( stat="identity", position="fill", width =1, alpha = 1)+
#   #scale_fill_manual(values = c("#fbbe02","#60003a"))+
#   scale_fill_manual(values = c("#60003a","#9c9592"))+
#   coord_cartesian(expand= 0)+
#   xlab("")+
#   ylab("proportion diet")+
#   themeo 


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

#join into release_metrics
diet_seaotter_sum_live$seaotter <- as.factor(diet_seaotter_sum_live$seaotter)
diet_seaotter_sum_live$percentlive <- diet_seaotter_sum_live$prop_foodtype_otter_sum * 100

release_metrics <- left_join(release_metrics, diet_seaotter_sum_live, by = "seaotter")


diet_seaotter_mean_live$seaotter <- as.factor(diet_seaotter_mean_live$seaotter)
diet_seaotter_mean_live$percentlive_mean <- diet_seaotter_mean_live$prop_foodtype_day_mean * 100

release_metrics <- left_join(release_metrics, diet_seaotter_mean_live, by = "seaotter")

write.csv(release_metrics, "data_output/release_metrics_51119.csv") # this version has both diet parameters

prop_live_diet_sum_plot <- ggplot(release_metrics) +
  geom_point(aes(x=fct_rev(seaotter), y = percentlive, color = sex), size =2, show.legend = FALSE)+
  coord_flip()+
  xlab("")+
  ylab("% total monthly live diet")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo2


prop_live_diet_mean_plot <- ggplot(release_metrics) +
  geom_point(aes(x=fct_rev(seaotter), y = percentlive_mean.y, color = sex), size =2, show.legend = FALSE)+
  coord_flip()+
  xlab("")+
  ylab("% mean daily live diet")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo2


grid.arrange(prop_live_diet_sum_plot, prop_live_diet_mean_plot,
             layout_matrix = rbind(c(1,2)))


ggplot(release_metrics) +
  geom_point(aes(x = percentlive_mean, y = scale_loess_pred))

# "#9c9592" "#60003a"
             
write.csv(diet, "data_output/diet.csv")
write.csv(diet_seaotter_sum_live, "data_output/diet_seaotter_sum_live.csv")
write.csv(diet_date_sum, "data_output/diet_date_sum.csv")
write.csv(diet_seaotter_mean_live, "data_output/diet_seaotter_mean_live.csv")

## arrange selected plots from above into a composite figure for fig 2
#condition_plot, strand_age_plot, rel_age_plot, days_in_plot, mom_plot, otters_year_plot, relnum_year_plot, success_year_plot ,
 grid.arrange(condition_plot_density, sex_ratio_plot, age_plot,  mom_plot, releases_successes_plot_bar, es_density, temp_plot, enso_plot, pdo_plot, prop_live_diet_sum_plot,
                         layout_matrix = rbind(c(1,1,1,2,5,5,5,5),
                                               c(1,1,1,2,5,5,5,5),
                                               c(1,1,1,2,6,6,6,6),
                                               c(3,3,4,4,7,7,7,7),
                                               c(3,3,4,4,7,7,7,7),
                                               c(3,3,4,4,8,8,8,8),
                                               c(3,3,4,4,8,8,8,8),
                                               c(10,10,10,10,9,9,9,9),
                                               c(10,10,10,10,9,9,9,9)))


 
 # another version with varied color palette
 grid.arrange(condition_plot_density, sex_ratio_plot, age_plot2,  mom_plot, releases_successes_plot_bar2, es_density, temp_plot, enso_plot, pdo_plot, prop_live_diet_sum_plot,
              layout_matrix = rbind(c(1,1,1,2,5,5,5,5),
                                    c(1,1,1,2,5,5,5,5),
                                    c(1,1,1,2,6,6,6,6),
                                    c(3,3,4,4,7,7,7,7),
                                    c(3,3,4,4,7,7,7,7),
                                    c(3,3,4,4,8,8,8,8),
                                    c(3,3,4,4,8,8,8,8),
                                    c(10,10,10,10,9,9,9,9),
                                    c(10,10,10,10,9,9,9,9)))
 
 
 #split the above into two plots
 #fig3 
 grid.arrange( temp_plot, enso_plot, pdo_plot,
              layout_matrix = rbind(c(1,1),
                                    c(2,2),
                                    c(3,3)))
 
 
 
 #new fig2
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
 
 
 
 
 
 
# ##### model fitting visualization
dist_cauchy_plotting_df <- read.csv("data_output/dist_cauchy_plotting_df.csv")

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

allcountstest_nona <- read.csv("data_output/allcountstest_nona.csv")

# plot to see if it looks ok logged
scaled_log <- ggplot(allcountstest_nona)+
  geom_segment(aes(xend = log(dist4), x = log(dist4),y = min(round(height, digits = 4)), yend = height),
               size = 4,color = "dark gray")+
  geom_line(data= dist_cauchy_plotting_df, aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.7), xlim = c(0,3.5))+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("log dispersal distance (km2)")+
  ylab("")+
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

ggplot(allcountstest_nona)+
  geom_segment(aes(xend = log(dist4), x = log(dist4),y = min(round(height, digits = 4)), yend = height, color = scale),
               size = 2)+
  geom_line(data= dist_cauchy_plotting_df, aes(x = log(dist4), y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.7), xlim = c(0,3.5), expand = 1)+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("ln dispersal distance (km2)")+
  scale_color_gradient( low = "#90abc9", high = "#eda621")+
  themeo #ok, they look good still

fig1a <- ggplot(allcountstest_nona)+
  geom_segment(aes(xend = dist4, x = dist4,y = min(round(height, digits = 4)), yend = height), color = "#93bbba",
               size = 2)+
  geom_line(data= dist_cauchy_plotting_df, aes(x = dist4, y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.75))+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("ln dispersal distance (km2)")+
  ylab("density")+
  scale_x_continuous(trans = "log10", breaks = c(0.5,1,2, 5,10, 20, 40))+
 # scale_y_continuous(limits = c(0,.75), expand = c(0,0))+
  themeo #ok, they look good still

fig1a_labels <- ggplot(allcountstest_nona)+
  geom_segment(aes(xend = dist4, x = dist4,y = min(round(height, digits = 4)), yend = height), color = "#93bbba",
               size = 2)+
  geom_line(data= dist_cauchy_plotting_df, aes(x = dist4, y = est_cauchy),stat = "identity", color = "black")+
  coord_cartesian(ylim = c(0,.75))+
  facet_wrap(~seaotter, ncol = 6)+
  xlab("ln dispersal distance (km2)")+
  ylab("density")+
  scale_x_continuous(trans = "log10", breaks = c(0.5,1,2, 5,10, 20, 40))

scale_sum <- allcountstest_nona %>% group_by(seaotter) %>% summarise(sum_scale = first(scale))

######
# make comprehensive version of release_metrics that includes max relnum as well
release_metrics <- left_join(release_metrics, relnum_mean_otter_recaps_sum, by = "seaotter")
 write.csv(release_metrics, "data_output/release_metrics.csv")                    
 
 
 dispersal_drivers <- release_metrics %>% dplyr::select(seaotter, scale, sex, relyear, relmonth, relsiteATOS, relage_d, strandage_d, strandsiteATOS, 
                                                        rel_date_real, mean_temp, condition, days_in, relyear_first.y, surrogate, outcome, max_relnum)
 
 colnames(dispersal_drivers)[colnames(dispersal_drivers)=="mean_temp"] <- "mean_daily_ehs_sst"
 colnames(dispersal_drivers)[colnames(dispersal_drivers)=="relyear_first.y"] <- "relyear_first"
 
 write.csv(dispersal_drivers, "data_output/dispersal_drivers.csv")
 
 
 ##########maps
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
 
 resights_daysout <- read.csv(here::here("data/release_resights.csv"), stringsAsFactors = FALSE)
 
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
 
 
 
 ##### read LEAST COST PATH data back in (can run this only instead of regenerating code after first time)
 testmap_path2_recap <- readRDS( "data_output/testmap_path2_recap.rds") # paths (lat/lon) of lcp 
 testmap_dist2_recap <- readRDS( "data_output/testmap_dist2_recap.rds") # distance calculations (km) from lcp
 test_dist_diag_recap <- readRDS("data_output/test_dist_diag_recap.rds")  # lcp distance including diagonal values AKA resights
 otter_resight_dist <- readRDS("data_output/otter_resight_dist.rds") # tibble of seaotter and diagonal values without NA
 test_dist_diag_recap_index <- readRDS("data_output/rtest_dist_diag_recap_index.rds") # includes indexing values for diagonals/resights to use in indexing only resight paths 
 lcp_df_unnest <- read.csv("data_output/lcp_df_unnest.csv") # lat/lon dataframe of lcp PATHS for mapping in ggplot
 
 
 
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
 
 otter809 <- subset(lcp_df_unnest, seaotter == "809")
 otter379 <- subset(lcp_df_unnest, seaotter == "379")
 otter526 <- subset(lcp_df_unnest, seaotter == "526")
 otter353 <- subset(lcp_df_unnest, seaotter == "353")
 otter286 <- subset(lcp_df_unnest, seaotter == "286")
 
 release_site_short <- subset(release_site, seaotter == c("228","379","587","716"))
 release_site_short2 <- subset(release_site, seaotter == c("809","379","526"))
 
 ##### fig1b depr
 ggplot()+
   geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
   geom_path(data = otter228, aes(x=long, y = lat), color = "#1b9e77")+
   geom_path(data = otter379, aes(x=long, y = lat), color = "#d95f02")+
   geom_path(data = otter716, aes(x=long, y = lat), color = "#7570b3")+
   geom_path(data = otter587, aes(x=long, y = lat), color = "#e7298a")+
   geom_sf(data = release_site_short, color = "black") + #release locations
   coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site_short))+ #study site
   themeo  
 
 
 ##### fig1b
 ggplot()+
   geom_contour(data = bathy_df, aes(x=x, y=y,z=GEBCO2014_.123.9342_35.1272_.120.655_38.482_30Sec_Geotiff), breaks = c( 0, -50, -100, -250,  -500, -1000, -2000), color = "grey")+ #bathy contours
   geom_path(data = otter228, aes(x=long, y = lat), color = "#1b9e77")+
   geom_path(data = otter353, aes(x=long, y = lat), color = "#d95f02")+
   geom_path(data = otter526, aes(x=long, y = lat), color = "#e7298a")+
   geom_sf(data = release_site_short2, color = "black") + #release locations
   coord_sf(xlim = c(-122.4, -121.7),ylim = c(36.4,37.1), crs = st_crs(release_site_short))+ #study site
   themeo  
 