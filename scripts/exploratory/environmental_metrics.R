library(tidyverse)
library(broom)


##### example code
# # original graph with smoother
# ggplot(data=mtcars, aes(hp,wt)) + 
#   stat_smooth(method = "loess", span = 0.7)
# 
# # Create model that will do the same thing as under the hood in ggplot2
# model <- loess(wt ~ hp, data = mtcars, span = 0.7)
# 
# # Add predicted values from model to original dataset using broom library
# mtcars2 <- augment(model, mtcars)
# 
# # Plot both lines 
# ggplot(data=mtcars2, aes(hp,wt)) + 
#   geom_line(aes(hp, .fitted), color = "red") +
#   stat_smooth(method = "loess", span = 0.7)


#### Read in environmental data
################################## SST and ENSO
ehs_temp <- read.csv("C:/Users/SBecker/Local-Git/otter_dispersal/data/EHS_Temp/ehs_raw_temp.csv") #read in temp data
head(ehs_temp) # check col names
ehs_temp <- ehs_temp %>% dplyr::select(DateTimeStamp, Temp) # select datetime and temp

ehs_temp$DateTimeStamp <- mdy_hm(ehs_temp$DateTimeStamp) # make date a date (POSIXct) format
ehs_temp$date <- date(ehs_temp$DateTimeStamp) # create a date variable from datetime

#plot(ehs_temp)

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



# calculate some moving averages ehd_temp_daily_mean_nona

library(zoo)
ehd_temp_daily_mean_nona <- ehd_temp_daily_mean_nona %>% 
  mutate(temp_ma_week = rollmean(x = mean_temp, 7, align = "right" , fill = NA),
         temp_ma_month = rollmean(x = mean_temp, 30, align = "right" , fill = NA),
         temp_ma_2month = rollmean(x = mean_temp, 60, align = "right" , fill = NA),
         temp_ma_3month = rollmean(x = mean_temp, 90, align = "right" , fill = NA),
         temp_ma_year = rollmean(x = mean_temp, 365, align = "right" , fill = NA)) 

ggplot()+
  geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = mean_temp), color = "black")+
  geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = temp_ma_week), color = "blue")+
   geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = temp_ma_month), color = "purple")+
  geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = temp_ma_2month), color = "green")+
  geom_path(data = ehd_temp_daily_mean_nona, aes(x = date, y = temp_ma_year), color = "red")+
  stat_smooth(data = ehd_temp_daily_mean_nona, aes(x = date, y = mean_temp), method = "loess", span = 0.5)+
 # geom_point(data = release_metrics_out, aes (x = date, y = temp_ma_week.x, color = scale), size = 3)+
 #scale_color_viridis()+
  themeo

ehd_temp_daily_mean_nona_simple <- ehd_temp_daily_mean_nona %>% dplyr::select(date, year, mean_temp)
ehd_temp_daily_mean_nona_simple$ID <- seq.int(nrow(ehd_temp_daily_mean_nona_simple))

###### extract loess values
### SST
#Create model that will do the same thing as under the hood in ggplot2
model <- loess(mean_temp ~ ID, data = ehd_temp_daily_mean_nona_simple, span = 0.5)

# Add predicted values from model to original dataset using broom library
ehd_temp_daily_mean_nona_simple2 <- augment(model, ehd_temp_daily_mean_nona_simple)

# Plot both lines
ggplot(data=ehd_temp_daily_mean_nona_simple2, aes(date,mean_temp)) +
  geom_line(aes(date, mean_temp), color = "black")+
  geom_line(aes(date, .fitted), color = "red") +
  stat_smooth(method = "loess", span = 0.5) +
  themeo

ehs_loess_fitted <- ehd_temp_daily_mean_nona_simple2 %>% dplyr::select(date, .fitted, .se.fit, .resid)

# bring drivers dataset in and join values
dispersal_drivers <- read.csv(here::here("data_output/dispersal_drivers.csv"), stringsAsFactors = FALSE)
dispersal_drivers$seaotter <- as.character(dispersal_drivers$seaotter)
dispersal_drivers$date <- ymd(dispersal_drivers$rel_date_real)

dispersal_drivers <- left_join(dispersal_drivers, ehs_loess_fitted, by = "date")


colnames(dispersal_drivers)[colnames(dispersal_drivers)==".fitted"] <- "sst_loess_pred"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".se.fit"] <- "sst_loess_se"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".resid"] <- "sst_loess_resid"

plot(dispersal_drivers$sst_loess_pred, dispersal_drivers$scale)

### PDO
PDO_2002_2017 <- PDO_2002_2017 %>% arrange(Date)
PDO_2002_2017$ID <- seq.int(nrow(PDO_2002_2017))
model_pdo <- loess(rank ~ ID, data = PDO_2002_2017, span = 0.6)

# Add predicted values from model to original dataset using broom library
PDO_2002_2017_2 <- augment(model_pdo, PDO_2002_2017)

# Plot both lines
ggplot(data=PDO_2002_2017_2, aes(Date,rank)) +
  geom_line(aes(Date,rank), color = "black")+
  geom_line(aes(Date, .fitted), color = "red") +
  stat_smooth(method = "loess", span = 0.6) +
  themeo

PDO_loess_fitted <- PDO_2002_2017_2 %>% dplyr::select(Date, .fitted, .se.fit, .resid)
PDO_loess_fitted$relmonth <- month(PDO_loess_fitted$date)
PDO_loess_fitted$relyear <- year(PDO_loess_fitted$date)
colnames(PDO_loess_fitted)[colnames(PDO_loess_fitted)=="Date"] <- "date"

# add in loess pdo
dispersal_drivers <- left_join(dispersal_drivers, PDO_loess_fitted, by = c("relmonth", "relyear"))

#change colnames and add in raw pdo
PDO_2002_2017$relmonth <- lubridate::month(PDO_2002_2017$Date)
PDO_2002_2017$relyear <- lubridate::year(PDO_2002_2017$Date)
dispersal_drivers$year <- dispersal_drivers$relyear
dispersal_drivers$month <- dispersal_drivers$relmonth 

PDO_2002_2017_short <- PDO_2002_2017 %>% dplyr::select(rank, relmonth, relyear)
dispersal_drivers <- left_join(dispersal_drivers, PDO_2002_2017_short, by = c("relmonth", "relyear")) 

colnames(dispersal_drivers)[colnames(dispersal_drivers)=="rank"] <- "pdo_index"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".fitted"] <- "pdo_loess_pred"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".se.fit"] <- "pdo_loess_se"
colnames(dispersal_drivers)[colnames(dispersal_drivers)==".resid"] <- "pdo_loess_resid"

plot(dispersal_drivers$pdo_loess_pred, dispersal_drivers$scale)


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
  geom_line(aes(date, .fitted), color = "red") +
  stat_smooth(method = "loess", span = 0.6) +
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



plot(dispersal_drivers$enso_loess_pred, dispersal_drivers$scale)

### scale loess
ggplot(release_metrics, aes(x = rel_date_real, y = scale))+
  geom_smooth(color = "darkgrey")+
  #geom_line()+
  geom_point()+
  #geom_point(aes(color = scale),size = 3)+
  xlab("release date")+
  ylab("scale")+
  # scale_color_gradient( low = "blue4", high = "gold")+
  coord_cartesian(xlim = NULL, ylim = c(-.25,6.25), expand =0)+
  themeo



#Create model that will do the same thing as under the hood in ggplot2
model_scale <- loess(scale ~ X, data = dispersal_drivers, span = 0.6)

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


ehs_loess_fitted <- ehd_temp_daily_mean_nona_simple2 %>% dplyr::select(date, .fitted, .se.fit, .resid)


#### population density in ehs
#elkhorn slough population density for only survey years
es_pop_dens <- read.csv(here::here("data/censusES1985_2017.csv"), stringsAsFactors = FALSE)
density_2002_2017 <- es_pop_dens %>% dplyr::filter(year >= 2002)

#fill 2011 na value
density_2002_2017_nona <- density_2002_2017 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))
#not sure this is great

#create a df for just elkhorn slough otters
dispersal_drivers_EHS <- dispersal_drivers %>% dplyr::filter(relsiteATOS == "321")
dispersal_drivers_EHS$year <- dispersal_drivers_EHS$relyear

dispersal_drivers_EHS <- left_join(dispersal_drivers_EHS, density_2002_2017_nona, by = "year")

#drop unneeded columns created by merge

colnames(dispersal_drivers)
# [1] "X"                  "seaotter"           "scale"              "sex"                "relyear"            "relmonth"          
# [7] "relsiteATOS"        "relage_d"           "strandage_d"        "strandsiteATOS"     "rel_date_real"      "mean_daily_ehs_sst"
# [13] "condition"          "days_in"            "relyear_first"      "surrogate"          "outcome"            "max_relnum"        
# [19] "date.x"             "sst_loess_pred"     "sst_loess_se"       "sst_loess_resid"    "date.y"             "pdo_loess_pred"    
# [25] "pdo_loess_se"       "pdo_loess_resid"    "date"               "enso_loess_pred"    "enso_loess_se"      "enso_loess_resid"  

dispersal_drivers[,c("date.x","date.y","date")] <- list(NULL)
colnames(dispersal_drivers)


colnames(dispersal_drivers_EHS)
# [1] "X"                  "seaotter"           "scale"              "sex"                "relyear"            "relmonth"          
# [7] "relsiteATOS"        "relage_d"           "strandage_d"        "strandsiteATOS"     "rel_date_real"      "mean_daily_ehs_sst"
# [13] "condition"          "days_in"            "relyear_first"      "surrogate"          "outcome"            "max_relnum"        
# [19] "date.x"             "sst_loess_pred"     "sst_loess_se"       "sst_loess_resid"    "date.y"             "pdo_loess_pred"    
# [25] "pdo_loess_se"       "pdo_loess_resid"    "date"               "enso_loess_pred"    "enso_loess_se"      "enso_loess_resid"  
# [31] "year"               "dens.sm"            "pupratio"           "X5yrtrend"    

dispersal_drivers_EHS[,c("date.x","date.y","date", "year")] <- list(NULL)
colnames(dispersal_drivers_EHS)


write.csv(dispersal_drivers_EHS, "data_output/dispersal_drivers_EHS.csv")
write.csv(dispersal_drivers, "data_output/dispersal_drivers.csv")


#### look at correlations of variables
library(corrplot)

str(dispersal_drivers)
dispersal_drivers_num <- dispersal_drivers %>% dplyr::select(scale, relyear, relmonth, relsiteATOS, relage_d, strandage_d, strandsiteATOS, mean_daily_ehs_sst, mei_rank, pdo_index,
                                                             condition, days_in, max_relnum, sst_loess_pred, pdo_loess_pred, enso_loess_pred, scale_loess_pred)

dispersal_drivers_num <- na.omit(dispersal_drivers_num)

m <- cor(dispersal_drivers_num)
corrplot(m)

dispersal_drivers_EHS_num <- dispersal_drivers_EHS %>% dplyr::select(scale, relyear, relmonth, relage_d, strandage_d, strandsiteATOS, mean_daily_ehs_sst,
                                                             condition, days_in, max_relnum, sst_loess_pred, pdo_loess_pred, enso_loess_pred, scale_loess_pred, mei_rank, pdo_index,dens.sm, X5yrtrend)

dispersal_drivers_EHS_num <- na.omit(dispersal_drivers_EHS_num)
m_ehs <- cor(dispersal_drivers_EHS_num)
corrplot(m_ehs)


dispersal_drivers %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
 # geom_density()        
  geom_histogram()


#rf exploration
library(randomForest)

dispersal_drivers_nona <- na.omit(dispersal_drivers)

scale_rf_global <- randomForest(scale ~  relmonth + relage_d   +  
  mean_daily_ehs_sst + pdo_loess_pred +
   X5yrtrend, data = dispersal_drivers_EHS_num, importance = TRUE)


dispersal_drivers_nona$sex <- as.factor(dispersal_drivers_nona$sex)
dispersal_drivers_nona$surrogate <- as.factor(dispersal_drivers_nona$surrogate)

scale_rf_global <- randomForest(scale ~ sex  + relmonth + relage_d   +  
                                  mean_daily_ehs_sst + sst_loess_pred , data = dispersal_drivers_nona, importance = TRUE)

dispersal_drivers_EHS_nona <- na.omit(dispersal_drivers_EHS)
dispersal_drivers_EHS_nona$sex <- as.factor(dispersal_drivers_EHS_nona$sex)
dispersal_drivers_EHS_nona$surrogate <- as.factor(dispersal_drivers_EHS_nona$surrogate)

scale_rf_global <- randomForest(scale ~ sex  + relmonth + relage_d   +  
                                  mean_daily_ehs_sst + pdo_loess_pred +
                                  X5yrtrend, data = dispersal_drivers_EHS_nona, importance = TRUE)

scale_rf_global <- randomForest(scale ~ sex  + relyear + relmonth + relage_d   +  relsiteATOS + strandsiteATOS +
                                  mean_daily_ehs_sst + condition + days_in + surrogate + max_relnum + sst_loess_pred +pdo_loess_pred +
                                  enso_loess_pred + dens.sm + X5yrtrend, data = dispersal_drivers_EHS_nona, importance = TRUE)
