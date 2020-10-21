# calculate daily water temp Elkhorn Slough

library(here)
library(tidyverse)
library(lubridate)
library(gridExtra)

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

# here::here() # test location #"C:/Users/SBecker/Local-Git/otter_dispersal"

ehs_temp <- read.csv("C:/Users/SBecker/Local-Git/otter_dispersal/data/EHS_Temp/ehs_raw_temp.csv") #read in temp data
head(ehs_temp) # check col names
ehs_temp <- ehs_temp %>% dplyr::select(DateTimeStamp, Temp) # select datetime and temp

ehs_temp$DateTimeStamp <- mdy_hm(ehs_temp$DateTimeStamp) # make date a date (POSIXct) format
ehs_temp$date <- date(ehs_temp$DateTimeStamp) # create a date variable from datetime

ehd_temp_daily_mean <- ehs_temp %>% group_by(date) %>% summarise(mean_temp = mean(Temp)) # mean daily temp
ehd_temp_daily_mean_nona <- ehd_temp_daily_mean %>% fill(mean_temp) # fill NA with previous value (?)

temp_plot <- ggplot(ehd_temp_daily_mean_nona, aes(x = date, y = mean_temp))+
  geom_path()+
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
  themeo

grid.arrange(temp_plot, enso_plot,
             layout_matrix = rbind(c(1),
                                   c(2)))
