# packages
library(tidyverse)

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

################### Local otter population density
#elkhorn slough population density for only survey years
es_pop_dens <- read.csv(here::here("data/censusES1985_2018.csv"), stringsAsFactors = FALSE)


density_2002_2018 <- es_pop_dens %>% dplyr::filter(year >= 2002) # select only years in our survey

#fill 2011 na value
#density_2002_2018_nona <- density_2002_2018 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))
library(imputeTS)
density_2002_2018<- na_ma(density_2002_2018, k = 1)

#plot yearly es density
 ggplot(density_2002_2018, aes(x = year, y = dens.sm))+ # could also look at X5yrtrend (5 year density trend)
  geom_area(fill = "#93bbba")+
  coord_cartesian(expand = 0)+
  xlab("")+
  ylab("population density")+
  scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_themeo()


ggplot(density_2002_2018, aes(x = year, y = dens.sm))+ # could also look at X5yrtrend (5 year density trend)
  #geom_area(fill = "#93bbba")+
  geom_point(color = "#466867")+
  geom_line(color = "#466867")+
  coord_cartesian(expand = 0)+
  xlab("")+
  ylab("population density")+
  scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_themeo()


# 5 yer trend
ggplot(density_2002_2018, aes(x = year, y = X5yrtrend))+ # could also look at X5yrtrend (5 year density trend)
  #geom_area(fill = "#93bbba")+
  geom_point(color = "#466867")+
  geom_line(color = "#466867")+
  coord_cartesian(expand = 0)+
  xlab("")+
  ylab("population growth")+
  scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))+
  theme_themeo()
