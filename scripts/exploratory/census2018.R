
library(sf)
library(rgdal)

census2018 <- readOGR(dsn = "C:/Users/SBecker/Local-Git/otter_dispersal/data/census2018", layer = "Census_sum_2018")
glimpse(census2018)


 plot(census2018)
 
 
 census2018.df <- as.data.frame(census2018)
 EHS_2018 <- census2018.df %>% dplyr::filter(ATOS_ID == "321")
 
 
 35.1133461 + 0.9542579 + 1.9772683
 
 
 
 census2017 <- readOGR(dsn = "C:/Users/SBecker/Local-Git/otter_dispersal/data/census2017/Census summary of southern sea otter 2017.shp")
 glimpse(census2017)
 

 plot(census2017)
 
 
 census2017.df <- as.data.frame(census2017)
 EHS_2017 <- census2017.df %>% dplyr::filter(ATOS_ID == "321")
 