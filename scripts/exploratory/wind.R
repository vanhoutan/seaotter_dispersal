######################
### wind data


#read in required packages
require(readxl)
require(tidyverse)
require(here)
library(data.table)
library(lubridate)
library(broom)


##### add plotting theme
theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          # axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          # axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title=element_blank(),
          strip.text=element_text(hjust=0) )}


############# read in wind data
#create a list of the files from your target directory
file_list1 <- list.files(path=here::here("/data/wind/46042/2002_2004"), full.names = T)
file_list2 <- list.files(path=here::here("/data/wind/46042/2005_2018"), full.names = T)

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
wind1 <- data.frame()

#specify needed columns
for (i in 1:length(file_list1)){
  temp_data <- fread(file_list1[i], stringsAsFactors = F,select = c(1:10)) #read in files using the fread function from the data.table package
  wind1 <- rbindlist(list(wind1, temp_data)) #for each iteration, bind the new data to the building dataset
}

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
wind2 <- data.frame()

#specify needed columns
for (i in 1:length(file_list2)){
  temp_data <- fread(file_list2[i], stringsAsFactors = F,select = c(1:10)) #read in files using the fread function from the data.table package
  wind2 <- rbindlist(list(wind2, temp_data)) #for each iteration, bind the new data to the building dataset
}


str(wind1)
str(wind2)

wind2 <- wind2 %>% dplyr::select("YYYY", "MM", "DD", "hh", "WD", "WSPD", "WVHT")
wind1 <- wind1 %>% dplyr::select("YYYY", "MM", "DD", "hh", "WD", "WSPD", "WVHT")

wind <- rbind(wind1, wind2)

############## look at patterns in wind data
# first, give date an actual date meaning: paste together datetime into one date object
wind <- wind %>% mutate(date = paste(YYYY, MM, DD, sep = "-"), time = (hh)) %>% 
  mutate(datetime = paste(date, time, sep = " ")) %>% mutate(datetime_real = ymd_h(datetime)) %>% arrange(datetime_real)
  
wind$WD <- as.numeric(wind$WD)
wind$WVHT <- as.numeric(wind$WVHT)
wind$WSPD <- as.numeric(wind$WSPD)

wind <- wind %>%  arrange (datetime_real)
str(wind)

wind$WSPD[wind$WSPD >25] <- NA
wind$WVHT[wind$WVHT >50] <- NA

ggplot(wind)+
  geom_path(aes(x = datetime_real, y = WVHT))

ggplot(wind)+
  geom_path(aes(x = datetime_real, y = WSPD))

#next steps: smooth time series, extract values, extract predicted values for release date 
# (or, moving average following release), look at in relation to scale, scale loess, add into models, 
# compare models with and without, add to visualizations if it seems to make a difference.

wind_dailymean <- wind %>% group_by(date) %>% mutate (med_d_wspd = median(WSPD), med_d_wvht = median(WVHT)) %>% 
  summarise(datetime = first(datetime), med_d_wspd = first(med_d_wspd), med_d_wvht = first(med_d_wvht))

wind_dailymean$date_rel <- ymd_h(wind_dailymean$datetime)
wind_dailymean <- wind_dailymean %>% arrange(date_rel)

ggplot(wind_dailymean)+
  geom_path(aes(x = date_rel, y = med_d_wspd))

ggplot(wind_dailymean)+
  geom_path(aes(x = date_rel, y = med_d_wvht))

## bring environmental variables into dispersal drivers dataframe
################### extract raw and modelled variables for wvht, wspd
### wind
wind_dailymean_simple <- wind_dailymean %>% dplyr::select(date_rel, datetime, date,  med_d_wspd, med_d_wvht)
wind_dailymean_simple$ID <- seq.int(nrow(wind_dailymean_simple))

###### extract loess values
#Create model that will do the same thing as under the hood in ggplot2
model_wspd <- loess(med_d_wspd ~ ID, data = wind_dailymean_simple, span = 0.05)

# expand to all possible dates
#wind_dailymean_simple <- complete(wind_dailymean_simple, date_rel = full_seq(date_rel, 1))

# Add predicted values from model_wspd to original dataset using broom library
wind_dailymean_simple <- augment(model_wspd, wind_dailymean_simple)

colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".fitted"] <- "wspd_loess_pred"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".se.fit"] <- "wspd_loess_se"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".resid"] <- "wspd_loess_resid"

# Plot both lines to test it is working
ggplot(wind_dailymean_simple) +
  geom_line(aes(date_rel, med_d_wspd), color = "black")+
  #stat_smooth(method = "loess", span = 0.5) +
  geom_line(aes(date_rel, wspd_loess_pred), color = "red") +
  theme_classic()

## now waveheight
model_wvht<- loess(med_d_wvht ~ ID, data = wind_dailymean_simple, span = 0.05)
# Add predicted values from model_wspd to original dataset using broom library
wind_dailymean_simple <- augment(model_wvht, wind_dailymean_simple)

colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".fitted"] <- "wvht_loess_pred"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".se.fit"] <- "wvht_loess_se"
colnames(wind_dailymean_simple)[colnames(wind_dailymean_simple)==".resid"] <- "wvht_loess_resid"


# Plot both lines to test it is working
ggplot(wind_dailymean_simple) +
  geom_line(aes(date_rel, med_d_wvht), color = "black")+
  #stat_smooth(method = "loess", span = 0.5) +
  geom_line(aes(date_rel, wvht_loess_pred), color = "red") +
  theme_classic()


# read in dispersal drivers
dd <- read.csv(here::here("data_output/dispersal_drivers.csv"))

wind_dailymean_simple_fitted <- wind_dailymean_simple %>% dplyr::select(date_rel, date,
                                      med_d_wspd, wspd_loess_pred, wspd_loess_se, wspd_loess_resid,
                                      med_d_wvht, wvht_loess_pred, wvht_loess_se, wvht_loess_resid )

dd$date_rel <- dd$rel_date_real
dd$date_rel <- ymd(dd$date_rel)
wind_dailymean_simple_fitted$date_rel <- ymd(wind_dailymean_simple_fitted$date)

dd <- left_join(dd, wind_dailymean_simple_fitted, by = "date_rel")

ggplot(dd)+
  geom_point(aes(x = date_rel, y = wspd_loess_pred))+
  theme_themeo()
ggplot(dd)+
  geom_point(aes(x = date_rel, y = wvht_loess_pred))+
  theme_themeo()

ggplot(dd)+
  geom_point(aes(x = wspd_loess_pred, y = scale))+
  theme_themeo()
ggplot(dd)+
  geom_point(aes(x = wvht_loess_pred, y = scale))+
  theme_themeo()

ggplot(dd)+
  geom_point(aes(x = wspd_loess_pred, y = scale_loess_pred))+
  theme_themeo()
ggplot(dd)+
  geom_point(aes(x = wvht_loess_pred, y = scale_loess_pred))+
  theme_themeo()

### experiment with filling NAs
# Plot both lines to test it is working
ggplot() +
  geom_line(data = wind_dailymean_simple_fitted, aes(date_rel, med_d_wvht), color = "black")+
  #stat_smooth(method = "loess", span = 0.5) +
  geom_line(data = wind_dailymean_simple_fitted, aes(date_rel, wvht_loess_pred), color = "red") +
  geom_point(data = dd, aes(x = date_rel, y =wvht_loess_pred ), color = "blue")+
  theme_themeo()

dd_fillna <- dd %>% fill(wvht_loess_pred, wspd_loess_pred)

ggplot() +
  geom_line(data = wind_dailymean_simple_fitted, aes(date_rel, med_d_wvht), color = "black")+
  #stat_smooth(method = "loess", span = 0.5) +
  geom_line(data = wind_dailymean_simple_fitted, aes(date_rel, wvht_loess_pred), color = "red") +
  geom_point(data = dd_fillna, aes(x = date_rel, y =wvht_loess_pred ), color = "blue")+
  theme_themeo()


ggplot(dd)+
  geom_point(aes(x = date_rel, y = wspd_loess_pred))+
  geom_smooth(aes(x = date_rel, y = wspd_loess_pred))+
  theme_themeo()
ggplot(dd)+
  geom_point(aes(x = date_rel, y = wvht_loess_pred))+
  geom_smooth(aes(x = date_rel, y = wvht_loess_pred))+
  theme_themeo()

ggplot(dd)+
  geom_point(aes(x = wspd_loess_pred, y = scale))+
  geom_smooth(aes(x = wspd_loess_pred, y = scale))+
  theme_themeo()
ggplot(dd)+
  geom_point(aes(x = wvht_loess_pred, y = scale))+
  geom_smooth(aes(x = wvht_loess_pred, y = scale))+
  theme_themeo()

ggplot(dd)+
  geom_point(aes(x = wspd_loess_pred, y = scale_loess_pred))+
  geom_smooth(aes(x = wspd_loess_pred, y = scale_loess_pred))+
  theme_themeo()
ggplot(dd)+
  geom_point(aes(x = wvht_loess_pred, y = scale_loess_pred))+
  geom_smooth(aes(x = wvht_loess_pred, y = scale_loess_pred))+
  theme_themeo()




# add into RF
##############################################
es_pop_dens <- read.csv(here::here("data/censusES1985_2017.csv"), stringsAsFactors = FALSE)
density_2002_2017 <- es_pop_dens %>% dplyr::filter(year >= 2002) # select only years in our survey

#fill 2011 na value
density_2002_2017_nona <- density_2002_2017 %>% mutate(dens.sm = replace(dens.sm, is.na(dens.sm), mean(dens.sm, na.rm = TRUE)))

#drop  2016
density_2002_2017_nona <- density_2002_2017_nona[-15,]

### density drivers with just EHS
dd_EHS <- dd %>% dplyr::filter(relsiteATOS == "321")
dd_EHS$year <- dd_EHS$relyear

dd_EHS <- left_join(dd_EHS, density_2002_2017_nona, by = "year")

#drop NA rows
dd_EHS_nona <- na.omit(dd_EHS)
dd_EHS_nona$sex <- as.factor(dd_EHS_nona$sex)
dd_EHS_nona$surrogate <- as.factor(dd_EHS_nona$surrogate)

new_dd_EHS_nona <- data.frame(dd_EHS_nona, id=1+c(1:nrow(dd_EHS_nona))%%20)
id <- unique(new_dd_EHS_nona$id)

new_dd_EHS_nona$seaotter <- as.factor(new_dd_EHS_nona$seaotter)
new_dd_EHS_nona$sex <- as.factor(new_dd_EHS_nona$sex)
new_dd_EHS_nona$surrogate <- as.factor(new_dd_EHS_nona$surrogate)
new_dd_EHS_nona$outcome <- as.factor(new_dd_EHS_nona$outcome)


######### POS Non-Correlated VARIABLES and LOESS SCALE AS RESPONSE
library(randomForest)
loo_ls_4v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex +  relage_d +
                       +percentlive + 
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_4v <- cbind(loo_ls_4v, imp)
}

str(loo_ls_4v)
imp_df_loo_ls_4v <- as.data.frame(t(loo_ls_4v))

str(imp_df_loo_ls_4v)
imp_incMSE_ls_4v <- imp_df_loo_ls_4v[seq(1, nrow(imp_df_loo_ls_4v),2),]
imp_IncNodePurity_ls_4v <- imp_df_loo_ls_4v[seq(2, nrow(imp_df_loo_ls_4v),2),]

imp_rank_ls_4v <- t(apply(imp_IncNodePurity_ls_4v, 1, rank, ties.method = "min"))
str(imp_rank_ls_4v)
head(imp_rank_ls_4v)

colMeans(imp_rank_ls_4v)
colMeans(imp_incMSE_ls_4v)

colMeans(imp_IncNodePurity_ls_4v)

ls_4v_global <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  
                               +percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_4v <- as.data.frame(colMeans(imp_incMSE_ls_4v))
names(imp_summary_ls_4v)[1] <- "PercentIncMSE"
imp_summary_ls_4v$variable <- c("sex",  "relage_d",  
                                
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_4v)
imp_summary_ls_4v$variable <- as.factor(imp_summary_ls_4v$variable)

imp_summary_ls_4v <- imp_summary_ls_4v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_4v <- ggplot(imp_summary_ls_4v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_4v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  theme_themeo()

#### error calculations
### R squared

r_squared_test_ls_4v <- NULL
r_squared_train_ls_4v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_4v <- cbind(r_squared_test_ls_4v, r2_test)  
  r_squared_train_ls_4v <- cbind(r_squared_train_ls_4v, r2_train)  
}

mean(r_squared_test_ls_4v)
mean(r_squared_train_ls_4v)
# > mean(r_squared_test_ls_3v)
# [1] 0.4622875
# > mean(r_squared_train_ls_3v)
# [1] 0.8640189



#### now run with wind and wave height
loo_ls_5v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex +  relage_d +
                       +percentlive + wspd_loess_pred + wvht_loess_pred +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_5v <- cbind(loo_ls_5v, imp)
}

str(loo_ls_5v)
imp_df_loo_ls_5v <- as.data.frame(t(loo_ls_5v))

str(imp_df_loo_ls_5v)
imp_incMSE_ls_5v <- imp_df_loo_ls_5v[seq(1, nrow(imp_df_loo_ls_5v),2),]
imp_IncNodePurity_ls_5v <- imp_df_loo_ls_5v[seq(2, nrow(imp_df_loo_ls_5v),2),]

imp_rank_ls_5v <- t(apply(imp_IncNodePurity_ls_5v, 1, rank, ties.method = "min"))
str(imp_rank_ls_5v)
head(imp_rank_ls_5v)

colMeans(imp_rank_ls_5v)
colMeans(imp_incMSE_ls_5v)

colMeans(imp_IncNodePurity_ls_5v)

ls_5v_global <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  wspd_loess_pred + wvht_loess_pred +
                               +percentlive +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_5v <- as.data.frame(colMeans(imp_incMSE_ls_5v))
names(imp_summary_ls_5v)[1] <- "PercentIncMSE"
imp_summary_ls_5v$variable <- c("sex",  "relage_d",  
                                
                                "percent_live_diet", "wind_speed" , "wave_height" , "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_5v)
imp_summary_ls_5v$variable <- as.factor(imp_summary_ls_5v$variable)

imp_summary_ls_5v <- imp_summary_ls_5v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_5v <- ggplot(imp_summary_ls_5v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_5v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  theme_themeo()

#### error calculations
### R squared

r_squared_test_ls_5v <- NULL
r_squared_train_ls_5v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       
                       +percentlive +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_5v <- cbind(r_squared_test_ls_5v, r2_test)  
  r_squared_train_ls_5v <- cbind(r_squared_train_ls_5v, r2_train)  
}

mean(r_squared_test_ls_5v)
mean(r_squared_train_ls_5v)
# > mean(r_squared_test_ls_3v)
# [1] 0.4622875
# > mean(r_squared_train_ls_3v)
# [1] 0.8640189


# compare var importance
library(gridExtra)

grid.arrange(varimp_ls_4v, varimp_ls_5v,
             layout_matrix = rbind(c(1,2)))
