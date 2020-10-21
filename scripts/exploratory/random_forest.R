

# random forest
library(randomForest)
library(tidyverse)
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


#read in data
dispersal_drivers <- read.csv(here::here("data_output/dispersal_drivers.csv"), stringsAsFactors = FALSE)
dispersal_drivers_EHS <- read.csv(here::here("data_output/dispersal_drivers_EHS.csv"), stringsAsFactors = FALSE)
diet_seaotter_sum_live <- read.csv("data_output/diet_seaotter_sum_live.csv")


#change columns to correct format
dispersal_drivers$seaotter <- as.character(dispersal_drivers$seaotter)
dispersal_drivers$date <- ymd(dispersal_drivers$rel_date_real)

dispersal_drivers_EHS$seaotter <- as.character(dispersal_drivers_EHS$seaotter)
dispersal_drivers_EHS$date <- ymd(dispersal_drivers_EHS$rel_date_real)

diet_seaotter_sum_live$seaotter <- as.character(diet_seaotter_sum_live$seaotter)

# add in diet to both dfs
dispersal_drivers <- left_join(dispersal_drivers,diet_seaotter_sum_live, by = "seaotter" )
dispersal_drivers_EHS <- left_join(dispersal_drivers_EHS,diet_seaotter_sum_live, by = "seaotter" )

#fix duplicate columns
dispersal_drivers <- dispersal_drivers %>%  dplyr::select("X.x","seaotter", "scale" ,"sex" , "relyear" , "relmonth" ,"relsiteATOS" , "relage_d"  ,"strandage_d" ,
                                                          "strandsiteATOS" , "rel_date_real" ,  "mean_daily_ehs_sst" , "condition"  , "days_in" , "relyear_first" ,
                                                          "surrogate" ,"outcome" ,"max_relnum","sst_loess_pred","sst_loess_se" , "sst_loess_resid","pdo_loess_pred"  ,
                                                          "pdo_loess_se" ,"pdo_loess_resid"  ,"enso_loess_pred" , "enso_loess_se"  , "enso_loess_resid" ,"date" , "mei_rank",
                                                          "pdo_index","scale_loess_pred" ,"scale_loess_se" ,"scale_loess_resid" , "FoodType.y"  ,             
                                                          "prop_foodtype_otter_sum.y" ,"percentlive.y"  )

#remove NAs as it messes up rf
dispersal_drivers_nona <- na.omit(dispersal_drivers)
dispersal_drivers_nona$sex <- as.factor(dispersal_drivers_nona$sex)
dispersal_drivers_nona$surrogate <- as.factor(dispersal_drivers_nona$surrogate)

dispersal_drivers_EHS_nona <- na.omit(dispersal_drivers_EHS)
dispersal_drivers_EHS_nona$sex <- as.factor(dispersal_drivers_EHS_nona$sex)
dispersal_drivers_EHS_nona$surrogate <- as.factor(dispersal_drivers_EHS_nona$surrogate)

#or, replace NAs 
dispersal_drivers_fillna <- dispersal_drivers %>% fill(mean_daily_ehs_sst, condition, sst_loess_pred, sst_loess_se, sst_loess_resid,
                                                       pdo_index, pdo_loess_pred, pdo_loess_se, pdo_loess_resid)
dispersal_drivers_EHS_fillna <- dispersal_drivers_EHS %>% fill(mean_daily_ehs_sst, condition, sst_loess_pred, sst_loess_se, sst_loess_resid,
                                                               pdo_index, pdo_loess_pred, pdo_loess_se, pdo_loess_resid, pupratio, X5yrtrend, X.y, FoodType.x, prop_foodtype_otter_sum.x)


### LOOCV

### Variable importance
### global model with variable shortlist
new_dd_EHS_nona <- data.frame(dispersal_drivers_EHS_fillna, id=1+c(1:nrow(dispersal_drivers_EHS_fillna))%%36)
id <- unique(new_dd_EHS_nona$id)

new_dd_EHS_nona$seaotter <- as.factor(new_dd_EHS_nona$seaotter)
new_dd_EHS_nona$sex <- as.factor(new_dd_EHS_nona$sex)
new_dd_EHS_nona$surrogate <- as.factor(new_dd_EHS_nona$surrogate)
new_dd_EHS_nona$outcome <- as.factor(new_dd_EHS_nona$outcome)

######### ALL VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_av<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_av <- cbind(loo_ls_av, imp)
}

str(loo_ls_av)
imp_df_loo_ls_av <- as.data.frame(t(loo_ls_av))

str(imp_df_loo_ls_av)
imp_incMSE_ls_av <- imp_df_loo_ls_av[seq(1, nrow(imp_df_loo_ls_av),2),]
imp_IncNodePurity_ls_av <- imp_df_loo_ls_av[seq(2, nrow(imp_df_loo_ls_av),2),]

imp_rank_ls_av <- t(apply(imp_IncNodePurity_ls_av, 1, rank, ties.method = "min"))
str(imp_rank_ls_av)
head(imp_rank_ls_av)

colMeans(imp_rank_ls_av)
colMeans(imp_incMSE_ls_av)
colMeans(imp_IncNodePurity_ls_av)

ls_av_global <- randomForest(scale_loess_pred ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + 
                               strandage_d + condition + mei_rank +
                               mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                               sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                               enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE , 
                             ntree = 1000, mtry = 2)


# variable importance plots
imp_summary_ls_av <- as.data.frame(colMeans(imp_incMSE_ls_av))
names(imp_summary_ls_av)[1] <- "PercentIncMSE"
imp_summary_ls_av$variable <- c("sex", "relmonth", "strandsite", "relage_d", "strandage_d", 
                            "condition" , "mei_rank", "mean_daily_ehs_sst",
                            "days_in", "surrogate", "max_relnum", "sst_loess_pred",
                            "pdo_less_pred", "pdo_index", "percent_live_diet", "enso_loess_pred", 
                            "dens.sm", "x5yrtrend")

str(imp_summary_ls_av)
imp_summary_ls_av$variable <- as.factor(imp_summary_ls_av$variable)

imp_summary_ls_av <- imp_summary_ls_av %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_av <- ggplot(imp_summary_ls_av, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_av, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_av <- NULL
r_squared_train_ls_av <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_av <- cbind(r_squared_test_ls_av, r2_test)  
  r_squared_train_ls_av <- cbind(r_squared_train_ls_av, r2_train)  
}

mean(r_squared_test_ls_av)
mean(r_squared_train_ls_av)
# > mean(r_squared_test_ls_av)
# [1] 0.3475286
# > mean(r_squared_train_ls_av)
# [1] 0.8730564


######## ALL VARIABLES AND RAW SCALE AS RESPONSE
loo_s_av<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_av <- cbind(loo_s_av, imp)
}

str(loo_s_av)
imp_df_loo_s_av <- as.data.frame(t(loo_s_av))

str(imp_df_loo_s_av)
imp_incMSE_s_av <- imp_df_loo_s_av[seq(1, nrow(imp_df_loo_s_av),2),]
imp_IncNodePurity_s_av <- imp_df_loo_s_av[seq(2, nrow(imp_df_loo_s_av),2),]

imp_rank_s_av <- t(apply(imp_IncNodePurity_s_av, 1, rank, ties.method = "min"))
str(imp_rank_s_av)
head(imp_rank_s_av)

colMeans(imp_rank_s_av)
colMeans(imp_incMSE_s_av)
colMeans(imp_IncNodePurity_s_av)

s_av_global <- randomForest(scale~ sex + relmonth + strandsiteATOS + relage_d + 
                               strandage_d + condition + mei_rank +
                               mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                               sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                               enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_av <- as.data.frame(colMeans(imp_incMSE_s_av))
names(imp_summary_s_av)[1] <- "PercentIncMSE"
imp_summary_s_av$variable <- c("sex",  "relmonth", "strandsite", "relage_d", "strandage_d", 
                                "condition" , "mei_rank", "mean_daily_ehs_sst",
                                "days_in", "surrogate", "max_relnum", "sst_loess_pred",
                                "pdo_less_pred", "pdo_index", "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_s_av)
imp_summary_s_av$variable <- as.factor(imp_summary_s_av$variable)


imp_summary_s_av <- imp_summary_s_av %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_av <- ggplot(imp_summary_s_av, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_av, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_av <- NULL
r_squared_train_s_av <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + 
                       sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_av <- cbind(r_squared_test_s_av, r2_test)  
  r_squared_train_s_av <- cbind(r_squared_train_s_av, r2_train)  
}

mean(r_squared_test_s_av)
mean(r_squared_train_s_av)
# > mean(r_squared_test_s_av)
# [1] -0.1227526
# > mean(r_squared_train_s_av)
# [1] -0.6871564

##################### test what variables are correlated and remove them
library(corrplot)
new_dd_EHS_nona_num <- dplyr::select_if(new_dd_EHS_nona, is.numeric)
new_dd_EHS_nona_num <- new_dd_EHS_nona_num %>%  dplyr::select("scale", "relmonth", "relage_d", 
                                    "strandage_d", "strandsiteATOS",  "mean_daily_ehs_sst", "condition", "days_in", 
                                     "max_relnum", "sst_loess_pred", "pdo_loess_pred", "enso_loess_pred", 
                                    "mei_rank", "pdo_index","pupratio", "X5yrtrend", "prop_foodtype_otter_sum")
corplot_allvar <- cor(new_dd_EHS_nona_num)
corrplot(corplot_allvar)



######### UNCORRELATED VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_2v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_2v <- cbind(loo_ls_2v, imp)
}

str(loo_ls_2v)
imp_df_loo_ls_2v <- as.data.frame(t(loo_ls_2v))

str(imp_df_loo_ls_2v)
imp_incMSE_ls_2v <- imp_df_loo_ls_2v[seq(1, nrow(imp_df_loo_ls_2v),2),]
imp_IncNodePurity_ls_2v <- imp_df_loo_ls_2v[seq(2, nrow(imp_df_loo_ls_2v),2),]

imp_rank_ls_2v <- t(apply(imp_IncNodePurity_ls_2v, 1, rank, ties.method = "min"))
str(imp_rank_ls_2v)
head(imp_rank_ls_2v)

colMeans(imp_rank_ls_2v)
colMeans(imp_incMSE_ls_2v)

colMeans(imp_IncNodePurity_ls_2v)

ls_2v_global <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                               strandage_d + condition  + mei_rank +
                               mean_daily_ehs_sst +  surrogate + 
                               + prop_foodtype_otter_sum +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE,
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_2v <- as.data.frame(colMeans(imp_incMSE_ls_2v))
names(imp_summary_ls_2v)[1] <- "PercentIncMSE"
imp_summary_ls_2v$variable <- c("sex",   "relmonth", "strandsite", "relage_d", "strandage_d", 
                                "condition" , "mei_rank", "mean_daily_ehs_sst",
                                "surrogate", 
                                  "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_2v)
imp_summary_ls_2v$variable <- as.factor(imp_summary_ls_2v$variable)

imp_summary_ls_2v <- imp_summary_ls_2v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_2v <- ggplot(imp_summary_ls_2v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_2v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_2v <- NULL
r_squared_train_ls_2v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_2v <- cbind(r_squared_test_ls_2v, r2_test)  
  r_squared_train_ls_2v <- cbind(r_squared_train_ls_2v, r2_train)  
}

mean(r_squared_test_ls_2v)
mean(r_squared_train_ls_2v)
# > mean(r_squared_test_ls_2v)
# [1] 0.2657378
# > mean(r_squared_train_ls_2v)
# [1] 0.8518921



######## UNCORRELATED VARIABLES AND RAW SCALE AS RESPONSE
loo_s_2v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex   + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_2v <- cbind(loo_s_2v, imp)
}

str(loo_s_2v)
imp_df_loo_s_2v <- as.data.frame(t(loo_s_2v))

str(imp_df_loo_s_2v)
imp_incMSE_s_2v <- imp_df_loo_s_2v[seq(1, nrow(imp_df_loo_s_2v),2),]
imp_IncNodePurity_s_2v <- imp_df_loo_s_2v[seq(2, nrow(imp_df_loo_s_2v),2),]

imp_rank_s_2v <- t(apply(imp_IncNodePurity_s_2v, 1, rank, ties.method = "min"))
str(imp_rank_s_2v)
head(imp_rank_s_2v)

colMeans(imp_rank_s_2v)
colMeans(imp_incMSE_s_2v)
colMeans(imp_IncNodePurity_s_2v)

s_2v_global <- randomForest(scale~ sex + relmonth + strandsiteATOS + relage_d + 
                              strandage_d + condition  + mei_rank +
                              mean_daily_ehs_sst +  surrogate + 
                              + prop_foodtype_otter_sum +
                              enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_2v <- as.data.frame(colMeans(imp_incMSE_s_2v))
names(imp_summary_s_2v)[1] <- "PercentIncMSE"
imp_summary_s_2v$variable <- c("sex", "relmonth", "strandsite", "relage_d", "strandage_d", 
                               "condition" , "mei_rank", "mean_daily_ehs_sst",
                                "surrogate","percent_live_diet", "enso_loess_pred", 
                               "dens.sm", "x5yrtrend")

str(imp_summary_s_2v)
imp_summary_s_2v$variable <- as.factor(imp_summary_s_2v$variable)

imp_summary_s_2v <- imp_summary_s_2v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_2v <- ggplot(imp_summary_s_2v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_2v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_2v <- NULL
r_squared_train_s_2v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex  + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_2v <- cbind(r_squared_test_s_2v, r2_test)  
  r_squared_train_s_2v <- cbind(r_squared_train_s_2v, r2_train)  
}

mean(r_squared_test_s_2v)
mean(r_squared_train_s_2v)
# > mean(r_squared_test_s_2v)
# [1] -0.06537995
# > mean(r_squared_train_s_2v)
# [1] -0.6583423



######### POS VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_3v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                        mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_ls_3v <- cbind(loo_ls_3v, imp)
}

str(loo_ls_3v)
imp_df_loo_ls_3v <- as.data.frame(t(loo_ls_3v))

str(imp_df_loo_ls_3v)
imp_incMSE_ls_3v <- imp_df_loo_ls_3v[seq(1, nrow(imp_df_loo_ls_3v),2),]
imp_IncNodePurity_ls_3v <- imp_df_loo_ls_3v[seq(2, nrow(imp_df_loo_ls_3v),2),]

imp_rank_ls_3v <- t(apply(imp_IncNodePurity_ls_3v, 1, rank, ties.method = "min"))
str(imp_rank_ls_3v)
head(imp_rank_ls_3v)

colMeans(imp_rank_ls_3v)
colMeans(imp_incMSE_ls_3v)

colMeans(imp_IncNodePurity_ls_3v)

ls_3v_global <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  
                               + prop_foodtype_otter_sum +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_3v <- as.data.frame(colMeans(imp_incMSE_ls_3v))
names(imp_summary_ls_3v)[1] <- "PercentIncMSE"
imp_summary_ls_3v$variable <- c("sex",  "relage_d",  
                                 "mei_rank", "mean_daily_ehs_sst",
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_3v)
imp_summary_ls_3v$variable <- as.factor(imp_summary_ls_3v$variable)

imp_summary_ls_3v <- imp_summary_ls_3v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_3v <- ggplot(imp_summary_ls_3v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_3v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_3v <- NULL
r_squared_train_ls_3v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_3v <- cbind(r_squared_test_ls_3v, r2_test)  
  r_squared_train_ls_3v <- cbind(r_squared_train_ls_3v, r2_train)  
}

mean(r_squared_test_ls_3v)
mean(r_squared_train_ls_3v)
# > mean(r_squared_test_ls_3v)
# [1] 0.4622875
# > mean(r_squared_train_ls_3v)
# [1] 0.8640189




######## POS VARIABLES AND RAW SCALE AS RESPONSE
loo_s_3v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_3v <- cbind(loo_s_3v, imp)
}

str(loo_s_3v)
imp_df_loo_s_3v <- as.data.frame(t(loo_s_3v))

str(imp_df_loo_s_3v)
imp_incMSE_s_3v <- imp_df_loo_s_3v[seq(1, nrow(imp_df_loo_s_3v),2),]
imp_IncNodePurity_s_3v <- imp_df_loo_s_3v[seq(2, nrow(imp_df_loo_s_3v),2),]

imp_rank_s_3v <- t(apply(imp_IncNodePurity_s_3v, 1, rank, ties.method = "min"))
str(imp_rank_s_3v)
head(imp_rank_s_3v)

colMeans(imp_rank_s_3v)
colMeans(imp_incMSE_s_3v)
colMeans(imp_IncNodePurity_s_3v)

s_3v_global <- randomForest(scale~ sex  +  relage_d + 
                              mei_rank +
                              mean_daily_ehs_sst +  
                              + prop_foodtype_otter_sum +
                              enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_3v <- as.data.frame(colMeans(imp_incMSE_s_3v))
names(imp_summary_s_3v)[1] <- "PercentIncMSE"
imp_summary_s_3v$variable <- c("sex",  "relage_d", 
                                "mei_rank", "mean_daily_ehs_sst",
                             "percent_live_diet", "enso_loess_pred", 
                               "dens.sm", "x5yrtrend")

str(imp_summary_s_3v)
imp_summary_s_3v$variable <- as.factor(imp_summary_s_3v$variable)

imp_summary_s_3v <- imp_summary_s_3v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_3v <- ggplot(imp_summary_s_3v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_3v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_3v <- NULL
r_squared_train_s_3v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_3v <- cbind(r_squared_test_s_3v, r2_test)  
  r_squared_train_s_3v <- cbind(r_squared_train_s_3v, r2_train)  
}

mean(r_squared_test_s_3v)
mean(r_squared_train_s_3v)
# > mean(r_squared_test_s_3v)
# [1] -0.0943753
# > mean(r_squared_train_s_3v)
# [1] -0.4451316



######## POS VARIABLES AND RAW SCALE AS RESPONSE, dropping more
loo_s_4v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~    mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred +  X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,],
                     importance = TRUE , ntree = 1000, mtry = 2)
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_s_4v <- cbind(loo_s_4v, imp)
}

str(loo_s_4v)
imp_df_loo_s_4v <- as.data.frame(t(loo_s_4v))

str(imp_df_loo_s_4v)
imp_incMSE_s_4v <- imp_df_loo_s_4v[seq(1, nrow(imp_df_loo_s_4v),2),]
imp_IncNodePurity_s_4v <- imp_df_loo_s_4v[seq(2, nrow(imp_df_loo_s_4v),2),]

imp_rank_s_4v <- t(apply(imp_IncNodePurity_s_4v, 1, rank, ties.method = "min"))
str(imp_rank_s_4v)
head(imp_rank_s_4v)

colMeans(imp_rank_s_4v)
colMeans(imp_incMSE_s_4v)
colMeans(imp_IncNodePurity_s_4v)

s_4v_global <- randomForest(scale~ mean_daily_ehs_sst +  
                              + prop_foodtype_otter_sum +
                              enso_loess_pred +  X5yrtrend,  data = new_dd_EHS_nona, importance = TRUE, ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_4v <- as.data.frame(colMeans(imp_incMSE_s_4v))
names(imp_summary_s_4v)[1] <- "PercentIncMSE"
imp_summary_s_4v$variable <- c("mean_daily_ehs_sst",
                               "percent_live_diet", "enso_loess_pred", 
                                "x5yrtrend")

str(imp_summary_s_4v)
imp_summary_s_4v$variable <- as.factor(imp_summary_s_4v$variable)

imp_summary_s_4v <- imp_summary_s_4v %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_4v <- ggplot(imp_summary_s_4v, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_4v, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo


#### error calculations
### R squared

r_squared_test_s_4v <- NULL
r_squared_train_s_4v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred +  X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE, ntree= 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_4v <- cbind(r_squared_test_s_4v, r2_test)  
  r_squared_train_s_4v <- cbind(r_squared_train_s_4v, r2_train)  
}

mean(r_squared_test_s_4v)
mean(r_squared_train_s_4v)
# > mean(r_squared_test_s_4v)
# [1] 0.0006571564
# > mean(r_squared_train_s_4v)
# [1] -0.4257987






######### POS Non-Correlated VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_4v<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex +  relage_d +
                       + prop_foodtype_otter_sum +
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
                               + prop_foodtype_otter_sum +
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
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_4v <- NULL
r_squared_train_ls_4v <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                    
                       + prop_foodtype_otter_sum +
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


grid.arrange(varimp_s_av, varimp_ls_av, varimp_s_2v, varimp_ls_2v, varimp_s_4v, varimp_ls_4v,
             layout_matrix = rbind(c(1,2),
                                   c(1,2),
                                   c(1,2),
                                   c(3,4),
                                   c(3,4),
                                   c(5,6)))


###############
# ls_4v is best

####################
# visualizations for ls_4v

ls_4v_global <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  
                               + prop_foodtype_otter_sum +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                             ntree = 1000, mtry = 2 )

################## model D: loess scale, top 50% variables
library(pdp)
## overlap many PDPs from many random forests

#here is one enso loess
pdp::partial(ls_4v_global, pred.var = "enso_loess_pred", plot = T, rug = 2)

#loop through and create 100 for 100 different RF splits and runs

ENSO_Loess_pdp_d <- NULL
for (i in id){
  
  sub_p <- id[i]
  
  #fit model
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                        mei_rank +
                        mean_daily_ehs_sst +  
                        + prop_foodtype_otter_sum +
                        enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona, importance = TRUE, 
                      ntree = 1000, mtry = 2 )
  
  # create partial object
  partial  <- pdp::partial(rf, pred.var = "enso_loess_pred", plot = F, rug = T)
  
  partial$ID <- id[i]
  
  ENSO_Loess_pdp_d <- rbind(ENSO_Loess_pdp_d, partial)  
}


ENSO_Loess_pdp_d
str(ENSO_Loess_pdp_d)
ENSO_Loess_pdp_d <- as.data.frame(ENSO_Loess_pdp_d)

# plot 
ENSO_Loess_pdp_d_plot <- ggplot(ENSO_Loess_pdp_d, aes(x = enso_loess_pred, y = yhat))+
  geom_line(aes(group = ID),alpha = 0.25)+
  #geom_smooth()+
  #scale_color_distiller(palette = 'BuGn', direction = 1)+
  theme_classic()

##ICE PLOT
# ice function
ranger_ice <- function(object, newdata) {
  predict(object, newdata)#$predictions
}

# create ice objects
ice_enso_loess_d <- pdp::partial(ls_4v_global, pred.var = "enso_loess_pred", pred.fun = ranger_ice)

# plot enso_loess ICE plots
ice_enso_loess_d <- ice_enso_loess_d %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_enso_loess_d_p <- ggplot(ice_enso_loess_d, aes(enso_loess_pred, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

ice_enso_loess_d_p_cent <- ggplot(ice_enso_loess_d, aes(enso_loess_pred, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

# create ice objects
dens.sm_4v <- pdp::partial(ls_4v_global, pred.var = "dens.sm", pred.fun = ranger_ice)

# plot dens.sm ICE plots
dens.sm_4v <- dens.sm_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

dens.sm_4v_p <- ggplot(dens.sm_4v, aes(dens.sm, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

dens.sm_4v_p_cent <- ggplot(dens.sm_4v, aes(dens.sm, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo


# create ice objects
diet_4v <- pdp::partial(ls_4v_global, pred.var = "prop_foodtype_otter_sum", pred.fun = ranger_ice)

# plot dens.sm ICE plots
diet_4v <- diet_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

diet_4v_p <- ggplot(diet_4v, aes(prop_foodtype_otter_sum, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

diet_4v_p_cent <- ggplot(diet_4v, aes(prop_foodtype_otter_sum, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

# create ice objects
X5yrtrend_4v <- pdp::partial(ls_4v_global, pred.var = "X5yrtrend", pred.fun = ranger_ice)

# plot dens.sm ICE plots
X5yrtrend_4v <- X5yrtrend_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

X5yrtrend_4v_p <- ggplot(X5yrtrend_4v, aes(X5yrtrend, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

X5yrtrend_4v_p_cent <- ggplot(X5yrtrend_4v, aes(X5yrtrend, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo


# create ice objects
relage_d_4v <- pdp::partial(ls_4v_global, pred.var = "relage_d", pred.fun = ranger_ice)

# plot dens.sm ICE plots
relage_d_4v <- relage_d_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

relage_d_4v_p <- ggplot(relage_d_4v, aes(relage_d, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

relage_d_4v_p_cent <- ggplot(relage_d_4v, aes(relage_d, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  coord_cartesian(expand = 0)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo



# create ice objects
sex_4v <- pdp::partial(ls_4v_global, pred.var = "sex", pred.fun = ranger_ice)

# plot dens.sm ICE plots
sex_4v <- sex_4v %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

sex_4v_p <- ggplot(sex_4v, aes(sex, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

sex_4v_p_cent <- ggplot(sex_4v, aes(sex, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo



dens.sm_ls_p <- ggplot(dd_ehs)+
  geom_point(aes(x = dens.sm, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = dens.sm, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

enso_loess_pred_ls_p <- ggplot(dd_ehs)+
  geom_point(aes(x = enso_loess_pred, y = scale_loess_pred, color = sex, show.legend = FALSE))+
  geom_smooth(aes(x = enso_loess_pred, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

percentlive.y_ls_p <- ggplot(dd_ehs)+
  geom_point(aes(x = percentlive.y, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = percentlive.y, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

relage_d_ls_p <- ggplot(dd_ehs)+
  geom_point(aes(x = relage_d, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = relage_d, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo

X5yrtrend_ls_p <- ggplot(dd_ehs)+
  geom_point(aes(x = X5yrtrend, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = X5yrtrend, y = scale_loess_pred), color = "darkgrey")+
  coord_cartesian(expand = 0)+
  themeo


grid.arrange(ice_enso_loess_d_p_cent, dens.sm_4v_p_cent, diet_4v_p_cent, X5yrtrend_4v_p_cent, relage_d_4v_p_cent,
             enso_loess_pred_ls_p, dens.sm_ls_p, percentlive.y_ls_p, X5yrtrend_ls_p, relage_d_ls_p,
             layout_matrix = rbind(c(6,1),
                                   c(7,2),
                                   c(8,3),
                                   c(9,4),
                                   c(10,5)))


pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "dens.sm"), plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "prop_foodtype_otter_sum"), plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "X5yrtrend"), plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var = c("prop_foodtype_otter_sum", "dens.sm"), plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var = c("prop_foodtype_otter_sum", "X5yrtrend"), plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var = c("X5yrtrend", "dens.sm"), plot = T, rug = T, chull = T)



partial_enso_dens <- pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "dens.sm"), plot = F, rug = T, chull = T)
partial_enso_trend <- pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "X5yrtrend"), plot = F, rug = T, chull = T)
partial_trend_dens <- pdp::partial(ls_4v_global, pred.var = c("X5yrtrend", "dens.sm"), plot = F, rug = T, chull = T)
partial_enso_diet <- pdp::partial(ls_4v_global, pred.var = c("enso_loess_pred", "prop_foodtype_otter_sum"), plot = F, rug = T, chull = T)



library(RColorBrewer)

cols <- c(brewer.pal(11,"Spectral")[c(11,10,9,8,7,6,5,4,3,2,1)])
cols2 <- c(brewer.pal(11,"Spectral")[c(11,10,9,8,7,6,5,4)])
cols3 <- c(brewer.pal(11,"Spectral")[c(10,10,9,8,7,6,5,4)])
cols4 <- c(brewer.pal(11,"Spectral")[c(10,9,8,7,6)])

MEI_dens <- ggplot()+
  geom_tile(data = partial_enso_dens, aes(x = enso_loess_pred, y = dens.sm, z = yhat, fill = yhat))+
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols2)+
  coord_cartesian(expand = 0)+
  labs(x = "MEI index", y = "density")+
  themeo


MEI_growth <- ggplot()+
  geom_tile(data = partial_enso_trend, aes(x = enso_loess_pred, y = X5yrtrend, z = yhat, fill = yhat))+
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols3)+ 
  coord_cartesian(expand = 0)+
  labs(x = "MEI index", y = "population growth")+
  themeo

MEI_diet <- ggplot()+
  geom_tile(data = partial_enso_diet, aes(x = enso_loess_pred, y = prop_foodtype_otter_sum, z = yhat, fill = yhat))+
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols)+
  coord_cartesian(expand = 0)+
  labs(x = "MEI index", y = "% live diet")+
  themeo


dens_growth <- ggplot()+
  geom_tile(data = partial_trend_dens, aes(x = X5yrtrend, y = dens.sm, z = yhat, fill = yhat))+
 # scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = cols4)+
  coord_cartesian(expand = 0)+
  labs(x = "population growth", y = "density")+
  themeo


pdp::partial(ls_4v_global, pred.var = "enso_loess_pred", plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var = "X5yrtrend", plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var =  "dens.sm", plot = T, rug = T, chull = T)
pdp::partial(ls_4v_global, pred.var =  "prop_foodtype_otter_sum", plot = T, rug = T, chull = T)




grid.arrange(enso_loess_pred_ls_p, dens.sm_ls_p, percentlive.y_ls_p, X5yrtrend_ls_p, relage_d_ls_p, 
             ice_enso_loess_d_p_cent, dens.sm_4v_p_cent, diet_4v_p_cent, X5yrtrend_4v_p_cent, relage_d_4v_p_cent,
             MEI_dens, MEI_growth, MEI_diet, dens_growth,  varimp_ls_4v, 
             layout_matrix = rbind(c(1,1,6,6,11,11,11),
                                   c(2,2,7,7,12,12,12),
                                   c(3,3,8,8,13,13,13),
                                   c(4,4,9,9,14,14,14),
                                   c(5,5,10,10,15,15,15)))

plot(dd_ehs$condition, dd_ehs$percentlive.y)
 
ggplot(dd_ehs)+
   geom_point(aes(x = percentlive.y, y = condition))+
   geom_smooth(aes(x = percentlive.y, y = condition))


# some sum stats for paper

#scale
max(dispersal_drivers$scale) #6.008212
min(dispersal_drivers$scale) #0.01
mean(dispersal_drivers$scale) #1.038502
median(dispersal_drivers$scale) #0.8058683
sd(dispersal_drivers$scale) #1.109983


#scale_loess
max(dispersal_drivers$scale_loess_pred) #2.602865
min(dispersal_drivers$scale_loess_pred) #0.4502193
mean(dispersal_drivers$scale_loess_pred) #1.059812
median(dispersal_drivers$scale_loess_pred) #0.8462748
sd(dispersal_drivers$scale_loess_pred) #0.5828553
 