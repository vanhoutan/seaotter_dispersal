# rerun random forests but with rescaled data from 0-1

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



# rescale numeric data
library(scales)

#check data structure and make sure nothing is numeric that shouldnt be (aka dates, months, years, otters)
str(dispersal_drivers_EHS_fillna)
dispersal_drivers_EHS_fillna$relyear <- as.character(dispersal_drivers_EHS_fillna$relyear)
dispersal_drivers_EHS_fillna$relmonth <- as.character(dispersal_drivers_EHS_fillna$relmonth)
dispersal_drivers_EHS_fillna$relyear_first <- as.character(dispersal_drivers_EHS_fillna$relyear_first)

dispersal_drivers_EHS_fillna_01 <- dispersal_drivers_EHS_fillna %>% dplyr::select_if(is.numeric) %>% 
  map(~ scales::rescale(.)) %>% 
  as.data.frame() 

colnames(dispersal_drivers_EHS_fillna_01)

dispersal_drivers_EHS_fillna_01_chr <- dispersal_drivers_EHS_fillna %>%  dplyr::select("seaotter", "sex", "relyear", "relmonth",
                                                                                       "surrogate", "date")

#just select needed columns
dispersal_drivers_EHS_fillna_01 <- dispersal_drivers_EHS_fillna_01 %>%  
          dplyr::select("scale", "relage_d", "scale_loess_pred", "dens.sm",
                       "strandage_d", "strandsiteATOS",  "mean_daily_ehs_sst", "condition", "days_in", 
                        "max_relnum", "sst_loess_pred", "pdo_loess_pred", "enso_loess_pred", 
                       "mei_rank", "pdo_index","pupratio", "X5yrtrend", "prop_foodtype_otter_sum")

dispersal_drivers_EHS_fillna_01 <- cbind(dispersal_drivers_EHS_fillna_01, dispersal_drivers_EHS_fillna_01_chr)
str(dispersal_drivers_EHS_fillna_01)



# try some random forest models on the scaled dataset
new_dd_EHS_nona_01 <- data.frame(dispersal_drivers_EHS_fillna_01, id=1+c(1:nrow(dispersal_drivers_EHS_fillna))%%36)
id <- unique(new_dd_EHS_nona_01$id)

new_dd_EHS_nona_01$seaotter <- as.factor(new_dd_EHS_nona_01$seaotter)
new_dd_EHS_nona_01$sex <- as.factor(new_dd_EHS_nona_01$sex)
new_dd_EHS_nona_01$surrogate <- as.factor(new_dd_EHS_nona_01$surrogate)


######### UNCORRELATED VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_2v_01<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  +seaotter + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona_01$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona_01[new_dd_EHS_nona_01$id==i,])
  loo_ls_2v_01 <- cbind(loo_ls_2v_01, imp)
}

str(loo_ls_2v_01)
imp_df_loo_ls_2v_01 <- as.data.frame(t(loo_ls_2v_01))

str(imp_df_loo_ls_2v_01)
imp_incMSE_ls_2v_01 <- imp_df_loo_ls_2v_01[seq(1, nrow(imp_df_loo_ls_2v_01),2),]
imp_IncNodePurity_ls_2v_01 <- imp_df_loo_ls_2v_01[seq(2, nrow(imp_df_loo_ls_2v_01),2),]

imp_rank_ls_2v_01 <- t(apply(imp_IncNodePurity_ls_2v_01, 1, rank, ties.method = "min"))
str(imp_rank_ls_2v_01)
head(imp_rank_ls_2v_01)

colMeans(imp_rank_ls_2v_01)
colMeans(imp_incMSE_ls_2v_01)

colMeans(imp_IncNodePurity_ls_2v_01)

ls_2v_global_01 <- randomForest(scale_loess_pred ~ sex  +seaotter + relmonth + strandsiteATOS + relage_d + 
                               strandage_d + condition  + mei_rank +
                               mean_daily_ehs_sst +  surrogate + 
                               + prop_foodtype_otter_sum +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona_01, importance = TRUE,
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_2v_01 <- as.data.frame(colMeans(imp_incMSE_ls_2v_01))
names(imp_summary_ls_2v_01)[1] <- "PercentIncMSE"
imp_summary_ls_2v_01$variable <- c("sex", "otter",  "relmonth", "strandsite", "relage_d", "strandage_d", 
                                "condition" , "mei_rank", "mean_daily_ehs_sst",
                                "surrogate", 
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_2v_01)
imp_summary_ls_2v_01$variable <- as.factor(imp_summary_ls_2v_01$variable)

imp_summary_ls_2v_01 <- imp_summary_ls_2v_01 %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_2v_01 <- ggplot(imp_summary_ls_2v_01, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_2v_01, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_2v_01 <- NULL
r_squared_train_ls_2v_01 <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +seaotter + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona[new_dd_EHS_nona_01$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona_01$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona_01))^2)/sum((new_dd_EHS_nona_01$scale_loess_pred-mean(new_dd_EHS_nona_01$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_2v_01 <- cbind(r_squared_test_ls_2v_01, r2_test)  
  r_squared_train_ls_2v_01 <- cbind(r_squared_train_ls_2v_01, r2_train)  
}

mean(r_squared_test_ls_2v_01)
mean(r_squared_train_ls_2v_01)
# > mean(r_squared_test_ls_2v)
# [1] 0.2657378
# > mean(r_squared_train_ls_2v)
# [1] 0.8518921



######## UNCORRELATED VARIABLES AND RAW SCALE AS RESPONSE
loo_s_2v_01<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex  +seaotter + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona_01$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona_01[new_dd_EHS_nona_01$id==i,])
  loo_s_2v_01 <- cbind(loo_s_2v_01, imp)
}

str(loo_s_2v_01)
imp_df_loo_s_2v_01 <- as.data.frame(t(loo_s_2v_01))

str(imp_df_loo_s_2v_01)
imp_incMSE_s_2v_01 <- imp_df_loo_s_2v_01[seq(1, nrow(imp_df_loo_s_2v_01),2),]
imp_IncNodePurity_s_2v_01 <- imp_df_loo_s_2v_01[seq(2, nrow(imp_df_loo_s_2v_01),2),]

imp_rank_s_2v_01 <- t(apply(imp_IncNodePurity_s_2v_01, 1, rank, ties.method = "min"))
str(imp_rank_s_2v_01)
head(imp_rank_s_2v_01)

colMeans(imp_rank_s_2v_01)
colMeans(imp_incMSE_s_2v_01)
colMeans(imp_IncNodePurity_s_2v_01)

s_2v_global_01 <- randomForest(scale~ sex  +seaotter + relmonth + strandsiteATOS + relage_d + 
                              strandage_d + condition  + mei_rank +
                              mean_daily_ehs_sst +  surrogate + 
                              + prop_foodtype_otter_sum +
                              enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona_01, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_2v_01 <- as.data.frame(colMeans(imp_incMSE_s_2v_01))
names(imp_summary_s_2v_01)[1] <- "PercentIncMSE"
imp_summary_s_2v_01$variable <- c("sex", "otter",  "relmonth", "strandsite", "relage_d", "strandage_d", 
                               "condition" , "mei_rank", "mean_daily_ehs_sst",
                               "surrogate","percent_live_diet", "enso_loess_pred", 
                               "dens.sm", "x5yrtrend")

str(imp_summary_s_2v_01)
imp_summary_s_2v_01$variable <- as.factor(imp_summary_s_2v_01$variable)

imp_summary_s_2v_01 <- imp_summary_s_2v_01 %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_2v_01 <- ggplot(imp_summary_s_2v_01, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_2v_01, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_2v_01 <- NULL
r_squared_train_s_2v_01 <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex  +seaotter + relmonth + strandsiteATOS + relage_d + 
                       strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst +  surrogate + 
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona_01[new_dd_EHS_nona_01$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona_01$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona_01))^2)/sum((new_dd_EHS_nona_01$scale_loess_pred-mean(new_dd_EHS_nona_01$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_2v_01 <- cbind(r_squared_test_s_2v_01, r2_test)  
  r_squared_train_s_2v_01 <- cbind(r_squared_train_s_2v_01, r2_train)  
}

mean(r_squared_test_s_2v_01)
mean(r_squared_train_s_2v_01)
# > mean(r_squared_test_s_2v_01)
# [1] -0.07829724
# > mean(r_squared_train_s_2v_01)
# [1] -0.08563565



######### POS VARIABLES and LOESS SCALE AS RESPONSE
loo_ls_3v_01<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, 
                     data = new_dd_EHS_nona_01[new_dd_EHS_nona_01$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona_01[new_dd_EHS_nona_01$id==i,])
  loo_ls_3v_01 <- cbind(loo_ls_3v_01, imp)
}

str(loo_ls_3v_01)
imp_df_loo_ls_3v_01 <- as.data.frame(t(loo_ls_3v_01))

str(imp_df_loo_ls_3v_01)
imp_incMSE_ls_3v_01 <- imp_df_loo_ls_3v_01[seq(1, nrow(imp_df_loo_ls_3v_01),2),]
imp_IncNodePurity_ls_3v_01 <- imp_df_loo_ls_3v_01[seq(2, nrow(imp_df_loo_ls_3v_01),2),]

imp_rank_ls_3v_01 <- t(apply(imp_IncNodePurity_ls_3v_01, 1, rank, ties.method = "min"))
str(imp_rank_ls_3v_01)
head(imp_rank_ls_3v_01)

colMeans(imp_rank_ls_3v_01)
colMeans(imp_incMSE_ls_3v_01)

colMeans(imp_IncNodePurity_ls_3v_01)

ls_3v_global_01 <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                               mei_rank +
                               mean_daily_ehs_sst +  
                               + prop_foodtype_otter_sum +
                               enso_loess_pred + dens.sm + X5yrtrend,   data = new_dd_EHS_nona_01, importance = TRUE, 
                             ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_ls_3v_01 <- as.data.frame(colMeans(imp_incMSE_ls_3v_01))
names(imp_summary_ls_3v_01)[1] <- "PercentIncMSE"
imp_summary_ls_3v_01$variable <- c("sex",  "relage_d",  
                                "mei_rank", "mean_daily_ehs_sst",
                                "percent_live_diet", "enso_loess_pred", 
                                "dens.sm", "x5yrtrend")

str(imp_summary_ls_3v_01)
imp_summary_ls_3v_01$variable <- as.factor(imp_summary_ls_3v_01$variable)

imp_summary_ls_3v_01 <- imp_summary_ls_3v_01 %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_ls_3v_01 <- ggplot(imp_summary_ls_3v_01, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_ls_3v_01, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_ls_3v_01 <- NULL
r_squared_train_ls_3v_01 <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona_01[new_dd_EHS_nona_01$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona_01$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona_01))^2)/sum((new_dd_EHS_nona_01$scale_loess_pred-mean(new_dd_EHS_nona_01$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_3v_01 <- cbind(r_squared_test_ls_3v_01, r2_test)  
  r_squared_train_ls_3v_01 <- cbind(r_squared_train_ls_3v_01, r2_train)  
}

mean(r_squared_test_ls_3v_01)
mean(r_squared_train_ls_3v_01)
# > mean(r_squared_test_ls_3v_01)
# [1] 0.46638
# > mean(r_squared_train_ls_3v_01)
# [1] 0.8652411



######## POS VARIABLES AND RAW SCALE AS RESPONSE
loo_s_3v_01<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona_01[new_dd_EHS_nona_01$id!=i,],
                     importance = TRUE, ntree = 1000, mtry = 2 )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona_01[new_dd_EHS_nona_01$id==i,])
  loo_s_3v_01 <- cbind(loo_s_3v_01, imp)
}

str(loo_s_3v_01)
imp_df_loo_s_3v_01 <- as.data.frame(t(loo_s_3v_01))

str(imp_df_loo_s_3v_01)
imp_incMSE_s_3v_01 <- imp_df_loo_s_3v_01[seq(1, nrow(imp_df_loo_s_3v_01),2),]
imp_IncNodePurity_s_3v_01 <- imp_df_loo_s_3v_01[seq(2, nrow(imp_df_loo_s_3v_01),2),]

imp_rank_s_3v_01 <- t(apply(imp_IncNodePurity_s_3v_01, 1, rank, ties.method = "min"))
str(imp_rank_s_3v_01)
head(imp_rank_s_3v_01)

colMeans(imp_rank_s_3v_01)
colMeans(imp_incMSE_s_3v_01)
colMeans(imp_IncNodePurity_s_3v_01)

s_3v_global_01 <- randomForest(scale~ sex  +  relage_d + 
                              mei_rank +
                              mean_daily_ehs_sst +  
                              + prop_foodtype_otter_sum +
                              enso_loess_pred + dens.sm + X5yrtrend,  data = new_dd_EHS_nona_01, importance = TRUE, 
                            ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_3v_01 <- as.data.frame(colMeans(imp_incMSE_s_3v_01))
names(imp_summary_s_3v_01)[1] <- "PercentIncMSE"
imp_summary_s_3v_01$variable <- c("sex",  "relage_d", 
                               "mei_rank", "mean_daily_ehs_sst",
                               "percent_live_diet", "enso_loess_pred", 
                               "dens.sm", "x5yrtrend")

str(imp_summary_s_3v_01)
imp_summary_s_3v_01$variable <- as.factor(imp_summary_s_3v_01$variable)

imp_summary_s_3v_01 <- imp_summary_s_3v_01 %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_3v_01 <- ggplot(imp_summary_s_3v_01, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_3v_01, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

#### error calculations
### R squared

r_squared_test_s_3v_01 <- NULL
r_squared_train_s_3v_01 <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ sex  +  relage_d + 
                       mei_rank +
                       mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend,
                     data = new_dd_EHS_nona_01[new_dd_EHS_nona_01$id!=i,], importance = TRUE, ntree = 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona_01$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona_01))^2)/sum((new_dd_EHS_nona_01$scale_loess_pred-mean(new_dd_EHS_nona_01$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_3v_01 <- cbind(r_squared_test_s_3v_01, r2_test)  
  r_squared_train_s_3v_01 <- cbind(r_squared_train_s_3v_01, r2_train)  
}

mean(r_squared_test_s_3v_01)
mean(r_squared_train_s_3v_01)
# > mean(r_squared_test_s_3v_01)
# [1] -0.09119041
# > mean(r_squared_train_s_3v_01)
# [1] -0.002713576



######## POS VARIABLES AND RAW SCALE AS RESPONSE, dropping more
loo_s_4v_01<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~    mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred +  X5yrtrend,
                     data = new_dd_EHS_nona[new_dd_EHS_nona_01$id!=i,],
                     importance = TRUE , ntree = 1000, mtry = 2)
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona_01[new_dd_EHS_nona$id==i,])
  loo_s_4v_01 <- cbind(loo_s_4v_01, imp)
}

str(loo_s_4v_01)
imp_df_loo_s_4v_01 <- as.data.frame(t(loo_s_4v_01))

str(imp_df_loo_s_4v_01)
imp_incMSE_s_4v_01 <- imp_df_loo_s_4v_01[seq(1, nrow(imp_df_loo_s_4v_01),2),]
imp_IncNodePurity_s_4v_01 <- imp_df_loo_s_4v_01[seq(2, nrow(imp_df_loo_s_4v_01),2),]

imp_rank_s_4v_01 <- t(apply(imp_IncNodePurity_s_4v_01, 1, rank, ties.method = "min"))
str(imp_rank_s_4v_01)
head(imp_rank_s_4v_01)

colMeans(imp_rank_s_4v_01)
colMeans(imp_incMSE_s_4v_01)
colMeans(imp_IncNodePurity_s_4v_01)

s_4v_global_01 <- randomForest(scale~ mean_daily_ehs_sst +  
                              + prop_foodtype_otter_sum +
                              enso_loess_pred +  X5yrtrend,  data = new_dd_EHS_nona_01, importance = TRUE, ntree = 1000, mtry = 2 )


# variable importance plots
imp_summary_s_4v_01 <- as.data.frame(colMeans(imp_incMSE_s_4v_01))
names(imp_summary_s_4v_01)[1] <- "PercentIncMSE"
imp_summary_s_4v_01$variable <- c("mean_daily_ehs_sst",
                               "percent_live_diet", "enso_loess_pred", 
                               "x5yrtrend")

str(imp_summary_s_4v_01)
imp_summary_s_4v_01$variable <- as.factor(imp_summary_s_4v_01$variable)

imp_summary_s_4v_01 <- imp_summary_s_4v_01 %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_s_4v_01 <- ggplot(imp_summary_s_4v_01, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_s_4v_01, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo


#### error calculations
### R squared

r_squared_test_s_4v_01 <- NULL
r_squared_train_s_4v_01 <- NULL
for (i in id){
  
  
  rf <- randomForest(scale~ mean_daily_ehs_sst +  
                       + prop_foodtype_otter_sum +
                       enso_loess_pred +  X5yrtrend,
                     data = new_dd_EHS_nona_01[new_dd_EHS_nona_01$id!=i,], importance = TRUE, ntree= 1000, mtry = 2 )
  r2_train    <- (1 - (sum((new_dd_EHS_nona_01$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona_01))^2)/sum((new_dd_EHS_nona_01$scale_loess_pred-mean(new_dd_EHS_nona_01$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test_s_4v_01 <- cbind(r_squared_test_s_4v_01, r2_test)  
  r_squared_train_s_4v_01 <- cbind(r_squared_train_s_4v_01, r2_train)  
}

mean(r_squared_test_s_4v_01)
mean(r_squared_train_s_4v_01)
# > mean(r_squared_test_s_4v_01)
# [1] -0.02942835
# > mean(r_squared_train_s_4v_01)
# [1] -0.02089611


