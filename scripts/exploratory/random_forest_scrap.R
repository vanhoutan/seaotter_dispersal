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

scale_rf_global_scale <- randomForest(scale ~ sex  + strandage_d + relage_d   +  
                                  mean_daily_ehs_sst + mei_rank + pdo_index + 
                                  sst_loess_pred , data = dispersal_drivers_nona, importance = TRUE)

scale_rf_global_scale_loess <- randomForest(scale_loess_pred ~ sex  + strandage_d + relage_d   +  
                                        mean_daily_ehs_sst + mei_rank + pdo_index + 
                                        sst_loess_pred , data = dispersal_drivers_nona, importance = TRUE)


scale_rf_global_ehs_scale <- randomForest(scale ~ sex  + strandage_d + relage_d   +  
                                            mean_daily_ehs_sst + mei_rank + pdo_index + 
                                            dens.sm +  X5yrtrend, data = dispersal_drivers_EHS_nona, importance = TRUE)

scale_rf_global_ehs_scale_loess <- randomForest(scale_loess_pred ~ sex  + strandage_d + relage_d   +  
                                  mean_daily_ehs_sst + mei_rank + pdo_index + 
                                  sst_loess_pred + dens.sm + X5yrtrend, data = dispersal_drivers_EHS_nona, importance = TRUE)

scale_rf_global_ehs_scale_loess2 <- randomForest(scale_loess_pred ~ sex  + condition  + mei_rank +
                                                   enso_loess_pred + dens.sm + X5yrtrend, data = dispersal_drivers_EHS_nona, importance = TRUE)


### LOOCV

### Variable importance
### global model with variable shortlist
new_dd_EHS_nona <- data.frame(dispersal_drivers_EHS_fillna, id=1+c(1:nrow(dispersal_drivers_EHS_fillna))%%36)
id <- unique(new_dd_EHS_nona$id)

new_dd_EHS_nona$seaotter <- as.factor(new_dd_EHS_nona$seaotter)
new_dd_EHS_nona$sex <- as.factor(new_dd_EHS_nona$sex)
new_dd_EHS_nona$surrogate <- as.factor(new_dd_EHS_nona$surrogate)
new_dd_EHS_nona$outcome <- as.factor(new_dd_EHS_nona$outcome)

loo_a <- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  + condition  + mei_rank +
                       enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_a <- cbind(loo_a, imp)
}

View(loo_a)


str(loo_a)
imp_df <- as.data.frame(t(loo_a))

str(imp_df)
imp_incMSE <- imp_df[seq(1, nrow(imp_df),2),]
imp_IncNodePurity <- imp_df[seq(2, nrow(imp_df),2),]

imp_rank <- t(apply(imp_IncNodePurity, 1, rank, ties.method = "min"))
str(imp_rank)
head(imp_rank)

colMeans(imp_rank)
colMeans(imp_incMSE)
colMeans(imp_IncNodePurity)

### lets run another one for ALL variables!
loo_b<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_b <- cbind(loo_b, imp)
}

View(loo_b)


str(loo_b)
imp_df_loo_b <- as.data.frame(t(loo_b))

str(imp_df_loo_b)
imp_incMSE_b <- imp_df_loo_b[seq(1, nrow(imp_df_loo_b),2),]
imp_IncNodePurity_b <- imp_df_loo_b[seq(2, nrow(imp_df_loo_b),2),]

imp_rank_b <- t(apply(imp_IncNodePurity_b, 1, rank, ties.method = "min"))
str(imp_rank_b)
head(imp_rank_b)

colMeans(imp_rank_b)
colMeans(imp_incMSE_b)
colMeans(imp_IncNodePurity_b)

b_global <- randomForest(scale_loess_pred ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + strandage_d + condition  + mei_rank +
                           mean_daily_ehs_sst + days_in + surrogate + max_relnum + sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                           enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona, importance = TRUE )
#performs pretty well


# now one for scale, not loess scale. With all 
loo_c<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_c <- cbind(loo_c, imp)
}

View(loo_c)


str(loo_c)
imp_df_loo_c <- as.data.frame(t(loo_c))

str(imp_df_loo_c)
imp_incMSE_c <- imp_df_loo_c[seq(1, nrow(imp_df_loo_c),2),]
imp_IncNodePurity_c <- imp_df_loo_c[seq(2, nrow(imp_df_loo_c),2),]

imp_rank_c <- t(apply(imp_IncNodePurity_c, 1, rank, ties.method = "min"))
str(imp_rank_c)
head(imp_rank_c)

colMeans(imp_rank_c)
colMeans(imp_incMSE_c)
colMeans(imp_IncNodePurity_c)


c_global <- randomForest(scale ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + strandage_d + condition  + mei_rank +
                     mean_daily_ehs_sst + days_in + surrogate + max_relnum + sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                     enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona, importance = TRUE )


# another one with the top 50% variables (imp rank) from loess all variable global model
d_global <- randomForest(scale_loess_pred ~ prop_foodtype_otter_sum + relyear + enso_loess_pred + X5yrtrend + surrogate + dens.sm + mei_rank + condition , data = new_dd_EHS_nona, importance = TRUE )


loo_d<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale_loess_pred ~ prop_foodtype_otter_sum + relyear + enso_loess_pred + X5yrtrend + surrogate + dens.sm + mei_rank + condition, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_d <- cbind(loo_d, imp)
}

View(loo_d)


str(loo_d)
imp_df_loo_d <- as.data.frame(t(loo_d))

str(imp_df_loo_d)
imp_incMSE_d <- imp_df_loo_d[seq(1, nrow(imp_df_loo_d),2),]
imp_IncNodePurity_d <- imp_df_loo_d[seq(2, nrow(imp_df_loo_d),2),]

imp_rank_d <- t(apply(imp_IncNodePurity_d, 1, rank, ties.method = "min"))
str(imp_rank_d)
head(imp_rank_d)

colMeans(imp_rank_d)
colMeans(imp_incMSE_d)
colMeans(imp_IncNodePurity_d)

# another one with the top 50% variables (imp rank) from raw scale all variable global model
e_global <- randomForest(scale ~ mei_rank+ max_relnum + X5yrtrend + condition + relage_d + enso_loess_pred + prop_foodtype_otter_sum + dens.sm, data = new_dd_EHS_nona, importance = TRUE )
loo_e<- NULL
for(i in id){
  #rf <- randomForest(Species~., data=newIris[newIris$id!=i,])
  rf <- randomForest(scale ~ mei_rank+ max_relnum + X5yrtrend + condition + relage_d + enso_loess_pred + prop_foodtype_otter_sum + dens.sm, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  # loo[[i]] <- predict(rf, newdata=newdf[newdf$id==i,])
  imp <- importance(rf, newdata=new_dd_EHS_nona[new_dd_EHS_nona$id==i,])
  loo_e <- cbind(loo_e, imp)
}

View(loo_e)


str(loo_e)
imp_df_loo_e<- as.data.frame(t(loo_e))

str(imp_df_loo_e)
imp_incMSE_e <- imp_df_loo_e[seq(1, nrow(imp_df_loo_e),2),]
imp_IncNodePurity_e <- imp_df_loo_e[seq(2, nrow(imp_df_loo_e),2),]

imp_rank_e <- t(apply(imp_IncNodePurity_e, 1, rank, ties.method = "min"))
str(imp_rank_e)
head(imp_rank_e)

colMeans(imp_rank_e)
colMeans(imp_incMSE_e)
colMeans(imp_IncNodePurity_e)


##########################################
### do some visulization for each of 4 models (raw scale all variable C , loess scale all variable B, raw scale top 50% E, loess scale top 50% D)

### scale loess all variable B

imp_summary_b <- as.data.frame(colMeans(imp_incMSE_b))
names(imp_summary_b)[1] <- "PercentIncMSE"
imp_summary_b$variable <- c("sex", "otter", "relyear", "relmonth", "strandsite", "relage_d", "strandage_d", "condition" , "mei_rank", "mean_daily_ehs_sst",
                            "days_in", "surrogate", "max_relnum", "sst_loess_pred", "pdo_less_pred", "pdo_index", "percent_live_diet", "enso_loess_pred", 
                            "dens.sm", "x5yrtrend")

str(imp_summary_b)
imp_summary_b$variable <- as.factor(imp_summary_b$variable)

library(RColorBrewer)
library(forcats)

imp_summary_b <- imp_summary_b %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 

# 
# imp_summary_b$variable_cat <- c("Habitat", "Habitat", "Anthropogenic", "Habitat")
# imp_summary_b$sd <- c(sd_a, sd_sst, sd_chi, sd_chla)
# imp_summary_b$se <- imp_summary$sd/sqrt(length(imp_summary$sd))

# cols6 <-  rev(c(brewer.pal(11,"RdYlBu")[c(9,3)]))
# cols7 <- rev(c("#1b9e77", "#d95f02"))

varimp_b <- ggplot(imp_summary_b, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_b, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  # geom_errorbar(aes(ymin = PercentIncMSE-se, ymax = PercentIncMSE+se), width =0.05)+
  # scale_color_manual(values = cols7)+
 coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo


### scale raw all variable C

imp_summary_c <- as.data.frame(colMeans(imp_incMSE_c))
names(imp_summary_c)[1] <- "PercentIncMSE"
imp_summary_c$variable <- c("sex", "otter", "relyear", "relmonth", "strandsite", "relage_d", "strandage_d", "condition" , "mei_rank", "mean_daily_ehs_sst",
                            "days_in", "surrogate", "max_relnum", "sst_loess_pred", "pdo_less_pred", "pdo_index", "percent_live_diet", "enso_loess_pred", 
                            "dens.sm", "x5yrtrend")

str(imp_summary_c)
imp_summary_c$variable <- as.factor(imp_summary_c$variable)

imp_summary_c <- imp_summary_c %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 

varimp_c <- ggplot(imp_summary_c, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_c, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  # geom_errorbar(aes(ymin = PercentIncMSE-se, ymax = PercentIncMSE+se), width =0.05)+
  # scale_color_manual(values = cols7)+
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo


### scale loess top 50% var D

imp_summary_d <- as.data.frame(colMeans(imp_incMSE_d))
names(imp_summary_d)[1] <- "PercentIncMSE"
imp_summary_d$variable <- c("prop_foodtype_otter_sum",     
                            "relyear" ,                  
                            "enso_loess_pred" ,          
                            "X5yrtrend"    ,              
                            "surrogate"   ,             
                            "dens.sm " ,                    
                            "mei_rank" ,
                            "condition")

str(imp_summary_d)
imp_summary_d$variable <- as.factor(imp_summary_d$variable)

imp_summary_d <- imp_summary_d %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 

varimp_d <- ggplot(imp_summary_d, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_d, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  # geom_errorbar(aes(ymin = PercentIncMSE-se, ymax = PercentIncMSE+se), width =0.05)+
  # scale_color_manual(values = cols7)+
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo




### scale raw top 50% var E

imp_summary_e <- as.data.frame(colMeans(imp_incMSE_e))
names(imp_summary_e)[1] <- "PercentIncMSE"
imp_summary_e$variable <- c("mei_rank",     
                            "max_relnum" ,                  
                            "X5yrtrend" ,          
                            "condition"    ,              
                            "relage_d"   ,             
                            "enso_loess_pred" ,                    
                            "prop_foodtype_otter_sum" ,
                            "dens.sm")

str(imp_summary_e)
imp_summary_e$variable <- as.factor(imp_summary_e$variable)

imp_summary_e <- imp_summary_e %>%
  mutate(variable = fct_reorder(variable, PercentIncMSE)) 


varimp_e<- ggplot(imp_summary_e, aes(x = variable, y = PercentIncMSE))  +
  geom_segment(data = imp_summary_e, aes(x=variable, xend=variable, y=0, yend=PercentIncMSE), size = 5) +
  # geom_errorbar(aes(ymin = PercentIncMSE-se, ymax = PercentIncMSE+se), width =0.05)+
  # scale_color_manual(values = cols7)+
  coord_flip()+
  labs(x = NULL, y = "mean % increase MSE")+
  themeo

library(gridExtra)
grid.arrange(varimp_c, varimp_e, varimp_b, varimp_d,
             layout_matrix = rbind(c(1,2),
                                   c(3,4)))

##################################################################################
# visualizing models

################## model D: loess scale, top 50% variables
library(pdp)
## overlap many PDPs from many random forests

#here is one enso loess
pdp::partial(d_global, pred.var = "enso_loess_pred", plot = T, rug = 2)

#loop through and create 100 for 100 different RF splits and runs

ENSO_Loess_pdp_d <- NULL
for (i in id){
  
  sub_p <- id[i]
  
  #fit model
  rf <- randomForest(scale_loess_pred ~ prop_foodtype_otter_sum + relyear + enso_loess_pred + X5yrtrend + surrogate + dens.sm + relage_d, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], mtry = 2, ntree = 1000)
  
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

#plot on respsonse scale
ggplot()+
  geom_line(data = ENSO_Loess_pdp_d, aes(x = enso_loess_pred, y = yhat, group = ID),alpha = 0.25)+
  geom_smooth(data = ENSO_Loess_pdp_d, aes(x = enso_loess_pred, y = yhat))+
  geom_point(data = new_dd_EHS_nona, aes(x = enso_loess_pred, y = scale_loess_pred))+
  #scale_color_distiller(palette = 'BuGn', direction = 1)+
  theme_classic()





# simple pdp
pdp::partial(d_global, pred.var = "prop_foodtype_otter_sum", plot = T, rug = T)
pdp::partial(d_global, pred.var = "relyear", plot = T, rug = T)
pdp::partial(d_global, pred.var = "enso_loess_pred", plot = T, rug = T)
pdp::partial(d_global, pred.var = "X5yrtrend", plot = T, rug = T)
pdp::partial(d_global, pred.var = "surrogate", plot = T, rug = T)
pdp::partial(d_global, pred.var = "dens.sm", plot = T, rug = T)
pdp::partial(d_global, pred.var = "relage_d", plot = T, rug = T)

# ice function
ranger_ice <- function(object, newdata) {
  predict(object, newdata)#$predictions
}

# create ice objects
ice_enso_loess_d <- pdp::partial(d_global, pred.var = "enso_loess_pred", pred.fun = ranger_ice)

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
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

ice_diet_d<- pdp::partial(d_global, pred.var = "prop_foodtype_otter_sum", pred.fun = ranger_ice)
# plot enso_loess ICE plots
ice_diet_d <- ice_diet_d %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_diet_d_p <- ggplot(ice_diet_d, aes(prop_foodtype_otter_sum, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

ice_diet_d_p_cent <- ggplot(ice_diet_d, aes(prop_foodtype_otter_sum, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo

ice_relyear_d <- pdp::partial(d_global, pred.var = "relyear", pred.fun = ranger_ice)

# plot enso_loess ICE plots
ice_relyear_d <- ice_relyear_d %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_relyear_d_p <- ggplot(ice_relyear_d, aes(relyear, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

ice_relyear_d_p_cent <- ggplot(ice_relyear_d, aes(relyear, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo


ice_5yrtrendt_d <- pdp::partial(d_global, pred.var = "X5yrtrend", pred.fun = ranger_ice)

# plot enso_loess ICE plots
ice_5yrtrendt_d <- ice_5yrtrendt_d %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_5yrtrendt_d_p <- ggplot(ice_5yrtrendt_d, aes(X5yrtrend, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

ice_5yrtrendt_d_p_cent <- ggplot(ice_5yrtrendt_d, aes(X5yrtrend, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo



ice_surrogate_d <- pdp::partial(d_global, pred.var = "surrogate", pred.fun = ranger_ice)
# plot enso_loess ICE plots
ice_surrogate_d <- ice_surrogate_d %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_surrogate_d_p <- ggplot(ice_surrogate_d, aes(surrogate, yhat))+
  geom_point(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

ice_surrogate_d_p_cent <- ggplot(ice_surrogate_d, aes(surrogate, yhat.centered))+
  geom_point(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo


ice_dens_d <- pdp::partial(d_global, pred.var = "dens.sm", pred.fun = ranger_ice)

# plot enso_loess ICE plots
ice_dens_d <- ice_dens_d %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_dens_d_p <- ggplot(ice_dens_d, aes(dens.sm, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

ice_dens_d_p_cent <- ggplot(ice_dens_d, aes(dens.sm, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo



ice_relage_d <- pdp::partial(d_global, pred.var = "relage_d", pred.fun = ranger_ice)
# plot enso_loess ICE plots
ice_relage_d <- ice_relage_d %>%
  group_by(yhat.id) %>% # perform next operation within each yhat.id
  mutate(yhat.centered = yhat - first(yhat))

ice_relage_d_p <- ggplot(ice_relage_d, aes(relage_d, yhat))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  theme_classic()

ice_relage_d_p_cent <- ggplot(ice_relage_d, aes(relage_d, yhat.centered))+
  geom_line(aes(group = yhat.id), alpha = 0.3)+
  stat_summary(fun.y = mean, geom = "line", col = "#1b9e77", size = 1)+
  #labs(x = "chorophyll A concentration (mg/m3)", y = "partial yhat (centered)")+
  themeo


library(gridExtra)
grid.arrange(ice_enso_loess_d_p, ice_5yrtrendt_d_p, ice_dens_d_p, ice_diet_d_p, ice_relage_d_p, ice_relyear_d_p,
             layout_matrix = rbind(c(1,2,3),
                                   c(4,5,6)))

library(gridExtra)
grid.arrange(ice_enso_loess_d_p_cent, ice_5yrtrendt_d_p_cent, ice_dens_d_p_cent, ice_diet_d_p_cent, ice_relage_d_p_cent, ice_relyear_d_p_cent,
             layout_matrix = rbind(c(1,2,3),
                                   c(4,5,6)))


#visualize variable relationships


enso_loess_p_raw <- ggplot(dispersal_drivers_EHS_nona, aes(x = enso_loess_pred, y= scale_loess_pred ))+
  geom_point(show.legend = FALSE)+
  geom_smooth(color = "darkgrey")+
  themeo

dens.sm_p_raw <-ggplot(dispersal_drivers_EHS_nona, aes(x = dens.sm, y= scale_loess_pred ))+
  geom_point(show.legend = FALSE)+
  geom_smooth(span = 1, color = "darkgrey")+
  themeo

trend_p_raw <-ggplot(dispersal_drivers_EHS_nona, aes(x = X5yrtrend, y= scale_loess_pred ))+
  geom_point(show.legend = FALSE)+
  geom_smooth(span = 1, color = "darkgrey")+
  themeo

diet_p_raw <-ggplot(dispersal_drivers_EHS_nona, aes(x = prop_foodtype_otter_sum, y= scale_loess_pred ))+
  geom_point(show.legend = FALSE)+
  geom_smooth(color = "darkgrey")+
  themeo

relage_p_raw <-ggplot(dispersal_drivers_EHS_nona, aes(x = relage_d, y= scale_loess_pred ))+
  geom_point(show.legend = FALSE)+
  geom_smooth(color = "darkgrey")+
  themeo

relyear_p_raw <-ggplot(dispersal_drivers_EHS_nona, aes(x = relyear, y= scale_loess_pred ))+
  geom_point(show.legend = FALSE)+
  geom_smooth(color = "darkgrey")+
  themeo


library(gridExtra)
grid.arrange(ice_enso_loess_d_p_cent, ice_5yrtrendt_d_p_cent, ice_dens_d_p_cent, ice_diet_d_p_cent, ice_relage_d_p_cent,
             enso_loess_p_raw, trend_p_raw, dens.sm_p_raw, diet_p_raw, relage_p_raw,
             layout_matrix = rbind(c(6,1),
                                   c(7,2),
                                   c(8,3),
                                   c(9,4),
                                   c(10,5)))



ggplot(dispersal_drivers_EHS_nona, aes(x = prop_foodtype_otter_sum, y= scale ))+
  geom_point(aes(color = sex))+
  geom_smooth(method = "lm")+
  themeo

ggplot(dispersal_drivers_EHS_nona, aes(x = condition, y= scale_loess_pred ))+
  geom_point(aes(color = sex))+
  geom_smooth(span = 3)+
  themeo

ggplot(dispersal_drivers_EHS_nona, aes(x = days_in, y= scale_loess_pred ))+
  geom_point(aes(color = sex))+
  geom_smooth(span = 3)+
  themeo


ggplot(dispersal_drivers_EHS_nona, aes(x = X.x, y= prop_foodtype_otter_sum ))+
  geom_point(aes(color = sex))+
  geom_smooth(span = 3)+
  themeo

ggplot(dispersal_drivers_EHS_nona, aes(x = X.x, y= condition ))+
  geom_point(aes(color = sex))+
  geom_smooth(span = 3)+
  themeo

ggplot(dispersal_drivers_EHS_nona, aes(x = X.x, y= days_in ))+
  geom_point(aes(color = sex))+
  geom_smooth(span = 3)+
  themeo

plot(dispersal_drivers_EHS_nona$condition, dispersal_drivers_EHS_nona$scale_loess_pred)
plot(dispersal_drivers_EHS_nona$mei_rank, dispersal_drivers_EHS_nona$scale_loess_pred)
plot(dispersal_drivers_EHS_nona$enso_loess_pred, dispersal_drivers_EHS_nona$scale_loess_pred)
plot(dispersal_drivers_EHS_nona$dens.sm, dispersal_drivers_EHS_nona$scale_loess_pred)
plot(dispersal_drivers_EHS_nona$X5yrtrend, dispersal_drivers_EHS_nona$scale_loess_pred)

####################################
#### error calculations

# RF B
### R squared
# newdf <- data.frame(df, id=1+c(1:nrow(df))%%35)
# id <- unique(newdf$id)

r_squared_test <- NULL
r_squared_train <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  r2_train    <- (1 - (sum((new_dd_EHS_nona$scale_loess_pred-predict(rf, newdata = new_dd_EHS_nona))^2)/sum((new_dd_EHS_nona$scale_loess_pred-mean(new_dd_EHS_nona$scale_loess_pred))^2)))
  r2_test <- rf$rsq
  r_squared_test <- cbind(r_squared_test, r2_test)  
  r_squared_train <- cbind(r_squared_train, r2_train)  
}

mean(r_squared_test)
mean(r_squared_train)

# > mean(r_squared_test)
# [1] 0.3492321
# > mean(r_squared_train)
# [1] 0.8715153


### RMSE
# newdf <- data.frame(df, id=1+c(1:nrow(df))%%35)
# id <- unique(newdf$id)

RMSE_test <- NULL
RMSE_train <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  rmse_train    <- sqrt(mean((new_dd_EHS_nona$scale_loess_pred - predict(rf, newdata = new_dd_EHS_nona))^2) )
  rmse_test <- sqrt(rf$mse)
  RMSE_test <- cbind(RMSE_test, rmse_test)  
  RMSE_train <- cbind(RMSE_train, rmse_train)  
}

mean(RMSE_test)
mean(RMSE_train)

# > mean(RMSE_test)
# [1] 0.2970897
# > mean(RMSE_train)
# [1] 0.1304264

### MSE
# newdf <- data.frame(df, id=1+c(1:nrow(df))%%35)
# id <- unique(newdf$id)

MSE_test <- NULL
MSE_train <- NULL
for (i in id){
  
  
  rf <- randomForest(scale_loess_pred ~ sex  +seaotter + relyear+ relmonth + strandsiteATOS + relage_d + strandage_d + condition  + mei_rank +
                       mean_daily_ehs_sst + days_in + surrogate + max_relnum + sst_loess_pred + pdo_loess_pred + pdo_index +prop_foodtype_otter_sum +
                       enso_loess_pred + dens.sm + X5yrtrend, data = new_dd_EHS_nona[new_dd_EHS_nona$id!=i,], importance = TRUE )
  mse_train    <- mean((new_dd_EHS_nona$scale_loess_pred - predict(rf, newdata = new_dd_EHS_nona))^2) 
  mse_test <- rf$mse
  MSE_test <- cbind(MSE_test, mse_test)  
  MSE_train <- cbind(MSE_train, mse_train)  
}

mean(MSE_test)
mean(MSE_train)
# > mean(MSE_test)
# [1] 0.08886314
# > mean(MSE_train)
# [1] 0.01718622
