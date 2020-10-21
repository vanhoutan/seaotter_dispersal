##########################################################################

# Calculate lcp distances from resight data ##
# bring in needed packages

library(knitr)
library(sf)
library(sp)
library(tidyverse)
library(gdistance)
library(raster)

# map of resight area extent (greater Monterey bay north and south)
bay <- st_read("C:/Otterdat/resightextent/reightareasm.shp") %>% st_transform(crs = 32610)

# sea otter resight data
resights <- read.csv("C:/Users/tnicholson/Documents/dispersal_revisions/post_release_resights_tn.csv")

# select relevant fields
resightss <- resights %>% dplyr::select(seaotter, release, date, longitude, latitude)
resightss$otter <- resightss$seaotter
resightssu <- resightss %>% unite(otterr, otter, release)

# convert resights to sf objects
resightssusc <- st_as_sf(resightssu, coords = c("longitude", "latitude"),
                         crs = 4326, stringsAsFactors = FALSE) %>% st_transform(crs = 32610)

# create a RasterLayer object from our map
raster_bay <- raster(x = extent(bay), nrow = 800,  ncol = 600) #higher resolution than 800 600
raster_bay <- rasterize(x = bay, y = raster_bay, field = 1)
raster_bay[is.na(raster_bay)] <- 0

# create a transition matrix and apply corrections
raster_bay_tr <- transition(raster_bay, transitionFunction = mean, directions = 16)
raster_bay_tr <- geoCorrection(raster_bay_tr, type = "c")

#set up loop

resightssusc$otterr <- as.factor(resightssusc$otterr)
indotter <- levels(as.factor(resightssusc$otterr))
otter_lcps <- NULL

#run loop subsetting each otter, calculating lcps and euclidean distances between resights

for(i in 1:length(indotter)){
  
  
  subotter <- subset(resightssusc, otterr == indotter[i])
  
  
  resight <- st_as_sf(subotter, coords = c("longitude", "latitude"),
                      crs = 4326, stringsAsFactors = FALSE) %>% st_transform(crs = 32610)
  
  distall = NULL
  dist2all = NULL
  dist12 = NULL
  
  count <- nrow(subotter)-1
  
  for (ii in 1:count){
    
    dist1 <- st_distance(x = subotter[ii,], y = subotter[ii+1,])
    dist1df <- data.frame(dist1)
    distall <- rbind(distall, dist1df)
    dist2 <- costDistance(raster_bay_tr,
                          fromCoords = as(as_Spatial(subotter[ii,]), "SpatialPoints"),
                          toCoords = as(as_Spatial(subotter[ii+1,]), "SpatialPoints"))
    dist2df <- data.frame(dist2)
    dist2all <- rbind(dist2all, dist2df)
    
  }
  
  dist12 <- cbind(dist2all, distall)
  dist12$seaotter <- indotter[i]
  otter_lcps <- rbind(otter_lcps, dist12)
}


# replace 0 dist2 lcps with euclidean dist1 (for very small distances, all < 55m, below transition matrix resolution)

otter_lcps <- otter_lcps %>% mutate(dist2 = case_when(dist2 == 0 ~ dist1, TRUE ~ dist2))

# save lcps
write.csv(otter_lcps, "C:/Users/tnicholson/Documents/dispersal_revisions/resightlcp.csv")

#strip units strings from dist2 field or reload to continue
#otter_lcps[1] <-lapply(otter_lcps[1], function(x) as.numeric(sub("\\s +\\D +$", "", x)))

############################################################################

# compare cauchy, rayleigh, gamma model fits for each otter (from lcps)

#more packages
library(MASS)
library(VGAM)
library(fitdistrplus)
library(lubridate)

otter_lcps <- read.csv("C:/Users/tnicholson/Documents/dispersal_revisions/resightlcp.csv")
#combine all lcps by sea otter, not release


otter_lcps$otter <- otter_lcps$seaotter
otter_lcps$seaotter <- str_sub(otter_lcps$otter, start = 1, end = 3)

otter_mloglik = NULL
ind_otter <- levels(as.factor(otter_lcps$seaotter))


#run loop
for(i in 1:length(ind_otter)){
  
  #subset otters
  sub_seaotter <- subset(otter_lcps, seaotter == ind_otter[i])
  
  #fit distribution
  fit_cauchy_loop <- fitdistr(sub_seaotter$dist2,"cauchy", lower= c(0.01, 0.01))
  fit_rayleigh_loop <- fitdistr(sub_seaotter$dist2, drayleigh,start = list(scale = 1))
  fit_gamma_loop <- fitdistr(sub_seaotter$dist2,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
  
  #extract loglik
  sub_seaotter$log_lik_cauchy <- fit_cauchy_loop$loglik
  sub_seaotter$log_lik_rayleigh <- fit_rayleigh_loop$loglik
  sub_seaotter$log_lik_gamma <- fit_gamma_loop$loglik
  
  #bind to null dataframe
  otter_mloglik <- rbind(otter_mloglik, sub_seaotter)
}

#calculate log-likelihood
otter_mlogliksum <- otter_mloglik %>% group_by(seaotter) %>% 
  summarise(first(log_lik_cauchy), first(log_lik_rayleigh), first(log_lik_gamma), count = n()) 

otter_mlogliksum <- rename(otter_mlogliksum, LL_cauchy = `first(log_lik_cauchy)`, LL_rayleigh = `first(log_lik_rayleigh)`, LL_gamma = `first(log_lik_gamma)`)

#calculate AICc

otter_mlogliksum$cauchyAICc <- ((-2*otter_mlogliksum$LL_cauchy)+(2*2)) + 2*3*4/(otter_mlogliksum$count-2-2)
otter_mlogliksum$rayleighAICc <- ((-2*otter_mlogliksum$LL_rayleigh)+ (2*1)) + 2*2*3/(otter_mlogliksum$count-1-2)
otter_mlogliksum$gammaAICc <- ((-2*otter_mlogliksum$LL_gamma)+ (2*2)) + 2*2*3/(otter_mlogliksum$count-1-2)

AIC_table <- otter_mlogliksum %>%  dplyr::select(seaotter, cauchyAICc, rayleighAICc, gammaAICc)
AIC_rank <- data.frame(AIC_table, t(apply(AIC_table[,c(2:4)], 1, rank, ties.method='min')))  
AIC_diff_dist <- AIC_rank %>%  mutate(raleigh = rayleighAICc - pmin(rayleighAICc, gammaAICc, cauchyAICc), gamma = gammaAICc - pmin(rayleighAICc, gammaAICc, cauchyAICc), cauchy = cauchyAICc - pmin(rayleighAICc, gammaAICc, cauchyAICc))

write.csv(AIC_rank, file = "C:/Users/tnicholson/Documents/R/otter dispersal_revisions/AICrank.csv")
write.csv(AIC_diff_dist, file = "C:/Users/tnicholson/Documents/R/otter dispersal_revisions/AICdiffdist.csv")


############################################################################

# Fit lcps to Cauchy distribution to derive scale parameters and scale SE

otter_lcps <- read.csv("C:/Users/tnicholson/Documents/dispersal_revisions/resightlcp.csv")

#combine all lcps by sea otter, not release

otter_lcps$otter <- otter_lcps$seaotter
otter_lcps$seaotter <- str_sub(otter_lcps$otter, start = 1, end = 3)

#set up loop
ind_otter <- levels(as.factor(otter_lcps$seaotter))
otter_fits <- NULL

#run loop
for(i in 1:length(ind_otter)){
  
  #subset otters
  sub_otter <- subset(otter_lcps, seaotter == ind_otter[i])
  
  #fit distribution
  fit_cauchy_loop <- fitdistr(sub_otter$dist2,"cauchy", lower= c(0.01, 0.01))
  
  #extract cauchy parameters
  location_cauchy <- fit_cauchy_loop$estimate[1] %>% as.numeric()
  scale_cauchy <- fit_cauchy_loop$estimate[2] %>% as.numeric()
  location_sd <- fit_cauchy_loop$sd[1] %>% as.numeric()
  scale_sd <- fit_cauchy_loop$sd[2] %>% as.numeric()
  
  #generate column of fits in subset-ed data frame
  sub_otter$cauchy_estimates <- dcauchy(sub_otter$dist2, location = location_cauchy, scale = scale_cauchy, log = FALSE)
  sub_otter$location <- location_cauchy
  sub_otter$location_sd <- location_sd
  sub_otter$scale <- scale_cauchy
  sub_otter$scale_sd <- scale_sd
  sub_otter$n <- nrow(sub_otter)
  
  #bind to null dataframe
  otter_fits <- rbind(otter_fits, sub_otter)
}

#summarize cauchy scale parameter by sea otter, adjust to km scale
cauchy_scale <- otter_fits %>% group_by(seaotter) %>% summarise(cauchyscale = first(scale)/1000, cauchysd = first(scale_sd)/1000)

#########################################################################

# random forest models

library(randomForest)

#load in dispersal factors
dispersal_factors <- read.csv("C:/Users/tnicholson/documents/dispersal_revisions/dispersal_driversdmc42.csv")

### Variable importance
### global model with variable shortlist
dispersaldf <- data.frame(dispersal_factors, id=1+c(1:nrow(dispersal_factors))%%42)
id <- unique(dispersaldf$id)

dispersaldf$seaotter <- as.factor(dispersaldf$seaotter)
dispersaldf$sex <- as.factor(dispersaldf$sex)
dispersaldf$surrogate <- as.factor(dispersaldf$surrogate)

# calculate LOOCV sensitivity, R2, MSE, and Variable importance for each

# Variable importance
# final variables and loess scale as response

loo_ls_3v<- NULL

for(i in id){
  rf <- randomForest(lcauchyscale06 ~  #sex + relage_d + 
                    #percentlive +
                    enso_loess_pred + pupratio  + X5yrtrend, 
                    data = dispersaldf[dispersaldf$id!=i,],
                    importance = TRUE, ntree = 1500, mtry = 2 )
  imp <- importance(rf, newdata=dispersaldf[dispersaldf$id==i,])
  loo_ls_3v <- cbind(loo_ls_3v, imp)
}

imp_df_loo_ls_3v <- as.data.frame(t(loo_ls_3v)) # restructure output
imp_incMSE_ls_3v <- imp_df_loo_ls_3v[seq(1, nrow(imp_df_loo_ls_3v),2),] #just MSE

colMeans(imp_incMSE_ls_3v) # mean incMSE by factor



# error calculations
# R squared

r_squared_test_ls_3v <- NULL
r_squared_train_ls_3v <- NULL

for (i in id){
  
  rf <- randomForest(lcauchyscale06 ~ #sex +#relage_d + 
                    #percentlive +
                    enso_loess_pred + pupratio + X5yrtrend,
                    data = dispersaldf[dispersaldf$id!=i,], importance = TRUE, ntree = 1500, mtry = 2 )
  r2_train    <- (1 - (sum((dispersaldf$lcauchyscale06-predict(rf, newdata = dispersaldf))^2)/sum((dispersaldf$lcauchyscale06-mean(dispersaldf$lcauchyscale06))^2)))
  r2_test <- rf$rsq
  r_squared_test_ls_3v <- cbind(r_squared_test_ls_3v, r2_test)  
  r_squared_train_ls_3v <- cbind(r_squared_train_ls_3v, r2_train)  
}

mean(r_squared_test_ls_3v)
mean(r_squared_train_ls_3v)
mean(rf$mse)



# run 100 random forests varying loess cauchy scale by individual scale and sd

loo_ls_3v2<- NULL
r_squared_test_ls_3v2<-NULL
dispersal_factors$rel_date_reals <- lubridate::mdy(dispersal_factors$rel_date_real)


for(i in 1:100){
  
  dispersal_factors <- dispersal_factors %>% rowwise() %>% mutate(samplecauchy = rnorm(1, mean=cauchyscale, sd=cauchysd))
  replace(dispersal_factors$samplecauchy, dispersal_factors$samplecauchy<0, 0.01)
  cauchyscalel06 <- loess(samplecauchy ~ as.numeric(rel_date_reals), data = dispersal_factors, span = 0.6)
  dispersal_factors$cauchysl06 <- cauchyscalel06$fitted
  
  rf2 <- randomForest(lcauchyscale06 ~ #sex + relage_d + 
                      #percentlive +
                      enso_loess_pred + pupratio + X5yrtrend, 
                      data = dispersal_factors,
                      importance = TRUE, ntree = 1500, mtry = 2)
  imp2 <- importance(rf2)
  loo_ls_3v2 <- cbind(loo_ls_3v2, imp2)
  r2_test2 <- mean(rf2$rsq)
  r_squared_test_ls_3v2 <- cbind(r_squared_test_ls_3v2, r2_test2)  
}

imp_df_loo_ls_3v2 <- as.data.frame(t(loo_ls_3v2))
imp_incMSE_ls_3v2 <- imp_df_loo_ls_3v2[seq(1, nrow(imp_df_loo_ls_3v2),2),]
colMeans(imp_incMSE_ls_3v2)

r_squared_test_ls_3v2 <- as.data.frame(t(r_squared_test_ls_3v2))
colMeans(r_squared_test_ls_3v2)

#bind results

rfresults <- cbind(imp_incMSE_ls_3v2, r_squared_test_ls_3v2)


# tune model parameters mtry and ntree for three variable model
# create dataframe of r-squared values from possible combinations of mtree and ntree

library(ggplot2)

ntree <- rep(seq(from = 50, to = 2000, by = 50), times = 2)
mtry <- rep(2:3, each = 40)

dfrfparameters <- data.frame(mtry, ntree)
dfrfrsquare <- NULL

for (ii in 1:nrow(dfrfparameters)){
  
  resultrsquare <- NULL
  r_squared_test_ls_av <- NULL
  r_squared_train_ls_av <- NULL
  for (i in id){
    
    
    rf <- randomForest(lcauchyscale06 ~ #sex + relage_d + 
                         #percentlive +
                         enso_loess_pred + pupratio + X5yrtrend,
                       data = dispersaldf[dispersaldf$id!=i,], importance = TRUE, ntree = dfrfparameters$ntree[ii], mtry = dfrfparameters$mtry[ii] )
    r2_train    <- (1 - (sum((dispersaldf$lcauchyscale06-predict(rf, newdata = dispersaldf))^2)/sum((dispersaldf$lcauchyscale06-mean(dispersaldf$lcauchyscale06))^2)))
    r2_test <- rf$rsq
    r_squared_test_ls_av <- cbind(r_squared_test_ls_av, r2_test)  
    r_squared_train_ls_av <- cbind(r_squared_train_ls_av, r2_train)  
  }
  
  resultrsquare$test <- mean(r_squared_test_ls_av)
  resultrsquare$train <- mean(r_squared_train_ls_av)
  resultrsquare$mtry <- dfrfparameters$mtry[ii]
  resultrsquare$ntree <- dfrfparameters$ntree[ii]
  resultrsquare <- data.frame(resultrsquare)
  
  dfrfrsquare <- rbind(dfrfrsquare, resultrsquare)
}   


dfrfrsquare$mtry <- as.factor(dfrfrsquare$mtry)

# which.max(dfrfrsquare$test)

ggplot(dfrfrsquare, aes(x = ntree, y = test)) + 
  geom_point() +
  facet_wrap(~mtry)


