#add packages
library(here)
library(tidyverse)

library(sf)
library(rgdal)
library(raster)

library(MASS)
library(VGAM)
library(fitdistrplus)

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


resights_dist_recap_noNA <- read.csv(here("data_output/resights_dist_recap_nona.csv"))
resights_dist_recap <- read.csv(here("data_output/resights_dist_recap.csv"))


#set up loop
ind_otter <- levels(as.factor(resights_dist_recap_noNA$seaotter))
otter_fits <- NULL

#run loop
for(i in 1:length(ind_otter)){
  
  #subset otters
  sub_otter <- subset(resights_dist_recap_noNA, seaotter == ind_otter[i])
  
  #fit distribution
  fit_cauchy_loop <- fitdistr(sub_otter$dist,"cauchy", lower= c(0.01, 0.01))
  
  
  #extract parameters
  #cauchy
  location_cauchy <- fit_cauchy_loop$estimate[1] %>% as.numeric()
  scale_cauchy <- fit_cauchy_loop$estimate[2] %>% as.numeric()
  
  
  
  #generate column of fits in subsetted data frame
  sub_otter$cauchy_estimates <- dcauchy(sub_otter$dist, location = location_cauchy, scale = scale_cauchy, log = FALSE)
  sub_otter$location <- location_cauchy
  sub_otter$scale <- scale_cauchy
  
  #bind to null dataframe
  otter_fits <- rbind(otter_fits, sub_otter)
}
View(otter_fits)

write.csv(otter_fits, "data_output/otter_cauchy_fits.csv")

ggplot(otter_fits)+
  #geom_histogram(aes(x=dist, y = ..density..), binwidth = 5000)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1000)+
  geom_line(aes(x = dist, y = cauchy_estimates),stat = "identity", color = "red")+
  facet_wrap(~seaotter, scales = "free_y")+
  #coord_cartesian(ylim= c(0, 0.0002))+ #0.0002 * binwidth (5000) = 1
  coord_cartesian(ylim= c(0, 0.001))+ #0.001 * binwidth (1000) = 1
  themeo



cauchy_scale <- otter_fits %>% group_by(seaotter) %>% summarise(scale = first(scale))

write.csv(cauchy_scale, "data_output/cauchy_scale.csv")

#try to do this with map or broom?




#not all models would run in loop, so iterated through separately

unique(resights_dist_recap_noNA$seaotter)
# 209 238 225 228 238 249 252 269 286 315 327 339 344 353 379 386 433 451 457 
# 466 473 475 501 518 520 526 558 587 595 621 623 653 657 671 673
# 685 687 716 723 774 808 809

# 209
#create inidividual otter df for model fitting

#individual otter df
otter_209 <- resights_dist_recap %>% subset(seaotter == "209")
otter_209 <- as.data.frame(otter_209)
otter_209_noNA <- na.omit(otter_209)


#rayleigh
#fit model
fit_rayleigh_209 <- fitdistr(otter_209_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_209)

#gamma
#fit model
fit_gamma_209 <- fitdistr(otter_209_noNA$dist,dgamma, start = list(shape = 5000, rate = 5))
str(fit_gamma_209)

#weibull
#fit model
fit_weibull_209 <- fitdistr(otter_209_noNA$dist,"weibull")
str(fit_weibull_209)

#cauchy
#fit model
fit_cauchy_209 <- fitdistr(otter_209_noNA$dist,"cauchy")
str(fit_cauchy_209)

#estimates from models
otter_209_noNA$rayleigh <- drayleigh(otter_209_noNA$dist, scale = fit_rayleigh_209$estimate, log = FALSE)

otter_209_noNA$gamma <- dgamma(otter_209_noNA$dist, shape  = 14.45597, rate = 0.00268, log = FALSE)

otter_209_noNA$weibull <- dweibull(otter_209_noNA$dist, shape = 0.824, scale = 5080.39, log = FALSE)

otter_209_noNA$cauchy <- dcauchy(otter_209_noNA$dist, location = 2328, scale = 1804, log = FALSE)
#otter_209_noNA$cauchy2 <- dcauchy(otter_209_noNA$dist, location = fit_cauchy_209$estimate[1], scale = fit_cauchy$estimate_209[2], log = FALSE)


#save loglik
otter_209_noNA$log_lik_ray <- fit_rayleigh_209$loglik
otter_209_noNA$log_lik_gamma <- fit_gamma_209$loglik
otter_209_noNA$log_lik_weibull <- fit_weibull_209$loglik
otter_209_noNA$log_lik_cauchy <- fit_cauchy_209$loglik


#plot over histogram
ggplot(otter_209_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 217

#create inidividual otter df for model fitting

#individual otter df
otter_217 <- resights_dist_recap %>% subset(seaotter == "217")
otter_217 <- as.data.frame(otter_217)
otter_217_noNA <- na.omit(otter_217)


#rayleigh
#fit model
fit_rayleigh_217 <- fitdistr(otter_217_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_217)

#gamma
#fit model
fit_gamma_217 <- fitdistr(otter_217_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_217)

#weibull
#fit model
fit_weibull_217 <- fitdistr(otter_217_noNA$dist,"weibull")
str(fit_weibull_217)

#cauchy
#fit model
fit_cauchy_217 <- fitdistr(otter_217_noNA$dist,"cauchy")
str(fit_cauchy_217)

#estimates from models
otter_217_noNA$rayleigh <- drayleigh(otter_217_noNA$dist, scale = fit_rayleigh_217$estimate, log = FALSE)

otter_217_noNA$gamma <- dgamma(otter_217_noNA$dist, shape  = fit_gamma_217$estimate[1], rate = fit_gamma_217$estimate[2], log = FALSE)

otter_217_noNA$weibull <- dweibull(otter_217_noNA$dist, shape = fit_weibull_217$estimate[1], scale = fit_weibull_217$estimate[2], log = FALSE)

#otter_217_noNA$cauchy <- dcauchy(otter_217_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_217_noNA$cauchy <- dcauchy(otter_217_noNA$dist, location = fit_cauchy_217$estimate[1], scale = fit_cauchy_217$estimate[2], log = FALSE)


#save loglik
otter_217_noNA$log_lik_ray <- fit_rayleigh_217$loglik
otter_217_noNA$log_lik_gamma <- fit_gamma_217$loglik
otter_217_noNA$log_lik_weibull <- fit_weibull_217$loglik
otter_217_noNA$log_lik_cauchy <- fit_cauchy_217$loglik

#plot over histogram
ggplot(otter_217_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  



# 225 

#create inidividual otter df for model fitting

#individual otter df
otter_225 <- resights_dist_recap %>% subset(seaotter == "225")
otter_225 <- as.data.frame(otter_225)
otter_225_noNA <- na.omit(otter_225)


#rayleigh
#fit model
fit_rayleigh_225 <- fitdistr(otter_225_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_225)

#gamma
#fit model
fit_gamma_225 <- fitdistr(otter_225_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_225)

#weibull
#fit model
fit_weibull_225 <- fitdistr(otter_225_noNA$dist,"weibull")
str(fit_weibull_225)

#cauchy
#fit model
fit_cauchy_225 <- fitdistr(otter_225_noNA$dist,"cauchy")
str(fit_cauchy_225)

#estimates from models
otter_225_noNA$rayleigh <- drayleigh(otter_225_noNA$dist, scale = fit_rayleigh_225$estimate, log = FALSE)

otter_225_noNA$gamma <- dgamma(otter_225_noNA$dist, shape  = fit_gamma_225$estimate[1], rate = fit_gamma_225$estimate[2], log = FALSE)

otter_225_noNA$weibull <- dweibull(otter_225_noNA$dist, shape = fit_weibull_225$estimate[1], scale = fit_weibull_225$estimate[2], log = FALSE)

#otter_225_noNA$cauchy <- dcauchy(otter_225_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_225_noNA$cauchy <- dcauchy(otter_225_noNA$dist, location = fit_cauchy_225$estimate[1], scale = fit_cauchy_225$estimate[2], log = FALSE)

#save loglik
otter_225_noNA$log_lik_ray <- fit_rayleigh_225$loglik
otter_225_noNA$log_lik_gamma <- fit_gamma_225$loglik
otter_225_noNA$log_lik_weibull <- fit_weibull_225$loglik
otter_225_noNA$log_lik_cauchy <- fit_cauchy_225$loglik

#plot over histogram
ggplot(otter_225_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  



# 228 

#create inidividual otter df for model fitting

#individual otter df
otter_228 <- resights_dist_recap %>% subset(seaotter == "228")
otter_228 <- as.data.frame(otter_228)
otter_228_noNA <- na.omit(otter_228)


#rayleigh
#fit model
fit_rayleigh_228 <- fitdistr(otter_228_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_228)

#gamma
#fit model
fit_gamma_228 <- fitdistr(otter_228_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_228)

#weibull
#fit model
fit_weibull_228 <- fitdistr(otter_228_noNA$dist,"weibull")
str(fit_weibull_228)

#cauchy
#fit model
fit_cauchy_228 <- fitdistr(otter_228_noNA$dist,"cauchy")
str(fit_cauchy_228)

#estimates from models
otter_228_noNA$rayleigh <- drayleigh(otter_228_noNA$dist, scale = fit_rayleigh_228$estimate, log = FALSE)

otter_228_noNA$gamma <- dgamma(otter_228_noNA$dist, shape  = fit_gamma_228$estimate[1], rate = fit_gamma_228$estimate[2], log = FALSE)

otter_228_noNA$weibull <- dweibull(otter_228_noNA$dist, shape = fit_weibull_228$estimate[1], scale = fit_weibull_228$estimate[2], log = FALSE)

#otter_228_noNA$cauchy <- dcauchy(otter_228_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_228_noNA$cauchy <- dcauchy(otter_228_noNA$dist, location = fit_cauchy_228$estimate[1], scale = fit_cauchy_228$estimate[2], log = FALSE)

#save loglik
otter_228_noNA$log_lik_ray <- fit_rayleigh_228$loglik
otter_228_noNA$log_lik_gamma <- fit_gamma_228$loglik
otter_228_noNA$log_lik_weibull <- fit_weibull_228$loglik
otter_228_noNA$log_lik_cauchy <- fit_cauchy_228$loglik

#plot over histogram
ggplot(otter_228_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  



# 238 

#create inidividual otter df for model fitting

#individual otter df
otter_238 <- resights_dist_recap %>% subset(seaotter == "238")
otter_238 <- as.data.frame(otter_238)
otter_238_noNA <- na.omit(otter_238)


#rayleigh
#fit model
fit_rayleigh_238 <- fitdistr(otter_238_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_238)

#gamma
#fit model
fit_gamma_238 <- fitdistr(otter_238_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_238)

#weibull
#fit model
fit_weibull_238 <- fitdistr(otter_238_noNA$dist,"weibull")
str(fit_weibull_238)

#cauchy
#fit model
fit_cauchy_238 <- fitdistr(otter_238_noNA$dist,"cauchy")
str(fit_cauchy_238)

#estimates from models
otter_238_noNA$rayleigh <- drayleigh(otter_238_noNA$dist, scale = fit_rayleigh_238$estimate, log = FALSE)

otter_238_noNA$gamma <- dgamma(otter_238_noNA$dist, shape  = fit_gamma_238$estimate[1], rate = fit_gamma_238$estimate[2], log = FALSE)

otter_238_noNA$weibull <- dweibull(otter_238_noNA$dist, shape = fit_weibull_238$estimate[1], scale = fit_weibull_238$estimate[2], log = FALSE)

#otter_238_noNA$cauchy <- dcauchy(otter_238_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_238_noNA$cauchy <- dcauchy(otter_238_noNA$dist, location = fit_cauchy_238$estimate[1], scale = fit_cauchy_238$estimate[2], log = FALSE)


#save loglik
otter_238_noNA$log_lik_ray <- fit_rayleigh_238$loglik
otter_238_noNA$log_lik_gamma <- fit_gamma_238$loglik
otter_238_noNA$log_lik_weibull <- fit_weibull_238$loglik
otter_238_noNA$log_lik_cauchy <- fit_cauchy_238$loglik

#plot over histogram
ggplot(otter_238_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 249 
#create inidividual otter df for model fitting

#individual otter df
otter_249 <- resights_dist_recap %>% subset(seaotter == "249")
otter_249 <- as.data.frame(otter_249)
otter_249_noNA <- na.omit(otter_249)


#rayleigh
#fit model
fit_rayleigh_249 <- fitdistr(otter_249_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_249)

#gamma
#fit model
fit_gamma_249 <- fitdistr(otter_249_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_249)

#weibull
#fit model
fit_weibull_249 <- fitdistr(otter_249_noNA$dist,"weibull")
str(fit_weibull_249)

#cauchy
#fit model
fit_cauchy_249 <- fitdistr(otter_249_noNA$dist,"cauchy")
str(fit_cauchy_249)

#estimates from models
otter_249_noNA$rayleigh <- drayleigh(otter_249_noNA$dist, scale = fit_rayleigh_249$estimate, log = FALSE)

otter_249_noNA$gamma <- dgamma(otter_249_noNA$dist, shape  = fit_gamma_249$estimate[1], rate = fit_gamma_249$estimate[2], log = FALSE)

otter_249_noNA$weibull <- dweibull(otter_249_noNA$dist, shape = fit_weibull_249$estimate[1], scale = fit_weibull_249$estimate[2], log = FALSE)

#otter_249_noNA$cauchy <- dcauchy(otter_249_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_249_noNA$cauchy <- dcauchy(otter_249_noNA$dist, location = fit_cauchy_249$estimate[1], scale = fit_cauchy_249$estimate[2], log = FALSE)

#save loglik
otter_249_noNA$log_lik_ray <- fit_rayleigh_249$loglik
otter_249_noNA$log_lik_gamma <- fit_gamma_249$loglik
otter_249_noNA$log_lik_weibull <- fit_weibull_249$loglik
otter_249_noNA$log_lik_cauchy <- fit_cauchy_249$loglik

#plot over histogram
ggplot(otter_249_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 252 
#create inidividual otter df for model fitting

#individual otter df
otter_252 <- resights_dist_recap %>% subset(seaotter == "252")
otter_252 <- as.data.frame(otter_252)
otter_252_noNA <- na.omit(otter_252)


#rayleigh
#fit model
fit_rayleigh_252 <- fitdistr(otter_252_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_252)

#gamma
#fit model
fit_gamma_252 <- fitdistr(otter_252_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_252)

#weibull
#fit model
fit_weibull_252 <- fitdistr(otter_252_noNA$dist,"weibull")
str(fit_weibull_252)

#cauchy
#fit model
fit_cauchy_252 <- fitdistr(otter_252_noNA$dist,"cauchy")
str(fit_cauchy_252)

#estimates from models
otter_252_noNA$rayleigh <- drayleigh(otter_252_noNA$dist, scale = fit_rayleigh_252$estimate, log = FALSE)

otter_252_noNA$gamma <- dgamma(otter_252_noNA$dist, shape  = fit_gamma_252$estimate[1], rate = fit_gamma_252$estimate[2], log = FALSE)

otter_252_noNA$weibull <- dweibull(otter_252_noNA$dist, shape = fit_weibull_252$estimate[1], scale = fit_weibull_252$estimate[2], log = FALSE)

#otter_252_noNA$cauchy <- dcauchy(otter_252_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_252_noNA$cauchy <- dcauchy(otter_252_noNA$dist, location = fit_cauchy_252$estimate[1], scale = fit_cauchy_252$estimate[2], log = FALSE)



#save loglik
otter_252_noNA$log_lik_ray <- fit_rayleigh_252$loglik
otter_252_noNA$log_lik_gamma <- fit_gamma_252$loglik
otter_252_noNA$log_lik_weibull <- fit_weibull_252$loglik
otter_252_noNA$log_lik_cauchy <- fit_cauchy_252$loglik

#plot over histogram
ggplot(otter_252_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 269 

#create inidividual otter df for model fitting

#individual otter df
otter_269 <- resights_dist_recap %>% subset(seaotter == "269")
otter_269 <- as.data.frame(otter_269)
otter_269_noNA <- na.omit(otter_269)


#rayleigh
#fit model
fit_rayleigh_269 <- fitdistr(otter_269_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_269)

#gamma
#fit model
fit_gamma_269 <- fitdistr(otter_269_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_269)

#weibull
#fit model
fit_weibull_269 <- fitdistr(otter_269_noNA$dist,"weibull")
str(fit_weibull_269)

#cauchy
#fit model
fit_cauchy_269 <- fitdistr(otter_269_noNA$dist,"cauchy")
str(fit_cauchy_269)

#estimates from models
otter_269_noNA$rayleigh <- drayleigh(otter_269_noNA$dist, scale = fit_rayleigh_269$estimate, log = FALSE)

otter_269_noNA$gamma <- dgamma(otter_269_noNA$dist, shape  = fit_gamma_269$estimate[1], rate = fit_gamma_269$estimate[2], log = FALSE)

otter_269_noNA$weibull <- dweibull(otter_269_noNA$dist, shape = fit_weibull_269$estimate[1], scale = fit_weibull_269$estimate[2], log = FALSE)

#otter_269_noNA$cauchy <- dcauchy(otter_269_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_269_noNA$cauchy <- dcauchy(otter_269_noNA$dist, location = fit_cauchy_269$estimate[1], scale = fit_cauchy_269$estimate[2], log = FALSE)

#save loglik
otter_269_noNA$log_lik_ray <- fit_rayleigh_269$loglik
otter_269_noNA$log_lik_gamma <- fit_gamma_269$loglik
otter_269_noNA$log_lik_weibull <- fit_weibull_269$loglik
otter_269_noNA$log_lik_cauchy <- fit_cauchy_269$loglik

#plot over histogram
ggplot(otter_269_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 286

#create inidividual otter df for model fitting

#individual otter df
otter_286 <- resights_dist_recap %>% subset(seaotter == "286")
otter_286 <- as.data.frame(otter_286)
otter_286_noNA <- na.omit(otter_286)


#rayleigh
#fit model
fit_rayleigh_286 <- fitdistr(otter_286_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_286)

#gamma
#fit model
fit_gamma_286 <- fitdistr(otter_286_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_286)

#weibull
#fit model
fit_weibull_286 <- fitdistr(otter_286_noNA$dist,"weibull")
str(fit_weibull_286)

#cauchy
#fit model
fit_cauchy_286 <- fitdistr(otter_286_noNA$dist,"cauchy")
str(fit_cauchy_286)

#estimates from models
otter_286_noNA$rayleigh <- drayleigh(otter_286_noNA$dist, scale = fit_rayleigh_286$estimate, log = FALSE)

otter_286_noNA$gamma <- dgamma(otter_286_noNA$dist, shape  = fit_gamma_286$estimate[1], rate = fit_gamma_286$estimate[2], log = FALSE)

otter_286_noNA$weibull <- dweibull(otter_286_noNA$dist, shape = fit_weibull_286$estimate[1], scale = fit_weibull_286$estimate[2], log = FALSE)

#otter_286_noNA$cauchy <- dcauchy(otter_286_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_286_noNA$cauchy <- dcauchy(otter_286_noNA$dist, location = fit_cauchy_286$estimate[1], scale = fit_cauchy_286$estimate[2], log = FALSE)

#save loglik
otter_286_noNA$log_lik_ray <- fit_rayleigh_286$loglik
otter_286_noNA$log_lik_gamma <- fit_gamma_286$loglik
otter_286_noNA$log_lik_weibull <- fit_weibull_286$loglik
otter_286_noNA$log_lik_cauchy <- fit_cauchy_286$loglik

#plot over histogram
ggplot(otter_286_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 315

#create inidividual otter df for model fitting

#individual otter df
otter_315 <- resights_dist_recap %>% subset(seaotter == "315")
otter_315 <- as.data.frame(otter_315)
otter_315_noNA <- na.omit(otter_315)


#rayleigh
#fit model
fit_rayleigh_315 <- fitdistr(otter_315_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_315)

#gamma
#fit model
fit_gamma_315 <- fitdistr(otter_315_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_315)

#weibull
#fit model
fit_weibull_315 <- fitdistr(otter_315_noNA$dist,"weibull")
str(fit_weibull_315)

#cauchy
#fit model
fit_cauchy_315 <- fitdistr(otter_315_noNA$dist,"cauchy")
str(fit_cauchy_315)

#estimates from models
otter_315_noNA$rayleigh <- drayleigh(otter_315_noNA$dist, scale = fit_rayleigh_315$estimate, log = FALSE)

otter_315_noNA$gamma <- dgamma(otter_315_noNA$dist, shape  = fit_gamma_315$estimate[1], rate = fit_gamma_315$estimate[2], log = FALSE)

otter_315_noNA$weibull <- dweibull(otter_315_noNA$dist, shape = fit_weibull_315$estimate[1], scale = fit_weibull_315$estimate[2], log = FALSE)

#otter_315_noNA$cauchy <- dcauchy(otter_315_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_315_noNA$cauchy <- dcauchy(otter_315_noNA$dist, location = fit_cauchy_315$estimate[1], scale = fit_cauchy_315$estimate[2], log = FALSE)

#save loglik
otter_315_noNA$log_lik_ray <- fit_rayleigh_315$loglik
otter_315_noNA$log_lik_gamma <- fit_gamma_315$loglik
otter_315_noNA$log_lik_weibull <- fit_weibull_315$loglik
otter_315_noNA$log_lik_cauchy <- fit_cauchy_315$loglik

#plot over histogram
ggplot(otter_315_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 327

#create inidividual otter df for model fitting

#individual otter df
otter_327 <- resights_dist_recap %>% subset(seaotter == "327")
otter_327 <- as.data.frame(otter_327)
otter_327_noNA <- na.omit(otter_327)


#rayleigh
#fit model
fit_rayleigh_327 <- fitdistr(otter_327_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_327)

#gamma
#fit model
fit_gamma_327 <- fitdistr(otter_327_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_327)

#weibull
#fit model
fit_weibull_327 <- fitdistr(otter_327_noNA$dist,"weibull")
str(fit_weibull_327)

#cauchy
#fit model
fit_cauchy_327 <- fitdistr(otter_327_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_327)

#estimates from models
otter_327_noNA$rayleigh <- drayleigh(otter_327_noNA$dist, scale = fit_rayleigh_327$estimate, log = FALSE)

otter_327_noNA$gamma <- dgamma(otter_327_noNA$dist, shape  = fit_gamma_327$estimate[1], rate = fit_gamma_327$estimate[2], log = FALSE)

otter_327_noNA$weibull <- dweibull(otter_327_noNA$dist, shape = fit_weibull_327$estimate[1], scale = fit_weibull_327$estimate[2], log = FALSE)

#otter_327_noNA$cauchy <- dcauchy(otter_327_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_327_noNA$cauchy <- dcauchy(otter_327_noNA$dist, location = fit_cauchy_327$estimate[1], scale = fit_cauchy_327$estimate[2], log = FALSE)

#save loglik
otter_327_noNA$log_lik_ray <- fit_rayleigh_327$loglik
otter_327_noNA$log_lik_gamma <- fit_gamma_327$loglik
otter_327_noNA$log_lik_weibull <- fit_weibull_327$loglik
otter_327_noNA$log_lik_cauchy <- fit_cauchy_327$loglik

#plot over histogram
ggplot(otter_327_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 339 

#create inidividual otter df for model fitting

#individual otter df
otter_339 <- resights_dist_recap %>% subset(seaotter == "339")
otter_339 <- as.data.frame(otter_339)
otter_339_noNA <- na.omit(otter_339)


#rayleigh
#fit model
fit_rayleigh_339 <- fitdistr(otter_339_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_339)

#gamma
#fit model
fit_gamma_339 <- fitdistr(otter_339_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_339)

#weibull
#fit model
fit_weibull_339 <- fitdistr(otter_339_noNA$dist,"weibull")
str(fit_weibull_339)

#cauchy
#fit model
fit_cauchy_339 <- fitdistr(otter_339_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_339)

#estimates from models
otter_339_noNA$rayleigh <- drayleigh(otter_339_noNA$dist, scale = fit_rayleigh_339$estimate, log = FALSE)

otter_339_noNA$gamma <- dgamma(otter_339_noNA$dist, shape  = fit_gamma_339$estimate[1], rate = fit_gamma_339$estimate[2], log = FALSE)

otter_339_noNA$weibull <- dweibull(otter_339_noNA$dist, shape = fit_weibull_339$estimate[1], scale = fit_weibull_339$estimate[2], log = FALSE)

#otter_339_noNA$cauchy <- dcauchy(otter_339_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_339_noNA$cauchy <- dcauchy(otter_339_noNA$dist, location = fit_cauchy_339$estimate[1], scale = fit_cauchy_339$estimate[2], log = FALSE)

#save loglik
otter_339_noNA$log_lik_ray <- fit_rayleigh_339$loglik
otter_339_noNA$log_lik_gamma <- fit_gamma_339$loglik
otter_339_noNA$log_lik_weibull <- fit_weibull_339$loglik
otter_339_noNA$log_lik_cauchy <- fit_cauchy_339$loglik

#plot over histogram
ggplot(otter_339_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 344
#create inidividual otter df for model fitting

#individual otter df
otter_344 <- resights_dist_recap %>% subset(seaotter == "344")
otter_344 <- as.data.frame(otter_344)
otter_344_noNA <- na.omit(otter_344)


#rayleigh
#fit model
fit_rayleigh_344 <- fitdistr(otter_344_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_344)

#gamma
#fit model
fit_gamma_344 <- fitdistr(otter_344_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_344)

#weibull
#fit model
fit_weibull_344 <- fitdistr(otter_344_noNA$dist,"weibull")
str(fit_weibull_344)

#cauchy
#fit model
fit_cauchy_344 <- fitdistr(otter_344_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_344)

#estimates from models
otter_344_noNA$rayleigh <- drayleigh(otter_344_noNA$dist, scale = fit_rayleigh_344$estimate, log = FALSE)

otter_344_noNA$gamma <- dgamma(otter_344_noNA$dist, shape  = fit_gamma_344$estimate[1], rate = fit_gamma_344$estimate[2], log = FALSE)

otter_344_noNA$weibull <- dweibull(otter_344_noNA$dist, shape = fit_weibull_344$estimate[1], scale = fit_weibull_344$estimate[2], log = FALSE)

#otter_344_noNA$cauchy <- dcauchy(otter_344_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_344_noNA$cauchy <- dcauchy(otter_344_noNA$dist, location = fit_cauchy_344$estimate[1], scale = fit_cauchy_344$estimate[2], log = FALSE)

#save loglik
otter_344_noNA$log_lik_ray <- fit_rayleigh_344$loglik
otter_344_noNA$log_lik_gamma <- fit_gamma_344$loglik
otter_344_noNA$log_lik_weibull <- fit_weibull_344$loglik
otter_344_noNA$log_lik_cauchy <- fit_cauchy_344$loglik

#plot over histogram
ggplot(otter_344_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 353

#create inidividual otter df for model fitting

#individual otter df
otter_353 <- resights_dist_recap %>% subset(seaotter == "353")
otter_353 <- as.data.frame(otter_353)
otter_353_noNA <- na.omit(otter_353)


#rayleigh
#fit model
fit_rayleigh_353 <- fitdistr(otter_353_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_353)

#gamma
#fit model
fit_gamma_353 <- fitdistr(otter_353_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_353)

#weibull
#fit model
fit_weibull_353 <- fitdistr(otter_353_noNA$dist,"weibull")
str(fit_weibull_353)

#cauchy
#fit model
fit_cauchy_353 <- fitdistr(otter_353_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_353)

#estimates from models
otter_353_noNA$rayleigh <- drayleigh(otter_353_noNA$dist, scale = fit_rayleigh_353$estimate, log = FALSE)

otter_353_noNA$gamma <- dgamma(otter_353_noNA$dist, shape  = fit_gamma_353$estimate[1], rate = fit_gamma_353$estimate[2], log = FALSE)

otter_353_noNA$weibull <- dweibull(otter_353_noNA$dist, shape = fit_weibull_353$estimate[1], scale = fit_weibull_353$estimate[2], log = FALSE)

#otter_353_noNA$cauchy <- dcauchy(otter_353_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_353_noNA$cauchy <- dcauchy(otter_353_noNA$dist, location = fit_cauchy_353$estimate[1], scale = fit_cauchy_353$estimate[2], log = FALSE)

#save loglik
otter_353_noNA$log_lik_ray <- fit_rayleigh_353$loglik
otter_353_noNA$log_lik_gamma <- fit_gamma_353$loglik
otter_353_noNA$log_lik_weibull <- fit_weibull_353$loglik
otter_353_noNA$log_lik_cauchy <- fit_cauchy_353$loglik

#plot over histogram
ggplot(otter_353_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 379
#create inidividual otter df for model fitting

#individual otter df
otter_379 <- resights_dist_recap %>% subset(seaotter == "379")
otter_379 <- as.data.frame(otter_379)
otter_379_noNA <- na.omit(otter_379)


# #rayleigh
# #fit model
# fit_rayleigh_379 <- fitdistr(otter_379_noNA$dist,  drayleigh,start = list(scale = 5), lower = 0.01)
# str(fit_rayleigh_379)

# #gamma
# #fit model
# fit_gamma_379 <- fitdistr(otter_379_noNA$dist,dgamma, start = list(shape = 15, rate = .01), lower = 0.01)
# str(fit_gamma_379)
# 
# #weibull
# #fit model
# fit_weibull_379 <- fitdistr(otter_379_noNA$dist,dweibull, start = list(shape = 0.5 ,scale =3000), lower = 0.01)
# str(fit_weibull_379)
# 
#cauchy
#fit model
fit_cauchy_379 <- fitdistr(otter_379_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_379)
# 
# #estimates from models
# otter_379_noNA$rayleigh <- drayleigh(otter_379_noNA$dist, scale = fit_rayleigh_379$estimate, log = FALSE)
# 
# otter_379_noNA$gamma <- dgamma(otter_379_noNA$dist, shape  = fit_gamma_379$estimate[1], rate = fit_gamma_379$estimate[2], log = FALSE)
# 
# otter_379_noNA$weibull <- dweibull(otter_379_noNA$dist, shape = fit_weibull_379$estimate[1], scale = fit_weibull_379$estimate[2], log = FALSE)

#otter_379_noNA$cauchy <- dcauchy(otter_379_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_379_noNA$cauchy <- dcauchy(otter_379_noNA$dist, location = fit_cauchy_379$estimate[1], scale = fit_cauchy_379$estimate[2], log = FALSE)

#plot over histogram
ggplot(otter_379_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 500)+
  # geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  # geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  # geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo


# 386

#create inidividual otter df for model fitting

#individual otter df
otter_386 <- resights_dist_recap %>% subset(seaotter == "386")
otter_386 <- as.data.frame(otter_386)
otter_386_noNA <- na.omit(otter_386)


#rayleigh
#fit model
fit_rayleigh_386 <- fitdistr(otter_386_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_386)

#gamma
#fit model
fit_gamma_386 <- fitdistr(otter_386_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_386)

#weibull
#fit model
fit_weibull_386 <- fitdistr(otter_386_noNA$dist,"weibull")
str(fit_weibull_386)

#cauchy
#fit model
fit_cauchy_386 <- fitdistr(otter_386_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_386)

#estimates from models
otter_386_noNA$rayleigh <- drayleigh(otter_386_noNA$dist, scale = fit_rayleigh_386$estimate, log = FALSE)

otter_386_noNA$gamma <- dgamma(otter_386_noNA$dist, shape  = fit_gamma_386$estimate[1], rate = fit_gamma_386$estimate[2], log = FALSE)

otter_386_noNA$weibull <- dweibull(otter_386_noNA$dist, shape = fit_weibull_386$estimate[1], scale = fit_weibull_386$estimate[2], log = FALSE)

#otter_386_noNA$cauchy <- dcauchy(otter_386_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_386_noNA$cauchy <- dcauchy(otter_386_noNA$dist, location = fit_cauchy_386$estimate[1], scale = fit_cauchy_386$estimate[2], log = FALSE)

#save loglik
otter_386_noNA$log_lik_ray <- fit_rayleigh_386$loglik
otter_386_noNA$log_lik_gamma <- fit_gamma_386$loglik
otter_386_noNA$log_lik_weibull <- fit_weibull_386$loglik
otter_386_noNA$log_lik_cauchy <- fit_cauchy_386$loglik

#plot over histogram
ggplot(otter_386_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 433

#create inidividual otter df for model fitting

#individual otter df
otter_433 <- resights_dist_recap %>% subset(seaotter == "433")
otter_433 <- as.data.frame(otter_433)
otter_433_noNA <- na.omit(otter_433)


#rayleigh
#fit model
fit_rayleigh_433 <- fitdistr(otter_433_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_433)

#gamma
#fit model
fit_gamma_433 <- fitdistr(otter_433_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_433)

#weibull
#fit model
fit_weibull_433 <- fitdistr(otter_433_noNA$dist,"weibull")
str(fit_weibull_433)

#cauchy
#fit model
fit_cauchy_433 <- fitdistr(otter_433_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_433)

#estimates from models
otter_433_noNA$rayleigh <- drayleigh(otter_433_noNA$dist, scale = fit_rayleigh_433$estimate, log = FALSE)

otter_433_noNA$gamma <- dgamma(otter_433_noNA$dist, shape  = fit_gamma_433$estimate[1], rate = fit_gamma_433$estimate[2], log = FALSE)

otter_433_noNA$weibull <- dweibull(otter_433_noNA$dist, shape = fit_weibull_433$estimate[1], scale = fit_weibull_433$estimate[2], log = FALSE)

#otter_433_noNA$cauchy <- dcauchy(otter_433_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_433_noNA$cauchy <- dcauchy(otter_433_noNA$dist, location = fit_cauchy_433$estimate[1], scale = fit_cauchy_433$estimate[2], log = FALSE)

#save loglik
otter_433_noNA$log_lik_ray <- fit_rayleigh_433$loglik
otter_433_noNA$log_lik_gamma <- fit_gamma_433$loglik
otter_433_noNA$log_lik_weibull <- fit_weibull_433$loglik
otter_433_noNA$log_lik_cauchy <- fit_cauchy_433$loglik

#plot over histogram
ggplot(otter_433_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 451 

#create inidividual otter df for model fitting

#individual otter df
otter_451 <- resights_dist_recap %>% subset(seaotter == "451")
otter_451 <- as.data.frame(otter_451)
otter_451_noNA <- na.omit(otter_451)


#rayleigh
#fit model
fit_rayleigh_451 <- fitdistr(otter_451_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_451)

#gamma
#fit model
fit_gamma_451 <- fitdistr(otter_451_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_451)

#weibull
#fit model
fit_weibull_451 <- fitdistr(otter_451_noNA$dist,"weibull")
str(fit_weibull_451)

#cauchy
#fit model
fit_cauchy_451 <- fitdistr(otter_451_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_451)

#estimates from models
otter_451_noNA$rayleigh <- drayleigh(otter_451_noNA$dist, scale = fit_rayleigh_451$estimate, log = FALSE)

otter_451_noNA$gamma <- dgamma(otter_451_noNA$dist, shape  = fit_gamma_451$estimate[1], rate = fit_gamma_451$estimate[2], log = FALSE)

otter_451_noNA$weibull <- dweibull(otter_451_noNA$dist, shape = fit_weibull_451$estimate[1], scale = fit_weibull_451$estimate[2], log = FALSE)

#otter_451_noNA$cauchy <- dcauchy(otter_451_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_451_noNA$cauchy <- dcauchy(otter_451_noNA$dist, location = fit_cauchy_451$estimate[1], scale = fit_cauchy_451$estimate[2], log = FALSE)

#save loglik
otter_451_noNA$log_lik_ray <- fit_rayleigh_451$loglik
otter_451_noNA$log_lik_gamma <- fit_gamma_451$loglik
otter_451_noNA$log_lik_weibull <- fit_weibull_451$loglik
otter_451_noNA$log_lik_cauchy <- fit_cauchy_451$loglik

#plot over histogram
ggplot(otter_451_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 457 

#create inidividual otter df for model fitting

#individual otter df
otter_457 <- resights_dist_recap %>% subset(seaotter == "457")
otter_457 <- as.data.frame(otter_457)
otter_457_noNA <- na.omit(otter_457)


#rayleigh
#fit model
fit_rayleigh_457 <- fitdistr(otter_457_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_457)

#gamma
#fit model
fit_gamma_457 <- fitdistr(otter_457_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_457)

#weibull
#fit model
fit_weibull_457 <- fitdistr(otter_457_noNA$dist,"weibull")
str(fit_weibull_457)

#cauchy
#fit model
fit_cauchy_457 <- fitdistr(otter_457_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_457)

#estimates from models
otter_457_noNA$rayleigh <- drayleigh(otter_457_noNA$dist, scale = fit_rayleigh_457$estimate, log = FALSE)

otter_457_noNA$gamma <- dgamma(otter_457_noNA$dist, shape  = fit_gamma_457$estimate[1], rate = fit_gamma_457$estimate[2], log = FALSE)

otter_457_noNA$weibull <- dweibull(otter_457_noNA$dist, shape = fit_weibull_457$estimate[1], scale = fit_weibull_457$estimate[2], log = FALSE)

#otter_457_noNA$cauchy <- dcauchy(otter_457_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_457_noNA$cauchy <- dcauchy(otter_457_noNA$dist, location = fit_cauchy_457$estimate[1], scale = fit_cauchy_457$estimate[2], log = FALSE)

#save loglik
otter_457_noNA$log_lik_ray <- fit_rayleigh_457$loglik
otter_457_noNA$log_lik_gamma <- fit_gamma_457$loglik
otter_457_noNA$log_lik_weibull <- fit_weibull_457$loglik
otter_457_noNA$log_lik_cauchy <- fit_cauchy_457$loglik

#plot over histogram
ggplot(otter_457_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 466

#create inidividual otter df for model fitting

#individual otter df
otter_466 <- resights_dist_recap %>% subset(seaotter == "466")
otter_466 <- as.data.frame(otter_466)
otter_466_noNA <- na.omit(otter_466)


#rayleigh
#fit model
fit_rayleigh_466 <- fitdistr(otter_466_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_466)

#gamma
#fit model
fit_gamma_466 <- fitdistr(otter_466_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_466)

#weibull
#fit model
fit_weibull_466 <- fitdistr(otter_466_noNA$dist,"weibull")
str(fit_weibull_466)

#cauchy
#fit model
fit_cauchy_466 <- fitdistr(otter_466_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_466)

#estimates from models
otter_466_noNA$rayleigh <- drayleigh(otter_466_noNA$dist, scale = fit_rayleigh_466$estimate, log = FALSE)

otter_466_noNA$gamma <- dgamma(otter_466_noNA$dist, shape  = fit_gamma_466$estimate[1], rate = fit_gamma_466$estimate[2], log = FALSE)

otter_466_noNA$weibull <- dweibull(otter_466_noNA$dist, shape = fit_weibull_466$estimate[1], scale = fit_weibull_466$estimate[2], log = FALSE)

#otter_466_noNA$cauchy <- dcauchy(otter_466_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_466_noNA$cauchy <- dcauchy(otter_466_noNA$dist, location = fit_cauchy_466$estimate[1], scale = fit_cauchy_466$estimate[2], log = FALSE)

#save loglik
otter_466_noNA$log_lik_ray <- fit_rayleigh_466$loglik
otter_466_noNA$log_lik_gamma <- fit_gamma_466$loglik
otter_466_noNA$log_lik_weibull <- fit_weibull_466$loglik
otter_466_noNA$log_lik_cauchy <- fit_cauchy_466$loglik

#plot over histogram
ggplot(otter_466_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 473 

#create inidividual otter df for model fitting

#individual otter df
otter_473 <- resights_dist_recap %>% subset(seaotter == "473")
otter_473 <- as.data.frame(otter_473)
otter_473_noNA <- na.omit(otter_473)


#rayleigh
#fit model
fit_rayleigh_473 <- fitdistr(otter_473_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_473)

#gamma
#fit model
fit_gamma_473 <- fitdistr(otter_473_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_473)

#weibull
#fit model
fit_weibull_473 <- fitdistr(otter_473_noNA$dist,"weibull")
str(fit_weibull_473)

#cauchy
#fit model
fit_cauchy_473 <- fitdistr(otter_473_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_473)

#estimates from models
otter_473_noNA$rayleigh <- drayleigh(otter_473_noNA$dist, scale = fit_rayleigh_473$estimate, log = FALSE)

otter_473_noNA$gamma <- dgamma(otter_473_noNA$dist, shape  = fit_gamma_473$estimate[1], rate = fit_gamma_473$estimate[2], log = FALSE)

otter_473_noNA$weibull <- dweibull(otter_473_noNA$dist, shape = fit_weibull_473$estimate[1], scale = fit_weibull_473$estimate[2], log = FALSE)

#otter_473_noNA$cauchy <- dcauchy(otter_473_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_473_noNA$cauchy <- dcauchy(otter_473_noNA$dist, location = fit_cauchy_473$estimate[1], scale = fit_cauchy_473$estimate[2], log = FALSE)

#save loglik
otter_473_noNA$log_lik_ray <- fit_rayleigh_473$loglik
otter_473_noNA$log_lik_gamma <- fit_gamma_473$loglik
otter_473_noNA$log_lik_weibull <- fit_weibull_473$loglik
otter_473_noNA$log_lik_cauchy <- fit_cauchy_473$loglik

#plot over histogram
ggplot(otter_473_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 250)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 475 

#create inidividual otter df for model fitting

#individual otter df
otter_475 <- resights_dist_recap %>% subset(seaotter == "475")
otter_475 <- as.data.frame(otter_475)
otter_475_noNA <- na.omit(otter_475)


#rayleigh
#fit model
fit_rayleigh_475 <- fitdistr(otter_475_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_475)

#gamma
#fit model
fit_gamma_475 <- fitdistr(otter_475_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_475)

#weibull
#fit model
fit_weibull_475 <- fitdistr(otter_475_noNA$dist,"weibull")
str(fit_weibull_475)

#cauchy
#fit model
fit_cauchy_475 <- fitdistr(otter_475_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_475)

#estimates from models
otter_475_noNA$rayleigh <- drayleigh(otter_475_noNA$dist, scale = fit_rayleigh_475$estimate, log = FALSE)

otter_475_noNA$gamma <- dgamma(otter_475_noNA$dist, shape  = fit_gamma_475$estimate[1], rate = fit_gamma_475$estimate[2], log = FALSE)

otter_475_noNA$weibull <- dweibull(otter_475_noNA$dist, shape = fit_weibull_475$estimate[1], scale = fit_weibull_475$estimate[2], log = FALSE)

#otter_475_noNA$cauchy <- dcauchy(otter_475_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_475_noNA$cauchy <- dcauchy(otter_475_noNA$dist, location = fit_cauchy_475$estimate[1], scale = fit_cauchy_475$estimate[2], log = FALSE)

#save loglik
otter_475_noNA$log_lik_ray <- fit_rayleigh_475$loglik
otter_475_noNA$log_lik_gamma <- fit_gamma_475$loglik
otter_475_noNA$log_lik_weibull <- fit_weibull_475$loglik
otter_475_noNA$log_lik_cauchy <- fit_cauchy_475$loglik

#plot over histogram
ggplot(otter_475_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 501

#create inidividual otter df for model fitting

#individual otter df
otter_501 <- resights_dist_recap %>% subset(seaotter == "501")
otter_501 <- as.data.frame(otter_501)
otter_501_noNA <- na.omit(otter_501)


#rayleigh
#fit model
fit_rayleigh_501 <- fitdistr(otter_501_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_501)

#gamma
#fit model
fit_gamma_501 <- fitdistr(otter_501_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_501)

#weibull
#fit model
fit_weibull_501 <- fitdistr(otter_501_noNA$dist,"weibull")
str(fit_weibull_501)

#cauchy
#fit model
fit_cauchy_501 <- fitdistr(otter_501_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_501)

#estimates from models
otter_501_noNA$rayleigh <- drayleigh(otter_501_noNA$dist, scale = fit_rayleigh_501$estimate, log = FALSE)

otter_501_noNA$gamma <- dgamma(otter_501_noNA$dist, shape  = fit_gamma_501$estimate[1], rate = fit_gamma_501$estimate[2], log = FALSE)

otter_501_noNA$weibull <- dweibull(otter_501_noNA$dist, shape = fit_weibull_501$estimate[1], scale = fit_weibull_501$estimate[2], log = FALSE)

#otter_501_noNA$cauchy <- dcauchy(otter_501_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_501_noNA$cauchy <- dcauchy(otter_501_noNA$dist, location = fit_cauchy_501$estimate[1], scale = fit_cauchy_501$estimate[2], log = FALSE)

#save loglik
otter_501_noNA$log_lik_ray <- fit_rayleigh_501$loglik
otter_501_noNA$log_lik_gamma <- fit_gamma_501$loglik
otter_501_noNA$log_lik_weibull <- fit_weibull_501$loglik
otter_501_noNA$log_lik_cauchy <- fit_cauchy_501$loglik

#plot over histogram
ggplot(otter_501_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 518 

#create inidividual otter df for model fitting

#individual otter df
otter_518 <- resights_dist_recap %>% subset(seaotter == "518")
otter_518 <- as.data.frame(otter_518)
otter_518_noNA <- na.omit(otter_518)


#rayleigh
#fit model
fit_rayleigh_518 <- fitdistr(otter_518_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_518)

#gamma
#fit model
fit_gamma_518 <- fitdistr(otter_518_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_518)

#weibull
#fit model
fit_weibull_518 <- fitdistr(otter_518_noNA$dist,"weibull")
str(fit_weibull_518)

#cauchy
#fit model
fit_cauchy_518 <- fitdistr(otter_518_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_518)

#estimates from models
otter_518_noNA$rayleigh <- drayleigh(otter_518_noNA$dist, scale = fit_rayleigh_518$estimate, log = FALSE)

otter_518_noNA$gamma <- dgamma(otter_518_noNA$dist, shape  = fit_gamma_518$estimate[1], rate = fit_gamma_518$estimate[2], log = FALSE)

otter_518_noNA$weibull <- dweibull(otter_518_noNA$dist, shape = fit_weibull_518$estimate[1], scale = fit_weibull_518$estimate[2], log = FALSE)

#otter_518_noNA$cauchy <- dcauchy(otter_518_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_518_noNA$cauchy <- dcauchy(otter_518_noNA$dist, location = fit_cauchy_518$estimate[1], scale = fit_cauchy_518$estimate[2], log = FALSE)

#save loglik
otter_518_noNA$log_lik_ray <- fit_rayleigh_518$loglik
otter_518_noNA$log_lik_gamma <- fit_gamma_518$loglik
otter_518_noNA$log_lik_weibull <- fit_weibull_518$loglik
otter_518_noNA$log_lik_cauchy <- fit_cauchy_518$loglik

#plot over histogram
ggplot(otter_518_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 250)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 



# 520 

#create inidividual otter df for model fitting

#individual otter df
otter_520 <- resights_dist_recap %>% subset(seaotter == "520")
otter_520 <- as.data.frame(otter_520)
otter_520_noNA <- na.omit(otter_520)


#rayleigh
#fit model
fit_rayleigh_520 <- fitdistr(otter_520_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_520)

#gamma
#fit model
fit_gamma_520 <- fitdistr(otter_520_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_520)

#weibull
#fit model
fit_weibull_520 <- fitdistr(otter_520_noNA$dist,"weibull")
str(fit_weibull_520)

#cauchy
#fit model
fit_cauchy_520 <- fitdistr(otter_520_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_520)

#estimates from models
otter_520_noNA$rayleigh <- drayleigh(otter_520_noNA$dist, scale = fit_rayleigh_520$estimate, log = FALSE)

otter_520_noNA$gamma <- dgamma(otter_520_noNA$dist, shape  = fit_gamma_520$estimate[1], rate = fit_gamma_520$estimate[2], log = FALSE)

otter_520_noNA$weibull <- dweibull(otter_520_noNA$dist, shape = fit_weibull_520$estimate[1], scale = fit_weibull_520$estimate[2], log = FALSE)

#otter_520_noNA$cauchy <- dcauchy(otter_520_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_520_noNA$cauchy <- dcauchy(otter_520_noNA$dist, location = fit_cauchy_520$estimate[1], scale = fit_cauchy_520$estimate[2], log = FALSE)


#save loglik
otter_520_noNA$log_lik_ray <- fit_rayleigh_520$loglik
otter_520_noNA$log_lik_gamma <- fit_gamma_520$loglik
otter_520_noNA$log_lik_weibull <- fit_weibull_520$loglik
otter_520_noNA$log_lik_cauchy <- fit_cauchy_520$loglik

#plot over histogram
ggplot(otter_520_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 526
#create inidividual otter df for model fitting

#individual otter df
otter_526 <- resights_dist_recap %>% subset(seaotter == "526")
otter_526 <- as.data.frame(otter_526)
otter_526_noNA <- na.omit(otter_526)


#rayleigh
#fit model
fit_rayleigh_526 <- fitdistr(otter_526_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_526)

#gamma
#fit model
fit_gamma_526 <- fitdistr(otter_526_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_526)

#weibull
#fit model
fit_weibull_526 <- fitdistr(otter_526_noNA$dist,"weibull")
str(fit_weibull_526)

#cauchy
#fit model
fit_cauchy_526 <- fitdistr(otter_526_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_526)

#estimates from models
otter_526_noNA$rayleigh <- drayleigh(otter_526_noNA$dist, scale = fit_rayleigh_526$estimate, log = FALSE)

otter_526_noNA$gamma <- dgamma(otter_526_noNA$dist, shape  = fit_gamma_526$estimate[1], rate = fit_gamma_526$estimate[2], log = FALSE)

otter_526_noNA$weibull <- dweibull(otter_526_noNA$dist, shape = fit_weibull_526$estimate[1], scale = fit_weibull_526$estimate[2], log = FALSE)

#otter_526_noNA$cauchy <- dcauchy(otter_526_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_526_noNA$cauchy <- dcauchy(otter_526_noNA$dist, location = fit_cauchy_526$estimate[1], scale = fit_cauchy_526$estimate[2], log = FALSE)

#save loglik
otter_526_noNA$log_lik_ray <- fit_rayleigh_526$loglik
otter_526_noNA$log_lik_gamma <- fit_gamma_526$loglik
otter_526_noNA$log_lik_weibull <- fit_weibull_526$loglik
otter_526_noNA$log_lik_cauchy <- fit_cauchy_526$loglik


#plot over histogram
ggplot(otter_526_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 558 

#create inidividual otter df for model fitting

#individual otter df
otter_558 <- resights_dist_recap %>% subset(seaotter == "558")
otter_558 <- as.data.frame(otter_558)
otter_558_noNA <- na.omit(otter_558)


#rayleigh
#fit model
fit_rayleigh_558 <- fitdistr(otter_558_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_558)

#gamma
#fit model
fit_gamma_558 <- fitdistr(otter_558_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_558)

#weibull
#fit model
fit_weibull_558 <- fitdistr(otter_558_noNA$dist,"weibull")
str(fit_weibull_558)

#cauchy
#fit model
fit_cauchy_558 <- fitdistr(otter_558_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_558)

#estimates from models
otter_558_noNA$rayleigh <- drayleigh(otter_558_noNA$dist, scale = fit_rayleigh_558$estimate, log = FALSE)

otter_558_noNA$gamma <- dgamma(otter_558_noNA$dist, shape  = fit_gamma_558$estimate[1], rate = fit_gamma_558$estimate[2], log = FALSE)

otter_558_noNA$weibull <- dweibull(otter_558_noNA$dist, shape = fit_weibull_558$estimate[1], scale = fit_weibull_558$estimate[2], log = FALSE)

#otter_558_noNA$cauchy <- dcauchy(otter_558_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_558_noNA$cauchy <- dcauchy(otter_558_noNA$dist, location = fit_cauchy_558$estimate[1], scale = fit_cauchy_558$estimate[2], log = FALSE)

#save loglik
otter_558_noNA$log_lik_ray <- fit_rayleigh_558$loglik
otter_558_noNA$log_lik_gamma <- fit_gamma_558$loglik
otter_558_noNA$log_lik_weibull <- fit_weibull_558$loglik
otter_558_noNA$log_lik_cauchy <- fit_cauchy_558$loglik


#plot over histogram
ggplot(otter_558_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 587 

#create inidividual otter df for model fitting

#individual otter df
otter_587 <- resights_dist_recap %>% subset(seaotter == "587")
otter_587 <- as.data.frame(otter_587)
otter_587_noNA <- na.omit(otter_587)


#rayleigh
#fit model
fit_rayleigh_587 <- fitdistr(otter_587_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_587)

#gamma
#fit model
fit_gamma_587 <- fitdistr(otter_587_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_587)

#weibull
#fit model
fit_weibull_587 <- fitdistr(otter_587_noNA$dist,"weibull")
str(fit_weibull_587)

#cauchy
#fit model
fit_cauchy_587 <- fitdistr(otter_587_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_587)

#estimates from models
otter_587_noNA$rayleigh <- drayleigh(otter_587_noNA$dist, scale = fit_rayleigh_587$estimate, log = FALSE)

otter_587_noNA$gamma <- dgamma(otter_587_noNA$dist, shape  = fit_gamma_587$estimate[1], rate = fit_gamma_587$estimate[2], log = FALSE)

otter_587_noNA$weibull <- dweibull(otter_587_noNA$dist, shape = fit_weibull_587$estimate[1], scale = fit_weibull_587$estimate[2], log = FALSE)

#otter_587_noNA$cauchy <- dcauchy(otter_587_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_587_noNA$cauchy <- dcauchy(otter_587_noNA$dist, location = fit_cauchy_587$estimate[1], scale = fit_cauchy_587$estimate[2], log = FALSE)

#save loglik
otter_587_noNA$log_lik_ray <- fit_rayleigh_587$loglik
otter_587_noNA$log_lik_gamma <- fit_gamma_587$loglik
otter_587_noNA$log_lik_weibull <- fit_weibull_587$loglik
otter_587_noNA$log_lik_cauchy <- fit_cauchy_587$loglik

#plot over histogram
ggplot(otter_587_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 595 
#create inidividual otter df for model fitting

#individual otter df
otter_595 <- resights_dist_recap %>% subset(seaotter == "595")
otter_595 <- as.data.frame(otter_595)
otter_595_noNA <- na.omit(otter_595)


#rayleigh
#fit model
fit_rayleigh_595 <- fitdistr(otter_595_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_595)

#gamma
#fit model
fit_gamma_595 <- fitdistr(otter_595_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_595)

#weibull
#fit model
fit_weibull_595 <- fitdistr(otter_595_noNA$dist,"weibull")
str(fit_weibull_595)

#cauchy
#fit model
fit_cauchy_595 <- fitdistr(otter_595_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_595)

#estimates from models
otter_595_noNA$rayleigh <- drayleigh(otter_595_noNA$dist, scale = fit_rayleigh_595$estimate, log = FALSE)

otter_595_noNA$gamma <- dgamma(otter_595_noNA$dist, shape  = fit_gamma_595$estimate[1], rate = fit_gamma_595$estimate[2], log = FALSE)

otter_595_noNA$weibull <- dweibull(otter_595_noNA$dist, shape = fit_weibull_595$estimate[1], scale = fit_weibull_595$estimate[2], log = FALSE)

#otter_595_noNA$cauchy <- dcauchy(otter_595_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_595_noNA$cauchy <- dcauchy(otter_595_noNA$dist, location = fit_cauchy_595$estimate[1], scale = fit_cauchy_595$estimate[2], log = FALSE)

#save loglik
otter_595_noNA$log_lik_ray <- fit_rayleigh_595$loglik
otter_595_noNA$log_lik_gamma <- fit_gamma_595$loglik
otter_595_noNA$log_lik_weibull <- fit_weibull_595$loglik
otter_595_noNA$log_lik_cauchy <- fit_cauchy_595$loglik

#plot over histogram
ggplot(otter_595_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 621 

#create inidividual otter df for model fitting

#individual otter df
otter_621 <- resights_dist_recap %>% subset(seaotter == "621")
otter_621 <- as.data.frame(otter_621)
otter_621_noNA <- na.omit(otter_621)


#rayleigh
#fit model
fit_rayleigh_621 <- fitdistr(otter_621_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_621)

#gamma
#fit model
fit_gamma_621 <- fitdistr(otter_621_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_621)

#weibull
#fit model
fit_weibull_621 <- fitdistr(otter_621_noNA$dist,"weibull")
str(fit_weibull_621)

#cauchy
#fit model
fit_cauchy_621 <- fitdistr(otter_621_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_621)

#estimates from models
otter_621_noNA$rayleigh <- drayleigh(otter_621_noNA$dist, scale = fit_rayleigh_621$estimate, log = FALSE)

otter_621_noNA$gamma <- dgamma(otter_621_noNA$dist, shape  = fit_gamma_621$estimate[1], rate = fit_gamma_621$estimate[2], log = FALSE)

otter_621_noNA$weibull <- dweibull(otter_621_noNA$dist, shape = fit_weibull_621$estimate[1], scale = fit_weibull_621$estimate[2], log = FALSE)

#otter_621_noNA$cauchy <- dcauchy(otter_621_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_621_noNA$cauchy <- dcauchy(otter_621_noNA$dist, location = fit_cauchy_621$estimate[1], scale = fit_cauchy_621$estimate[2], log = FALSE)

#save loglik
otter_621_noNA$log_lik_ray <- fit_rayleigh_621$loglik
otter_621_noNA$log_lik_gamma <- fit_gamma_621$loglik
otter_621_noNA$log_lik_weibull <- fit_weibull_621$loglik
otter_621_noNA$log_lik_cauchy <- fit_cauchy_621$loglik

#plot over histogram
ggplot(otter_621_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 623
#create inidividual otter df for model fitting

#individual otter df
otter_623 <- resights_dist_recap %>% subset(seaotter == "623")
otter_623 <- as.data.frame(otter_623)
otter_623_noNA <- na.omit(otter_623)


#rayleigh
#fit model
fit_rayleigh_623 <- fitdistr(otter_623_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_623)

#gamma
#fit model
fit_gamma_623 <- fitdistr(otter_623_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_623)

#weibull
#fit model
fit_weibull_623 <- fitdistr(otter_623_noNA$dist,"weibull")
str(fit_weibull_623)

#cauchy
#fit model
fit_cauchy_623 <- fitdistr(otter_623_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_623)

#estimates from models
otter_623_noNA$rayleigh <- drayleigh(otter_623_noNA$dist, scale = fit_rayleigh_623$estimate, log = FALSE)

otter_623_noNA$gamma <- dgamma(otter_623_noNA$dist, shape  = fit_gamma_623$estimate[1], rate = fit_gamma_623$estimate[2], log = FALSE)

otter_623_noNA$weibull <- dweibull(otter_623_noNA$dist, shape = fit_weibull_623$estimate[1], scale = fit_weibull_623$estimate[2], log = FALSE)

#otter_623_noNA$cauchy <- dcauchy(otter_623_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_623_noNA$cauchy <- dcauchy(otter_623_noNA$dist, location = fit_cauchy_623$estimate[1], scale = fit_cauchy_623$estimate[2], log = FALSE)

#save loglik
otter_623_noNA$log_lik_ray <- fit_rayleigh_623$loglik
otter_623_noNA$log_lik_gamma <- fit_gamma_623$loglik
otter_623_noNA$log_lik_weibull <- fit_weibull_623$loglik
otter_623_noNA$log_lik_cauchy <- fit_cauchy_623$loglik

#plot over histogram
ggplot(otter_623_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


# 653

#create inidividual otter df for model fitting

#individual otter df
otter_653 <- resights_dist_recap %>% subset(seaotter == "653")
otter_653 <- as.data.frame(otter_653)
otter_653_noNA <- na.omit(otter_653)


#rayleigh
#fit model
fit_rayleigh_653 <- fitdistr(otter_653_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_653)

#gamma
#fit model
fit_gamma_653 <- fitdistr(otter_653_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_653)

#weibull
#fit model
fit_weibull_653 <- fitdistr(otter_653_noNA$dist,"weibull")
str(fit_weibull_653)

#cauchy
#fit model
fit_cauchy_653 <- fitdistr(otter_653_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_653)

#estimates from models
otter_653_noNA$rayleigh <- drayleigh(otter_653_noNA$dist, scale = fit_rayleigh_653$estimate, log = FALSE)

otter_653_noNA$gamma <- dgamma(otter_653_noNA$dist, shape  = fit_gamma_653$estimate[1], rate = fit_gamma_653$estimate[2], log = FALSE)

otter_653_noNA$weibull <- dweibull(otter_653_noNA$dist, shape = fit_weibull_653$estimate[1], scale = fit_weibull_653$estimate[2], log = FALSE)

#otter_653_noNA$cauchy <- dcauchy(otter_653_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_653_noNA$cauchy <- dcauchy(otter_653_noNA$dist, location = fit_cauchy_653$estimate[1], scale = fit_cauchy_653$estimate[2], log = FALSE)

#save loglik
otter_653_noNA$log_lik_ray <- fit_rayleigh_653$loglik
otter_653_noNA$log_lik_gamma <- fit_gamma_653$loglik
otter_653_noNA$log_lik_weibull <- fit_weibull_653$loglik
otter_653_noNA$log_lik_cauchy <- fit_cauchy_653$loglik

#plot over histogram
ggplot(otter_653_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 657 

#create inidividual otter df for model fitting

#individual otter df
otter_657 <- resights_dist_recap %>% subset(seaotter == "657")
otter_657 <- as.data.frame(otter_657)
otter_657_noNA <- na.omit(otter_657)


#rayleigh
#fit model
fit_rayleigh_657 <- fitdistr(otter_657_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_657)

#gamma
#fit model
fit_gamma_657 <- fitdistr(otter_657_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_657)

#weibull
#fit model
fit_weibull_657 <- fitdistr(otter_657_noNA$dist,"weibull")
str(fit_weibull_657)

#cauchy
#fit model
fit_cauchy_657 <- fitdistr(otter_657_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_657)

#estimates from models
otter_657_noNA$rayleigh <- drayleigh(otter_657_noNA$dist, scale = fit_rayleigh_657$estimate, log = FALSE)

otter_657_noNA$gamma <- dgamma(otter_657_noNA$dist, shape  = fit_gamma_657$estimate[1], rate = fit_gamma_657$estimate[2], log = FALSE)

otter_657_noNA$weibull <- dweibull(otter_657_noNA$dist, shape = fit_weibull_657$estimate[1], scale = fit_weibull_657$estimate[2], log = FALSE)

#otter_657_noNA$cauchy <- dcauchy(otter_657_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_657_noNA$cauchy <- dcauchy(otter_657_noNA$dist, location = fit_cauchy_657$estimate[1], scale = fit_cauchy_657$estimate[2], log = FALSE)

#save loglik
otter_657_noNA$log_lik_ray <- fit_rayleigh_657$loglik
otter_657_noNA$log_lik_gamma <- fit_gamma_657$loglik
otter_657_noNA$log_lik_weibull <- fit_weibull_657$loglik
otter_657_noNA$log_lik_cauchy <- fit_cauchy_657$loglik

#plot over histogram
ggplot(otter_657_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 671 

#create inidividual otter df for model fitting

#individual otter df
otter_671 <- resights_dist_recap %>% subset(seaotter == "671")
otter_671 <- as.data.frame(otter_671)
otter_671_noNA <- na.omit(otter_671)


#rayleigh
#fit model
fit_rayleigh_671 <- fitdistr(otter_671_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_671)

#gamma
#fit model
fit_gamma_671 <- fitdistr(otter_671_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_671)

#weibull
#fit model
fit_weibull_671 <- fitdistr(otter_671_noNA$dist,"weibull")
str(fit_weibull_671)

#cauchy
#fit model
fit_cauchy_671 <- fitdistr(otter_671_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_671)

#estimates from models
otter_671_noNA$rayleigh <- drayleigh(otter_671_noNA$dist, scale = fit_rayleigh_671$estimate, log = FALSE)

otter_671_noNA$gamma <- dgamma(otter_671_noNA$dist, shape  = fit_gamma_671$estimate[1], rate = fit_gamma_671$estimate[2], log = FALSE)

otter_671_noNA$weibull <- dweibull(otter_671_noNA$dist, shape = fit_weibull_671$estimate[1], scale = fit_weibull_671$estimate[2], log = FALSE)

#otter_671_noNA$cauchy <- dcauchy(otter_671_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_671_noNA$cauchy <- dcauchy(otter_671_noNA$dist, location = fit_cauchy_671$estimate[1], scale = fit_cauchy_671$estimate[2], log = FALSE)

#save loglik
otter_671_noNA$log_lik_ray <- fit_rayleigh_671$loglik
otter_671_noNA$log_lik_gamma <- fit_gamma_671$loglik
otter_671_noNA$log_lik_weibull <- fit_weibull_671$loglik
otter_671_noNA$log_lik_cauchy <- fit_cauchy_671$loglik

#plot over histogram
ggplot(otter_671_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 673

#create inidividual otter df for model fitting

#individual otter df
otter_673 <- resights_dist_recap %>% subset(seaotter == "673")
otter_673 <- as.data.frame(otter_673)
otter_673_noNA <- na.omit(otter_673)


#rayleigh
#fit model
fit_rayleigh_673 <- fitdistr(otter_673_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_673)

#gamma
#fit model
fit_gamma_673 <- fitdistr(otter_673_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_673)

#weibull
#fit model
fit_weibull_673 <- fitdistr(otter_673_noNA$dist,"weibull")
str(fit_weibull_673)

#cauchy
#fit model
fit_cauchy_673 <- fitdistr(otter_673_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_673)

#estimates from models
otter_673_noNA$rayleigh <- drayleigh(otter_673_noNA$dist, scale = fit_rayleigh_673$estimate, log = FALSE)

otter_673_noNA$gamma <- dgamma(otter_673_noNA$dist, shape  = fit_gamma_673$estimate[1], rate = fit_gamma_673$estimate[2], log = FALSE)

otter_673_noNA$weibull <- dweibull(otter_673_noNA$dist, shape = fit_weibull_673$estimate[1], scale = fit_weibull_673$estimate[2], log = FALSE)

#otter_673_noNA$cauchy <- dcauchy(otter_673_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_673_noNA$cauchy <- dcauchy(otter_673_noNA$dist, location = fit_cauchy_673$estimate[1], scale = fit_cauchy_673$estimate[2], log = FALSE)

#save loglik
otter_673_noNA$log_lik_ray <- fit_rayleigh_673$loglik
otter_673_noNA$log_lik_gamma <- fit_gamma_673$loglik
otter_673_noNA$log_lik_weibull <- fit_weibull_673$loglik
otter_673_noNA$log_lik_cauchy <- fit_cauchy_673$loglik

#plot over histogram
ggplot(otter_673_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 685 

#create inidividual otter df for model fitting

#individual otter df
otter_685 <- resights_dist_recap %>% subset(seaotter == "685")
otter_685 <- as.data.frame(otter_685)
otter_685_noNA <- na.omit(otter_685)


#rayleigh
#fit model
fit_rayleigh_685 <- fitdistr(otter_685_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_685)

#gamma
#fit model
fit_gamma_685 <- fitdistr(otter_685_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_685)

#weibull
#fit model
fit_weibull_685 <- fitdistr(otter_685_noNA$dist,"weibull")
str(fit_weibull_685)

#cauchy
#fit model
fit_cauchy_685 <- fitdistr(otter_685_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_685)

#estimates from models
otter_685_noNA$rayleigh <- drayleigh(otter_685_noNA$dist, scale = fit_rayleigh_685$estimate, log = FALSE)

otter_685_noNA$gamma <- dgamma(otter_685_noNA$dist, shape  = fit_gamma_685$estimate[1], rate = fit_gamma_685$estimate[2], log = FALSE)

otter_685_noNA$weibull <- dweibull(otter_685_noNA$dist, shape = fit_weibull_685$estimate[1], scale = fit_weibull_685$estimate[2], log = FALSE)

#otter_685_noNA$cauchy <- dcauchy(otter_685_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_685_noNA$cauchy <- dcauchy(otter_685_noNA$dist, location = fit_cauchy_685$estimate[1], scale = fit_cauchy_685$estimate[2], log = FALSE)

#save loglik
otter_685_noNA$log_lik_ray <- fit_rayleigh_685$loglik
otter_685_noNA$log_lik_gamma <- fit_gamma_685$loglik
otter_685_noNA$log_lik_weibull <- fit_weibull_685$loglik
otter_685_noNA$log_lik_cauchy <- fit_cauchy_685$loglik

#plot over histogram
ggplot(otter_685_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 687 

#create inidividual otter df for model fitting

#individual otter df
otter_687 <- resights_dist_recap %>% subset(seaotter == "687")
otter_687 <- as.data.frame(otter_687)
otter_687_noNA <- na.omit(otter_687)


#rayleigh
#fit model
fit_rayleigh_687 <- fitdistr(otter_687_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_687)

#gamma
#fit model
fit_gamma_687 <- fitdistr(otter_687_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_687)

#weibull
#fit model
fit_weibull_687 <- fitdistr(otter_687_noNA$dist,"weibull")
str(fit_weibull_687)

#cauchy
#fit model
fit_cauchy_687 <- fitdistr(otter_687_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_687)

#estimates from models
otter_687_noNA$rayleigh <- drayleigh(otter_687_noNA$dist, scale = fit_rayleigh_687$estimate, log = FALSE)

otter_687_noNA$gamma <- dgamma(otter_687_noNA$dist, shape  = fit_gamma_687$estimate[1], rate = fit_gamma_687$estimate[2], log = FALSE)

otter_687_noNA$weibull <- dweibull(otter_687_noNA$dist, shape = fit_weibull_687$estimate[1], scale = fit_weibull_687$estimate[2], log = FALSE)

#otter_687_noNA$cauchy <- dcauchy(otter_687_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_687_noNA$cauchy <- dcauchy(otter_687_noNA$dist, location = fit_cauchy_687$estimate[1], scale = fit_cauchy_687$estimate[2], log = FALSE)

#save loglik
otter_687_noNA$log_lik_ray <- fit_rayleigh_687$loglik
otter_687_noNA$log_lik_gamma <- fit_gamma_687$loglik
otter_687_noNA$log_lik_weibull <- fit_weibull_687$loglik
otter_687_noNA$log_lik_cauchy <- fit_cauchy_687$loglik

#plot over histogram
ggplot(otter_687_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 716 

#create inidividual otter df for model fitting

#individual otter df
otter_716 <- resights_dist_recap %>% subset(seaotter == "716")
otter_716 <- as.data.frame(otter_716)
otter_716_noNA <- na.omit(otter_716)


#rayleigh
#fit model
fit_rayleigh_716 <- fitdistr(otter_716_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_716)

#gamma
#fit model
fit_gamma_716 <- fitdistr(otter_716_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_716)

#weibull
#fit model
fit_weibull_716 <- fitdistr(otter_716_noNA$dist,"weibull")
str(fit_weibull_716)

#cauchy
#fit model
fit_cauchy_716 <- fitdistr(otter_716_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_716)

#estimates from models
otter_716_noNA$rayleigh <- drayleigh(otter_716_noNA$dist, scale = fit_rayleigh_716$estimate, log = FALSE)

otter_716_noNA$gamma <- dgamma(otter_716_noNA$dist, shape  = fit_gamma_716$estimate[1], rate = fit_gamma_716$estimate[2], log = FALSE)

otter_716_noNA$weibull <- dweibull(otter_716_noNA$dist, shape = fit_weibull_716$estimate[1], scale = fit_weibull_716$estimate[2], log = FALSE)

#otter_716_noNA$cauchy <- dcauchy(otter_716_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_716_noNA$cauchy <- dcauchy(otter_716_noNA$dist, location = fit_cauchy_716$estimate[1], scale = fit_cauchy_716$estimate[2], log = FALSE)

#save loglik
otter_716_noNA$log_lik_ray <- fit_rayleigh_716$loglik
otter_716_noNA$log_lik_gamma <- fit_gamma_716$loglik
otter_716_noNA$log_lik_weibull <- fit_weibull_716$loglik
otter_716_noNA$log_lik_cauchy <- fit_cauchy_716$loglik

#plot over histogram
ggplot(otter_716_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 723 

#create inidividual otter df for model fitting

#individual otter df
otter_723 <- resights_dist_recap %>% subset(seaotter == "723")
otter_723 <- as.data.frame(otter_723)
otter_723_noNA <- na.omit(otter_723)


#rayleigh
#fit model
fit_rayleigh_723 <- fitdistr(otter_723_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_723)

#gamma
#fit model
fit_gamma_723 <- fitdistr(otter_723_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_723)

#weibull
#fit model
fit_weibull_723 <- fitdistr(otter_723_noNA$dist,"weibull")
str(fit_weibull_723)

#cauchy
#fit model
fit_cauchy_723 <- fitdistr(otter_723_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_723)

#estimates from models
otter_723_noNA$rayleigh <- drayleigh(otter_723_noNA$dist, scale = fit_rayleigh_723$estimate, log = FALSE)

otter_723_noNA$gamma <- dgamma(otter_723_noNA$dist, shape  = fit_gamma_723$estimate[1], rate = fit_gamma_723$estimate[2], log = FALSE)

otter_723_noNA$weibull <- dweibull(otter_723_noNA$dist, shape = fit_weibull_723$estimate[1], scale = fit_weibull_723$estimate[2], log = FALSE)

#otter_723_noNA$cauchy <- dcauchy(otter_723_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_723_noNA$cauchy <- dcauchy(otter_723_noNA$dist, location = fit_cauchy_723$estimate[1], scale = fit_cauchy_723$estimate[2], log = FALSE)

#save loglik
otter_723_noNA$log_lik_ray <- fit_rayleigh_723$loglik
otter_723_noNA$log_lik_gamma <- fit_gamma_723$loglik
otter_723_noNA$log_lik_weibull <- fit_weibull_723$loglik
otter_723_noNA$log_lik_cauchy <- fit_cauchy_723$loglik


#plot over histogram
ggplot(otter_723_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 774 

#create inidividual otter df for model fitting

#individual otter df
otter_774 <- resights_dist_recap %>% subset(seaotter == "774")
otter_774 <- as.data.frame(otter_774)
otter_774_noNA <- na.omit(otter_774)


#rayleigh
#fit model
fit_rayleigh_774 <- fitdistr(otter_774_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_774)

#gamma
#fit model
fit_gamma_774 <- fitdistr(otter_774_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_774)

#weibull
#fit model
fit_weibull_774 <- fitdistr(otter_774_noNA$dist,"weibull")
str(fit_weibull_774)

#cauchy
#fit model
fit_cauchy_774 <- fitdistr(otter_774_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_774)

#estimates from models
otter_774_noNA$rayleigh <- drayleigh(otter_774_noNA$dist, scale = fit_rayleigh_774$estimate, log = FALSE)

otter_774_noNA$gamma <- dgamma(otter_774_noNA$dist, shape  = fit_gamma_774$estimate[1], rate = fit_gamma_774$estimate[2], log = FALSE)

otter_774_noNA$weibull <- dweibull(otter_774_noNA$dist, shape = fit_weibull_774$estimate[1], scale = fit_weibull_774$estimate[2], log = FALSE)

#otter_774_noNA$cauchy <- dcauchy(otter_774_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_774_noNA$cauchy <- dcauchy(otter_774_noNA$dist, location = fit_cauchy_774$estimate[1], scale = fit_cauchy_774$estimate[2], log = FALSE)

#save loglik
otter_774_noNA$log_lik_ray <- fit_rayleigh_774$loglik
otter_774_noNA$log_lik_gamma <- fit_gamma_774$loglik
otter_774_noNA$log_lik_weibull <- fit_weibull_774$loglik
otter_774_noNA$log_lik_cauchy <- fit_cauchy_774$loglik

#plot over histogram
ggplot(otter_774_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 808 

#create inidividual otter df for model fitting

#individual otter df
otter_808 <- resights_dist_recap %>% subset(seaotter == "808")
otter_808 <- as.data.frame(otter_808)
otter_808_noNA <- na.omit(otter_808)


#rayleigh
#fit model
fit_rayleigh_808 <- fitdistr(otter_808_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_808)

#gamma
#fit model
fit_gamma_808 <- fitdistr(otter_808_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_808)

#weibull
#fit model
fit_weibull_808 <- fitdistr(otter_808_noNA$dist,"weibull")
str(fit_weibull_808)

#cauchy
#fit model
fit_cauchy_808 <- fitdistr(otter_808_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_808)

#estimates from models
otter_808_noNA$rayleigh <- drayleigh(otter_808_noNA$dist, scale = fit_rayleigh_808$estimate, log = FALSE)

otter_808_noNA$gamma <- dgamma(otter_808_noNA$dist, shape  = fit_gamma_808$estimate[1], rate = fit_gamma_808$estimate[2], log = FALSE)

otter_808_noNA$weibull <- dweibull(otter_808_noNA$dist, shape = fit_weibull_808$estimate[1], scale = fit_weibull_808$estimate[2], log = FALSE)

#otter_808_noNA$cauchy <- dcauchy(otter_808_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_808_noNA$cauchy <- dcauchy(otter_808_noNA$dist, location = fit_cauchy_808$estimate[1], scale = fit_cauchy_808$estimate[2], log = FALSE)

#save loglik
otter_808_noNA$log_lik_ray <- fit_rayleigh_808$loglik
otter_808_noNA$log_lik_gamma <- fit_gamma_808$loglik
otter_808_noNA$log_lik_weibull <- fit_weibull_808$loglik
otter_808_noNA$log_lik_cauchy <- fit_cauchy_808$loglik

#plot over histogram
ggplot(otter_808_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 

# 809
#create inidividual otter df for model fitting

#individual otter df
otter_809 <- resights_dist_recap %>% subset(seaotter == "809")
otter_809 <- as.data.frame(otter_809)
otter_809_noNA <- na.omit(otter_809)


#rayleigh
#fit model
fit_rayleigh_809 <- fitdistr(otter_809_noNA$dist, drayleigh,start = list(scale = 1))
str(fit_rayleigh_809)

#gamma
#fit model
fit_gamma_809 <- fitdistr(otter_809_noNA$dist,dgamma, start = list(shape = 14, rate = .001), lower = 0.01)
str(fit_gamma_809)

#weibull
#fit model
fit_weibull_809 <- fitdistr(otter_809_noNA$dist,"weibull")
str(fit_weibull_809)

#cauchy
#fit model
fit_cauchy_809 <- fitdistr(otter_809_noNA$dist, dcauchy, start = list(location = 4000, scale = 2000), lower = 0.01)
str(fit_cauchy_809)

#estimates from models
otter_809_noNA$rayleigh <- drayleigh(otter_809_noNA$dist, scale = fit_rayleigh_809$estimate, log = FALSE)

otter_809_noNA$gamma <- dgamma(otter_809_noNA$dist, shape  = fit_gamma_809$estimate[1], rate = fit_gamma_809$estimate[2], log = FALSE)

otter_809_noNA$weibull <- dweibull(otter_809_noNA$dist, shape = fit_weibull_809$estimate[1], scale = fit_weibull_809$estimate[2], log = FALSE)

#otter_809_noNA$cauchy <- dcauchy(otter_809_noNA$dist, location = 2328, scale = 1804, log = FALSE)
otter_809_noNA$cauchy <- dcauchy(otter_809_noNA$dist, location = fit_cauchy_809$estimate[1], scale = fit_cauchy_809$estimate[2], log = FALSE)


#save loglik
otter_809_noNA$log_lik_ray <- fit_rayleigh_809$loglik
otter_809_noNA$log_lik_gamma <- fit_gamma_809$loglik
otter_809_noNA$log_lik_weibull <- fit_weibull_809$loglik
otter_809_noNA$log_lik_cauchy <- fit_cauchy_809$loglik

#plot over histogram
ggplot(otter_809_noNA)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  themeo 


#rbind together
# all fit but 379, add in once successfully fit
otter_modelfit_dist <- rbind(otter_209_noNA ,  otter_238_noNA ,  otter_225_noNA ,  otter_228_noNA ,  otter_238_noNA ,  otter_249_noNA ,  otter_252_noNA , 
                             otter_269_noNA ,  otter_286_noNA ,  otter_315_noNA ,  otter_327_noNA ,  otter_339_noNA ,  otter_344_noNA ,  otter_353_noNA , 
                             otter_386_noNA ,  otter_433_noNA ,  otter_451_noNA ,  otter_457_noNA ,  otter_466_noNA ,  otter_473_noNA , 
                             otter_475_noNA ,  otter_501_noNA ,  otter_518_noNA ,  otter_520_noNA ,  otter_526_noNA ,  otter_558_noNA ,  otter_587_noNA , 
                             otter_595_noNA ,  otter_621_noNA ,  otter_623_noNA ,  otter_653_noNA ,  otter_657_noNA ,  otter_671_noNA ,  otter_673_noNA , 
                             otter_685_noNA ,  otter_687_noNA ,  otter_716_noNA ,  otter_723_noNA ,  otter_774_noNA ,  otter_808_noNA ,  otter_809_noNA)
write.csv(otter_modelfit_dist, "data_output/otter_model_fit_dist.csv")

#plot
ggplot(otter_modelfit_dist)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 2000)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  facet_wrap(~seaotter, scales = "free_y", ncol = 6)#+
#  themeo 

#calculate log-likelihood
otter_loglik <- otter_modelfit_dist %>%  
  group_by(seaotter) %>% 
  summarise(first(log_lik_ray), 
            first(log_lik_gamma),
            first (log_lik_weibull ),
            first(log_lik_cauchy)) 


head(otter_loglik)
otter_loglik<- rename(otter_loglik, LL_rayleigh = `first(log_lik_ray)`, LL_gamma = `first(log_lik_gamma)`, LL_weibull = `first(log_lik_weibull)`, LL_cauchy = `first(log_lik_cauchy)` )


#calculate AIC

otter_loglik$rayAIC <- ((-2*otter_loglik$LL_rayleigh)+ (2*1))

otter_loglik$gammaAIC <- ((-2*otter_loglik$LL_gamma)+ (2*2))

otter_loglik$weibullAIC <- ((-2*otter_loglik$LL_weibull)+(2*2))

otter_loglik$cauchyAIC <- ((-2*otter_loglik$LL_cauchy)+(2*2))


AIC_table <- otter_loglik %>%  dplyr::select(seaotter, rayAIC, gammaAIC, weibullAIC, cauchyAIC)


AIC_rank <- data.frame(AIC_table, t(apply(AIC_table[,c(2:5)], 1, rank, ties.method='min')))  

AIC_diff_dist <- AIC_rank %>%  mutate(AIC_difference = weibullAIC - (pmin(rayAIC,gammaAIC, cauchyAIC))) 
max(AIC_diff$AIC_difference) #4.461328

write.csv(AIC_diff_dist, "data_output/AIC_diff_dist.csv")

#plot scaled 0-1
library(scales)
ggplot(otter_modelfit_dist)+
  geom_histogram(aes(x=dist, y = ..density..), binwidth = 1500)+
  geom_line(aes(x = dist, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = dist, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = dist, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = dist, y = cauchy),stat = "identity", color = "green")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  facet_wrap(~seaotter, scales = "free_y")+
  coord_cartesian(xlim = c(0,50000), ylim = c(0,0.0007))+
  #xlim(c(0,10))+
  themeo 




