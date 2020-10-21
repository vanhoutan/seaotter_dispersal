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


unique(resights_dist_recap_noNA$seaotter)
# 209 238 225 228 238 249 252 269 286 315 327 339 344 353 379 386 433 451 457 
# 466 473 475 501 518 520 526 558 587 595 621 623 653 657 671 673
# 685 687 716 723 774 808 809

# 209
#create inidividual otter df for model fitting

#individual otter df
otter_209_rate <- resights_dist_recap %>% subset(seaotter == "209")
otter_209_rate <- as.data.frame(otter_209_rate)
otter_209_noNA_rate <- na.omit(otter_209_rate)


#rayleigh
#fit model
fit_rayleigh_209_rate <- fitdistr(otter_209_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_209_rate)

#gamma
#fit model
fit_gamma_209_rate <- fitdistr(otter_209_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_209_rate)

#weibull
#fit model
fit_weibull_209_rate <- fitdistr(otter_209_noNA_rate$km_hr,"weibull")
str(fit_weibull_209_rate)

#cauchy
#fit model
fit_cauchy_209_rate <- fitdistr(otter_209_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_209_rate)

#estimates from models
otter_209_noNA_rate$rayleigh <- drayleigh(otter_209_noNA_rate$km_hr, scale = fit_rayleigh_209_rate$estimate, log = FALSE)

otter_209_noNA_rate$gamma <- dgamma(otter_209_noNA_rate$km_hr, shape  = fit_gamma_209_rate$estimate[1], rate = fit_gamma_209_rate$estimate[2], log = FALSE)

otter_209_noNA_rate$weibull <- dweibull(otter_209_noNA_rate$km_hr, shape = fit_weibull_209_rate$estimate[1], scale = fit_weibull_209_rate$estimate[2], log = FALSE)

otter_209_noNA_rate$cauchy <- dcauchy(otter_209_noNA_rate$km_hr, location = fit_cauchy_209_rate$estimate[1], scale = fit_cauchy_209_rate$estimate[2], log = FALSE)
#otter_209_noNA$cauchy2 <- dcauchy(otter_209_noNA$dist, location = fit_cauchy_209$estimate[1], scale = fit_cauchy$estimate_209[2], log = FALSE)


#save loglik
otter_209_noNA_rate$log_lik_ray <- fit_rayleigh_209_rate$loglik
otter_209_noNA_rate$log_lik_gamma <- fit_gamma_209_rate$loglik
otter_209_noNA_rate$log_lik_weibull <- fit_weibull_209_rate$loglik
otter_209_noNA_rate$log_lik_cauchy <- fit_cauchy_209_rate$loglik


#plot over histogram
ggplot(otter_209_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  



# 238 
#create inidividual otter df for model fitting

#individual otter df
otter_238_rate <- resights_dist_recap %>% subset(seaotter == "238")
otter_238_rate <- as.data.frame(otter_238_rate)
otter_238_noNA_rate <- na.omit(otter_238_rate)


#rayleigh
#fit model
fit_rayleigh_238_rate <- fitdistr(otter_238_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_238_rate)

#gamma
#fit model
fit_gamma_238_rate <- fitdistr(otter_238_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_238_rate)

#weibull
#fit model
fit_weibull_238_rate <- fitdistr(otter_238_noNA_rate$km_hr,"weibull")
str(fit_weibull_238_rate)

#cauchy
#fit model
fit_cauchy_238_rate <- fitdistr(otter_238_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_238_rate)

#estimates from models
otter_238_noNA_rate$rayleigh <- drayleigh(otter_238_noNA_rate$km_hr, scale = fit_rayleigh_238_rate$estimate, log = FALSE)

otter_238_noNA_rate$gamma <- dgamma(otter_238_noNA_rate$km_hr, shape  = fit_gamma_238_rate$estimate[1], rate = fit_gamma_238_rate$estimate[2], log = FALSE)

otter_238_noNA_rate$weibull <- dweibull(otter_238_noNA_rate$km_hr, shape = fit_weibull_238_rate$estimate[1], scale = fit_weibull_238_rate$estimate[2], log = FALSE)

otter_238_noNA_rate$cauchy <- dcauchy(otter_238_noNA_rate$km_hr, location = fit_cauchy_238_rate$estimate[1], scale = fit_cauchy_238_rate$estimate[2], log = FALSE)
#otter_238_noNA$cauchy2 <- dcauchy(otter_238_noNA$dist, location = fit_cauchy_238$estimate[1], scale = fit_cauchy$estimate_238[2], log = FALSE)


#save loglik
otter_238_noNA_rate$log_lik_ray <- fit_rayleigh_238_rate$loglik
otter_238_noNA_rate$log_lik_gamma <- fit_gamma_238_rate$loglik
otter_238_noNA_rate$log_lik_weibull <- fit_weibull_238_rate$loglik
otter_238_noNA_rate$log_lik_cauchy <- fit_cauchy_238_rate$loglik


#plot over histogram
ggplot(otter_238_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 225 

#create inidividual otter df for model fitting

#individual otter df
otter_225_rate <- resights_dist_recap %>% subset(seaotter == "225")
otter_225_rate <- as.data.frame(otter_225_rate)
otter_225_noNA_rate <- na.omit(otter_225_rate)


#rayleigh
#fit model
fit_rayleigh_225_rate <- fitdistr(otter_225_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_225_rate)

#gamma
#fit model
fit_gamma_225_rate <- fitdistr(otter_225_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_225_rate)

#weibull
#fit model
fit_weibull_225_rate <- fitdistr(otter_225_noNA_rate$km_hr,"weibull")
str(fit_weibull_225_rate)

#cauchy
#fit model
fit_cauchy_225_rate <- fitdistr(otter_225_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_225_rate)

#estimates from models
otter_225_noNA_rate$rayleigh <- drayleigh(otter_225_noNA_rate$km_hr, scale = fit_rayleigh_225_rate$estimate, log = FALSE)

otter_225_noNA_rate$gamma <- dgamma(otter_225_noNA_rate$km_hr, shape  = fit_gamma_225_rate$estimate[1], rate = fit_gamma_225_rate$estimate[2], log = FALSE)

otter_225_noNA_rate$weibull <- dweibull(otter_225_noNA_rate$km_hr, shape = fit_weibull_225_rate$estimate[1], scale = fit_weibull_225_rate$estimate[2], log = FALSE)

otter_225_noNA_rate$cauchy <- dcauchy(otter_225_noNA_rate$km_hr, location = fit_cauchy_225_rate$estimate[1], scale = fit_cauchy_225_rate$estimate[2], log = FALSE)
#otter_225_noNA$cauchy2 <- dcauchy(otter_225_noNA$dist, location = fit_cauchy_225$estimate[1], scale = fit_cauchy$estimate_225[2], log = FALSE)


#save loglik
otter_225_noNA_rate$log_lik_ray <- fit_rayleigh_225_rate$loglik
otter_225_noNA_rate$log_lik_gamma <- fit_gamma_225_rate$loglik
otter_225_noNA_rate$log_lik_weibull <- fit_weibull_225_rate$loglik
otter_225_noNA_rate$log_lik_cauchy <- fit_cauchy_225_rate$loglik


#plot over histogram
ggplot(otter_225_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 228 
#create inidividual otter df for model fitting

#individual otter df
otter_228_rate <- resights_dist_recap %>% subset(seaotter == "228")
otter_228_rate <- as.data.frame(otter_228_rate)
otter_228_noNA_rate <- na.omit(otter_228_rate)


#rayleigh
#fit model
fit_rayleigh_228_rate <- fitdistr(otter_228_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_228_rate)

#gamma
#fit model
fit_gamma_228_rate <- fitdistr(otter_228_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_228_rate)

#weibull
#fit model
fit_weibull_228_rate <- fitdistr(otter_228_noNA_rate$km_hr,"weibull")
str(fit_weibull_228_rate)

#cauchy
#fit model
fit_cauchy_228_rate <- fitdistr(otter_228_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_228_rate)

#estimates from models
otter_228_noNA_rate$rayleigh <- drayleigh(otter_228_noNA_rate$km_hr, scale = fit_rayleigh_228_rate$estimate, log = FALSE)

otter_228_noNA_rate$gamma <- dgamma(otter_228_noNA_rate$km_hr, shape  = fit_gamma_228_rate$estimate[1], rate = fit_gamma_228_rate$estimate[2], log = FALSE)

otter_228_noNA_rate$weibull <- dweibull(otter_228_noNA_rate$km_hr, shape = fit_weibull_228_rate$estimate[1], scale = fit_weibull_228_rate$estimate[2], log = FALSE)

otter_228_noNA_rate$cauchy <- dcauchy(otter_228_noNA_rate$km_hr, location = fit_cauchy_228_rate$estimate[1], scale = fit_cauchy_228_rate$estimate[2], log = FALSE)
#otter_228_noNA$cauchy2 <- dcauchy(otter_228_noNA$dist, location = fit_cauchy_228$estimate[1], scale = fit_cauchy$estimate_228[2], log = FALSE)


#save loglik
otter_228_noNA_rate$log_lik_ray <- fit_rayleigh_228_rate$loglik
otter_228_noNA_rate$log_lik_gamma <- fit_gamma_228_rate$loglik
otter_228_noNA_rate$log_lik_weibull <- fit_weibull_228_rate$loglik
otter_228_noNA_rate$log_lik_cauchy <- fit_cauchy_228_rate$loglik


#plot over histogram
ggplot(otter_228_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 238
#create inidividual otter df for model fitting

#individual otter df
otter_238_rate <- resights_dist_recap %>% subset(seaotter == "238")
otter_238_rate <- as.data.frame(otter_238_rate)
otter_238_noNA_rate <- na.omit(otter_238_rate)


#rayleigh
#fit model
fit_rayleigh_238_rate <- fitdistr(otter_238_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_238_rate)

#gamma
#fit model
fit_gamma_238_rate <- fitdistr(otter_238_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_238_rate)

#weibull
#fit model
fit_weibull_238_rate <- fitdistr(otter_238_noNA_rate$km_hr,"weibull")
str(fit_weibull_238_rate)

#cauchy
#fit model
fit_cauchy_238_rate <- fitdistr(otter_238_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_238_rate)

#estimates from models
otter_238_noNA_rate$rayleigh <- drayleigh(otter_238_noNA_rate$km_hr, scale = fit_rayleigh_238_rate$estimate, log = FALSE)

otter_238_noNA_rate$gamma <- dgamma(otter_238_noNA_rate$km_hr, shape  = fit_gamma_238_rate$estimate[1], rate = fit_gamma_238_rate$estimate[2], log = FALSE)

otter_238_noNA_rate$weibull <- dweibull(otter_238_noNA_rate$km_hr, shape = fit_weibull_238_rate$estimate[1], scale = fit_weibull_238_rate$estimate[2], log = FALSE)

otter_238_noNA_rate$cauchy <- dcauchy(otter_238_noNA_rate$km_hr, location = fit_cauchy_238_rate$estimate[1], scale = fit_cauchy_238_rate$estimate[2], log = FALSE)
#otter_238_noNA$cauchy2 <- dcauchy(otter_238_noNA$dist, location = fit_cauchy_238$estimate[1], scale = fit_cauchy$estimate_238[2], log = FALSE)


#save loglik
otter_238_noNA_rate$log_lik_ray <- fit_rayleigh_238_rate$loglik
otter_238_noNA_rate$log_lik_gamma <- fit_gamma_238_rate$loglik
otter_238_noNA_rate$log_lik_weibull <- fit_weibull_238_rate$loglik
otter_238_noNA_rate$log_lik_cauchy <- fit_cauchy_238_rate$loglik


#plot over histogram
ggplot(otter_238_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 249 
#create inidividual otter df for model fitting

#individual otter df
otter_249_rate <- resights_dist_recap %>% subset(seaotter == "249")
otter_249_rate <- as.data.frame(otter_249_rate)
otter_249_noNA_rate <- na.omit(otter_249_rate)


#rayleigh
#fit model
fit_rayleigh_249_rate <- fitdistr(otter_249_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_249_rate)

#gamma
#fit model
fit_gamma_249_rate <- fitdistr(otter_249_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_249_rate)

#weibull
#fit model
fit_weibull_249_rate <- fitdistr(otter_249_noNA_rate$km_hr,"weibull")
str(fit_weibull_249_rate)

#cauchy
#fit model
fit_cauchy_249_rate <- fitdistr(otter_249_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_249_rate)

#estimates from models
otter_249_noNA_rate$rayleigh <- drayleigh(otter_249_noNA_rate$km_hr, scale = fit_rayleigh_249_rate$estimate, log = FALSE)

otter_249_noNA_rate$gamma <- dgamma(otter_249_noNA_rate$km_hr, shape  = fit_gamma_249_rate$estimate[1], rate = fit_gamma_249_rate$estimate[2], log = FALSE)

otter_249_noNA_rate$weibull <- dweibull(otter_249_noNA_rate$km_hr, shape = fit_weibull_249_rate$estimate[1], scale = fit_weibull_249_rate$estimate[2], log = FALSE)

otter_249_noNA_rate$cauchy <- dcauchy(otter_249_noNA_rate$km_hr, location = fit_cauchy_249_rate$estimate[1], scale = fit_cauchy_249_rate$estimate[2], log = FALSE)
#otter_249_noNA$cauchy2 <- dcauchy(otter_249_noNA$dist, location = fit_cauchy_249$estimate[1], scale = fit_cauchy$estimate_249[2], log = FALSE)


#save loglik
otter_249_noNA_rate$log_lik_ray <- fit_rayleigh_249_rate$loglik
otter_249_noNA_rate$log_lik_gamma <- fit_gamma_249_rate$loglik
otter_249_noNA_rate$log_lik_weibull <- fit_weibull_249_rate$loglik
otter_249_noNA_rate$log_lik_cauchy <- fit_cauchy_249_rate$loglik


#plot over histogram
ggplot(otter_249_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 252 
#create inidividual otter df for model fitting

#individual otter df
otter_252_rate <- resights_dist_recap %>% subset(seaotter == "252")
otter_252_rate <- as.data.frame(otter_252_rate)
otter_252_noNA_rate <- na.omit(otter_252_rate)


#rayleigh
#fit model
fit_rayleigh_252_rate <- fitdistr(otter_252_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_252_rate)

#gamma
#fit model
fit_gamma_252_rate <- fitdistr(otter_252_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_252_rate)

#weibull
#fit model
fit_weibull_252_rate <- fitdistr(otter_252_noNA_rate$km_hr,"weibull")
str(fit_weibull_252_rate)

#cauchy
#fit model
fit_cauchy_252_rate <- fitdistr(otter_252_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_252_rate)

#estimates from models
otter_252_noNA_rate$rayleigh <- drayleigh(otter_252_noNA_rate$km_hr, scale = fit_rayleigh_252_rate$estimate, log = FALSE)

otter_252_noNA_rate$gamma <- dgamma(otter_252_noNA_rate$km_hr, shape  = fit_gamma_252_rate$estimate[1], rate = fit_gamma_252_rate$estimate[2], log = FALSE)

otter_252_noNA_rate$weibull <- dweibull(otter_252_noNA_rate$km_hr, shape = fit_weibull_252_rate$estimate[1], scale = fit_weibull_252_rate$estimate[2], log = FALSE)

otter_252_noNA_rate$cauchy <- dcauchy(otter_252_noNA_rate$km_hr, location = fit_cauchy_252_rate$estimate[1], scale = fit_cauchy_252_rate$estimate[2], log = FALSE)
#otter_252_noNA$cauchy2 <- dcauchy(otter_252_noNA$dist, location = fit_cauchy_252$estimate[1], scale = fit_cauchy$estimate_252[2], log = FALSE)


#save loglik
otter_252_noNA_rate$log_lik_ray <- fit_rayleigh_252_rate$loglik
otter_252_noNA_rate$log_lik_gamma <- fit_gamma_252_rate$loglik
otter_252_noNA_rate$log_lik_weibull <- fit_weibull_252_rate$loglik
otter_252_noNA_rate$log_lik_cauchy <- fit_cauchy_252_rate$loglik


#plot over histogram
ggplot(otter_252_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 269 
#create inidividual otter df for model fitting

#individual otter df
otter_269_rate <- resights_dist_recap %>% subset(seaotter == "269")
otter_269_rate <- as.data.frame(otter_269_rate)
otter_269_noNA_rate <- na.omit(otter_269_rate)


#rayleigh
#fit model
fit_rayleigh_269_rate <- fitdistr(otter_269_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_269_rate)

#gamma
#fit model
fit_gamma_269_rate <- fitdistr(otter_269_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_269_rate)

#weibull
#fit model
fit_weibull_269_rate <- fitdistr(otter_269_noNA_rate$km_hr,"weibull")
str(fit_weibull_269_rate)

#cauchy
#fit model
fit_cauchy_269_rate <- fitdistr(otter_269_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_269_rate)

#estimates from models
otter_269_noNA_rate$rayleigh <- drayleigh(otter_269_noNA_rate$km_hr, scale = fit_rayleigh_269_rate$estimate, log = FALSE)

otter_269_noNA_rate$gamma <- dgamma(otter_269_noNA_rate$km_hr, shape  = fit_gamma_269_rate$estimate[1], rate = fit_gamma_269_rate$estimate[2], log = FALSE)

otter_269_noNA_rate$weibull <- dweibull(otter_269_noNA_rate$km_hr, shape = fit_weibull_269_rate$estimate[1], scale = fit_weibull_269_rate$estimate[2], log = FALSE)

otter_269_noNA_rate$cauchy <- dcauchy(otter_269_noNA_rate$km_hr, location = fit_cauchy_269_rate$estimate[1], scale = fit_cauchy_269_rate$estimate[2], log = FALSE)
#otter_269_noNA$cauchy2 <- dcauchy(otter_269_noNA$dist, location = fit_cauchy_269$estimate[1], scale = fit_cauchy$estimate_269[2], log = FALSE)


#save loglik
otter_269_noNA_rate$log_lik_ray <- fit_rayleigh_269_rate$loglik
otter_269_noNA_rate$log_lik_gamma <- fit_gamma_269_rate$loglik
otter_269_noNA_rate$log_lik_weibull <- fit_weibull_269_rate$loglik
otter_269_noNA_rate$log_lik_cauchy <- fit_cauchy_269_rate$loglik


#plot over histogram
ggplot(otter_269_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 286
#create inidividual otter df for model fitting

#individual otter df
otter_286_rate <- resights_dist_recap %>% subset(seaotter == "286")
otter_286_rate <- as.data.frame(otter_286_rate)
otter_286_noNA_rate <- na.omit(otter_286_rate)


#rayleigh
#fit model
fit_rayleigh_286_rate <- fitdistr(otter_286_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_286_rate)

#gamma
#fit model
fit_gamma_286_rate <- fitdistr(otter_286_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_286_rate)

#weibull
#fit model
fit_weibull_286_rate <- fitdistr(otter_286_noNA_rate$km_hr,"weibull")
str(fit_weibull_286_rate)

#cauchy
#fit model
fit_cauchy_286_rate <- fitdistr(otter_286_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_286_rate)

#estimates from models
otter_286_noNA_rate$rayleigh <- drayleigh(otter_286_noNA_rate$km_hr, scale = fit_rayleigh_286_rate$estimate, log = FALSE)

otter_286_noNA_rate$gamma <- dgamma(otter_286_noNA_rate$km_hr, shape  = fit_gamma_286_rate$estimate[1], rate = fit_gamma_286_rate$estimate[2], log = FALSE)

otter_286_noNA_rate$weibull <- dweibull(otter_286_noNA_rate$km_hr, shape = fit_weibull_286_rate$estimate[1], scale = fit_weibull_286_rate$estimate[2], log = FALSE)

otter_286_noNA_rate$cauchy <- dcauchy(otter_286_noNA_rate$km_hr, location = fit_cauchy_286_rate$estimate[1], scale = fit_cauchy_286_rate$estimate[2], log = FALSE)
#otter_286_noNA$cauchy2 <- dcauchy(otter_286_noNA$dist, location = fit_cauchy_286$estimate[1], scale = fit_cauchy$estimate_286[2], log = FALSE)


#save loglik
otter_286_noNA_rate$log_lik_ray <- fit_rayleigh_286_rate$loglik
otter_286_noNA_rate$log_lik_gamma <- fit_gamma_286_rate$loglik
otter_286_noNA_rate$log_lik_weibull <- fit_weibull_286_rate$loglik
otter_286_noNA_rate$log_lik_cauchy <- fit_cauchy_286_rate$loglik


#plot over histogram
ggplot(otter_286_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 315 

#create inidividual otter df for model fitting

#individual otter df
otter_315_rate <- resights_dist_recap %>% subset(seaotter == "315")
otter_315_rate <- as.data.frame(otter_315_rate)
otter_315_noNA_rate <- na.omit(otter_315_rate)


#rayleigh
#fit model
fit_rayleigh_315_rate <- fitdistr(otter_315_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_315_rate)

#gamma
#fit model
fit_gamma_315_rate <- fitdistr(otter_315_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_315_rate)

#weibull
#fit model
fit_weibull_315_rate <- fitdistr(otter_315_noNA_rate$km_hr,"weibull")
str(fit_weibull_315_rate)

#cauchy
#fit model
fit_cauchy_315_rate <- fitdistr(otter_315_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_315_rate)

#estimates from models
otter_315_noNA_rate$rayleigh <- drayleigh(otter_315_noNA_rate$km_hr, scale = fit_rayleigh_315_rate$estimate, log = FALSE)

otter_315_noNA_rate$gamma <- dgamma(otter_315_noNA_rate$km_hr, shape  = fit_gamma_315_rate$estimate[1], rate = fit_gamma_315_rate$estimate[2], log = FALSE)

otter_315_noNA_rate$weibull <- dweibull(otter_315_noNA_rate$km_hr, shape = fit_weibull_315_rate$estimate[1], scale = fit_weibull_315_rate$estimate[2], log = FALSE)

otter_315_noNA_rate$cauchy <- dcauchy(otter_315_noNA_rate$km_hr, location = fit_cauchy_315_rate$estimate[1], scale = fit_cauchy_315_rate$estimate[2], log = FALSE)
#otter_315_noNA$cauchy2 <- dcauchy(otter_315_noNA$dist, location = fit_cauchy_315$estimate[1], scale = fit_cauchy$estimate_315[2], log = FALSE)


#save loglik
otter_315_noNA_rate$log_lik_ray <- fit_rayleigh_315_rate$loglik
otter_315_noNA_rate$log_lik_gamma <- fit_gamma_315_rate$loglik
otter_315_noNA_rate$log_lik_weibull <- fit_weibull_315_rate$loglik
otter_315_noNA_rate$log_lik_cauchy <- fit_cauchy_315_rate$loglik


#plot over histogram
ggplot(otter_315_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 327 

#create inidividual otter df for model fitting

#individual otter df
otter_327_rate <- resights_dist_recap %>% subset(seaotter == "327")
otter_327_rate <- as.data.frame(otter_327_rate)
otter_327_noNA_rate <- na.omit(otter_327_rate)


#rayleigh
#fit model
fit_rayleigh_327_rate <- fitdistr(otter_327_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_327_rate)

#gamma
#fit model
fit_gamma_327_rate <- fitdistr(otter_327_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_327_rate)

#weibull
#fit model
fit_weibull_327_rate <- fitdistr(otter_327_noNA_rate$km_hr,"weibull")
str(fit_weibull_327_rate)

#cauchy
#fit model
fit_cauchy_327_rate <- fitdistr(otter_327_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_327_rate)

#estimates from models
otter_327_noNA_rate$rayleigh <- drayleigh(otter_327_noNA_rate$km_hr, scale = fit_rayleigh_327_rate$estimate, log = FALSE)

otter_327_noNA_rate$gamma <- dgamma(otter_327_noNA_rate$km_hr, shape  = fit_gamma_327_rate$estimate[1], rate = fit_gamma_327_rate$estimate[2], log = FALSE)

otter_327_noNA_rate$weibull <- dweibull(otter_327_noNA_rate$km_hr, shape = fit_weibull_327_rate$estimate[1], scale = fit_weibull_327_rate$estimate[2], log = FALSE)

otter_327_noNA_rate$cauchy <- dcauchy(otter_327_noNA_rate$km_hr, location = fit_cauchy_327_rate$estimate[1], scale = fit_cauchy_327_rate$estimate[2], log = FALSE)
#otter_327_noNA$cauchy2 <- dcauchy(otter_327_noNA$dist, location = fit_cauchy_327$estimate[1], scale = fit_cauchy$estimate_327[2], log = FALSE)


#save loglik
otter_327_noNA_rate$log_lik_ray <- fit_rayleigh_327_rate$loglik
otter_327_noNA_rate$log_lik_gamma <- fit_gamma_327_rate$loglik
otter_327_noNA_rate$log_lik_weibull <- fit_weibull_327_rate$loglik
otter_327_noNA_rate$log_lik_cauchy <- fit_cauchy_327_rate$loglik


#plot over histogram
ggplot(otter_327_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 339 
#create inidividual otter df for model fitting

#individual otter df
otter_339_rate <- resights_dist_recap %>% subset(seaotter == "339")
otter_339_rate <- as.data.frame(otter_339_rate)
otter_339_noNA_rate <- na.omit(otter_339_rate)


#rayleigh
#fit model
fit_rayleigh_339_rate <- fitdistr(otter_339_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_339_rate)

#gamma
#fit model
fit_gamma_339_rate <- fitdistr(otter_339_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_339_rate)

#weibull
#fit model
fit_weibull_339_rate <- fitdistr(otter_339_noNA_rate$km_hr,"weibull")
str(fit_weibull_339_rate)

#cauchy
#fit model
fit_cauchy_339_rate <- fitdistr(otter_339_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_339_rate)

#estimates from models
otter_339_noNA_rate$rayleigh <- drayleigh(otter_339_noNA_rate$km_hr, scale = fit_rayleigh_339_rate$estimate, log = FALSE)

otter_339_noNA_rate$gamma <- dgamma(otter_339_noNA_rate$km_hr, shape  = fit_gamma_339_rate$estimate[1], rate = fit_gamma_339_rate$estimate[2], log = FALSE)

otter_339_noNA_rate$weibull <- dweibull(otter_339_noNA_rate$km_hr, shape = fit_weibull_339_rate$estimate[1], scale = fit_weibull_339_rate$estimate[2], log = FALSE)

otter_339_noNA_rate$cauchy <- dcauchy(otter_339_noNA_rate$km_hr, location = fit_cauchy_339_rate$estimate[1], scale = fit_cauchy_339_rate$estimate[2], log = FALSE)
#otter_339_noNA$cauchy2 <- dcauchy(otter_339_noNA$dist, location = fit_cauchy_339$estimate[1], scale = fit_cauchy$estimate_339[2], log = FALSE)


#save loglik
otter_339_noNA_rate$log_lik_ray <- fit_rayleigh_339_rate$loglik
otter_339_noNA_rate$log_lik_gamma <- fit_gamma_339_rate$loglik
otter_339_noNA_rate$log_lik_weibull <- fit_weibull_339_rate$loglik
otter_339_noNA_rate$log_lik_cauchy <- fit_cauchy_339_rate$loglik


#plot over histogram
ggplot(otter_339_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 344 
#create inidividual otter df for model fitting

#individual otter df
otter_344_rate <- resights_dist_recap %>% subset(seaotter == "344")
otter_344_rate <- as.data.frame(otter_344_rate)
otter_344_noNA_rate <- na.omit(otter_344_rate)


#rayleigh
#fit model
fit_rayleigh_344_rate <- fitdistr(otter_344_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_344_rate)

#gamma
#fit model
fit_gamma_344_rate <- fitdistr(otter_344_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_344_rate)

#weibull
#fit model
fit_weibull_344_rate <- fitdistr(otter_344_noNA_rate$km_hr,"weibull")
str(fit_weibull_344_rate)

#cauchy
#fit model
fit_cauchy_344_rate <- fitdistr(otter_344_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_344_rate)

#estimates from models
otter_344_noNA_rate$rayleigh <- drayleigh(otter_344_noNA_rate$km_hr, scale = fit_rayleigh_344_rate$estimate, log = FALSE)

otter_344_noNA_rate$gamma <- dgamma(otter_344_noNA_rate$km_hr, shape  = fit_gamma_344_rate$estimate[1], rate = fit_gamma_344_rate$estimate[2], log = FALSE)

otter_344_noNA_rate$weibull <- dweibull(otter_344_noNA_rate$km_hr, shape = fit_weibull_344_rate$estimate[1], scale = fit_weibull_344_rate$estimate[2], log = FALSE)

otter_344_noNA_rate$cauchy <- dcauchy(otter_344_noNA_rate$km_hr, location = fit_cauchy_344_rate$estimate[1], scale = fit_cauchy_344_rate$estimate[2], log = FALSE)
#otter_344_noNA$cauchy2 <- dcauchy(otter_344_noNA$dist, location = fit_cauchy_344$estimate[1], scale = fit_cauchy$estimate_344[2], log = FALSE)


#save loglik
otter_344_noNA_rate$log_lik_ray <- fit_rayleigh_344_rate$loglik
otter_344_noNA_rate$log_lik_gamma <- fit_gamma_344_rate$loglik
otter_344_noNA_rate$log_lik_weibull <- fit_weibull_344_rate$loglik
otter_344_noNA_rate$log_lik_cauchy <- fit_cauchy_344_rate$loglik


#plot over histogram
ggplot(otter_344_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 353 
#create inidividual otter df for model fitting

#individual otter df
otter_353_rate <- resights_dist_recap %>% subset(seaotter == "353")
otter_353_rate <- as.data.frame(otter_353_rate)
otter_353_noNA_rate <- na.omit(otter_353_rate)


#rayleigh
#fit model
fit_rayleigh_353_rate <- fitdistr(otter_353_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_353_rate)

#gamma
#fit model
fit_gamma_353_rate <- fitdistr(otter_353_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_353_rate)

#weibull
#fit model
fit_weibull_353_rate <- fitdistr(otter_353_noNA_rate$km_hr,"weibull")
str(fit_weibull_353_rate)

#cauchy
#fit model
fit_cauchy_353_rate <- fitdistr(otter_353_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_353_rate)

#estimates from models
otter_353_noNA_rate$rayleigh <- drayleigh(otter_353_noNA_rate$km_hr, scale = fit_rayleigh_353_rate$estimate, log = FALSE)

otter_353_noNA_rate$gamma <- dgamma(otter_353_noNA_rate$km_hr, shape  = fit_gamma_353_rate$estimate[1], rate = fit_gamma_353_rate$estimate[2], log = FALSE)

otter_353_noNA_rate$weibull <- dweibull(otter_353_noNA_rate$km_hr, shape = fit_weibull_353_rate$estimate[1], scale = fit_weibull_353_rate$estimate[2], log = FALSE)

otter_353_noNA_rate$cauchy <- dcauchy(otter_353_noNA_rate$km_hr, location = fit_cauchy_353_rate$estimate[1], scale = fit_cauchy_353_rate$estimate[2], log = FALSE)
#otter_353_noNA$cauchy2 <- dcauchy(otter_353_noNA$dist, location = fit_cauchy_353$estimate[1], scale = fit_cauchy$estimate_353[2], log = FALSE)


#save loglik
otter_353_noNA_rate$log_lik_ray <- fit_rayleigh_353_rate$loglik
otter_353_noNA_rate$log_lik_gamma <- fit_gamma_353_rate$loglik
otter_353_noNA_rate$log_lik_weibull <- fit_weibull_353_rate$loglik
otter_353_noNA_rate$log_lik_cauchy <- fit_cauchy_353_rate$loglik


#plot over histogram
ggplot(otter_353_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 379
#create inidividual otter df for model fitting

#individual otter df
otter_379_rate <- resights_dist_recap %>% subset(seaotter == "379")
otter_379_rate <- as.data.frame(otter_379_rate)
otter_379_noNA_rate <- na.omit(otter_379_rate)

# 
# #rayleigh
# #fit model
# fit_rayleigh_379_rate <- fitdistr(otter_379_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
# str(fit_rayleigh_379_rate)
# 
# #gamma
# #fit model
# fit_gamma_379_rate <- fitdistr(otter_379_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
# str(fit_gamma_379_rate)
# 
# #weibull
# #fit model
# fit_weibull_379_rate <- fitdistr(otter_379_noNA_rate$km_hr,"weibull")
# str(fit_weibull_379_rate)

#cauchy
#fit model
fit_cauchy_379_rate <- fitdistr(otter_379_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_379_rate)

#estimates from models
# otter_379_noNA_rate$rayleigh <- drayleigh(otter_379_noNA_rate$km_hr, scale = fit_rayleigh_379_rate$estimate, log = FALSE)
# 
# otter_379_noNA_rate$gamma <- dgamma(otter_379_noNA_rate$km_hr, shape  = fit_gamma_379_rate$estimate[1], rate = fit_gamma_379_rate$estimate[2], log = FALSE)
# 
# otter_379_noNA_rate$weibull <- dweibull(otter_379_noNA_rate$km_hr, shape = fit_weibull_379_rate$estimate[1], scale = fit_weibull_379_rate$estimate[2], log = FALSE)

otter_379_noNA_rate$cauchy <- dcauchy(otter_379_noNA_rate$km_hr, location = fit_cauchy_379_rate$estimate[1], scale = fit_cauchy_379_rate$estimate[2], log = FALSE)
#otter_379_noNA$cauchy2 <- dcauchy(otter_379_noNA$dist, location = fit_cauchy_379$estimate[1], scale = fit_cauchy$estimate_379[2], log = FALSE)


#save loglik
# otter_379_noNA_rate$log_lik_ray <- fit_rayleigh_379_rate$loglik
# otter_379_noNA_rate$log_lik_gamma <- fit_gamma_379_rate$loglik
# otter_379_noNA_rate$log_lik_weibull <- fit_weibull_379_rate$loglik
otter_379_noNA_rate$log_lik_cauchy <- fit_cauchy_379_rate$loglik


#plot over histogram
ggplot(otter_379_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  # geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  # geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  # geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 386 
#create inidividual otter df for model fitting

#individual otter df
otter_386_rate <- resights_dist_recap %>% subset(seaotter == "386")
otter_386_rate <- as.data.frame(otter_386_rate)
otter_386_noNA_rate <- na.omit(otter_386_rate)


#rayleigh
#fit model
fit_rayleigh_386_rate <- fitdistr(otter_386_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_386_rate)

#gamma
#fit model
fit_gamma_386_rate <- fitdistr(otter_386_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_386_rate)

#weibull
#fit model
fit_weibull_386_rate <- fitdistr(otter_386_noNA_rate$km_hr,"weibull")
str(fit_weibull_386_rate)

#cauchy
#fit model
fit_cauchy_386_rate <- fitdistr(otter_386_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_386_rate)

#estimates from models
otter_386_noNA_rate$rayleigh <- drayleigh(otter_386_noNA_rate$km_hr, scale = fit_rayleigh_386_rate$estimate, log = FALSE)

otter_386_noNA_rate$gamma <- dgamma(otter_386_noNA_rate$km_hr, shape  = fit_gamma_386_rate$estimate[1], rate = fit_gamma_386_rate$estimate[2], log = FALSE)

otter_386_noNA_rate$weibull <- dweibull(otter_386_noNA_rate$km_hr, shape = fit_weibull_386_rate$estimate[1], scale = fit_weibull_386_rate$estimate[2], log = FALSE)

otter_386_noNA_rate$cauchy <- dcauchy(otter_386_noNA_rate$km_hr, location = fit_cauchy_386_rate$estimate[1], scale = fit_cauchy_386_rate$estimate[2], log = FALSE)
#otter_386_noNA$cauchy2 <- dcauchy(otter_386_noNA$dist, location = fit_cauchy_386$estimate[1], scale = fit_cauchy$estimate_386[2], log = FALSE)


#save loglik
otter_386_noNA_rate$log_lik_ray <- fit_rayleigh_386_rate$loglik
otter_386_noNA_rate$log_lik_gamma <- fit_gamma_386_rate$loglik
otter_386_noNA_rate$log_lik_weibull <- fit_weibull_386_rate$loglik
otter_386_noNA_rate$log_lik_cauchy <- fit_cauchy_386_rate$loglik


#plot over histogram
ggplot(otter_386_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 433 
#create inidividual otter df for model fitting

#individual otter df
otter_433_rate <- resights_dist_recap %>% subset(seaotter == "433")
otter_433_rate <- as.data.frame(otter_433_rate)
otter_433_noNA_rate <- na.omit(otter_433_rate)


#rayleigh
#fit model
fit_rayleigh_433_rate <- fitdistr(otter_433_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_433_rate)

#gamma
#fit model
fit_gamma_433_rate <- fitdistr(otter_433_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_433_rate)

#weibull
#fit model
fit_weibull_433_rate <- fitdistr(otter_433_noNA_rate$km_hr,"weibull")
str(fit_weibull_433_rate)

#cauchy
#fit model
fit_cauchy_433_rate <- fitdistr(otter_433_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_433_rate)

#estimates from models
otter_433_noNA_rate$rayleigh <- drayleigh(otter_433_noNA_rate$km_hr, scale = fit_rayleigh_433_rate$estimate, log = FALSE)

otter_433_noNA_rate$gamma <- dgamma(otter_433_noNA_rate$km_hr, shape  = fit_gamma_433_rate$estimate[1], rate = fit_gamma_433_rate$estimate[2], log = FALSE)

otter_433_noNA_rate$weibull <- dweibull(otter_433_noNA_rate$km_hr, shape = fit_weibull_433_rate$estimate[1], scale = fit_weibull_433_rate$estimate[2], log = FALSE)

otter_433_noNA_rate$cauchy <- dcauchy(otter_433_noNA_rate$km_hr, location = fit_cauchy_433_rate$estimate[1], scale = fit_cauchy_433_rate$estimate[2], log = FALSE)
#otter_433_noNA$cauchy2 <- dcauchy(otter_433_noNA$dist, location = fit_cauchy_433$estimate[1], scale = fit_cauchy$estimate_433[2], log = FALSE)


#save loglik
otter_433_noNA_rate$log_lik_ray <- fit_rayleigh_433_rate$loglik
otter_433_noNA_rate$log_lik_gamma <- fit_gamma_433_rate$loglik
otter_433_noNA_rate$log_lik_weibull <- fit_weibull_433_rate$loglik
otter_433_noNA_rate$log_lik_cauchy <- fit_cauchy_433_rate$loglik


#plot over histogram
ggplot(otter_433_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 451 
#create inidividual otter df for model fitting

#individual otter df
otter_451_rate <- resights_dist_recap %>% subset(seaotter == "451")
otter_451_rate <- as.data.frame(otter_451_rate)
otter_451_noNA_rate <- na.omit(otter_451_rate)


#rayleigh
#fit model
fit_rayleigh_451_rate <- fitdistr(otter_451_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_451_rate)

#gamma
#fit model
fit_gamma_451_rate <- fitdistr(otter_451_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_451_rate)

#weibull
#fit model
fit_weibull_451_rate <- fitdistr(otter_451_noNA_rate$km_hr,"weibull")
str(fit_weibull_451_rate)

#cauchy
#fit model
fit_cauchy_451_rate <- fitdistr(otter_451_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_451_rate)

#estimates from models
otter_451_noNA_rate$rayleigh <- drayleigh(otter_451_noNA_rate$km_hr, scale = fit_rayleigh_451_rate$estimate, log = FALSE)

otter_451_noNA_rate$gamma <- dgamma(otter_451_noNA_rate$km_hr, shape  = fit_gamma_451_rate$estimate[1], rate = fit_gamma_451_rate$estimate[2], log = FALSE)

otter_451_noNA_rate$weibull <- dweibull(otter_451_noNA_rate$km_hr, shape = fit_weibull_451_rate$estimate[1], scale = fit_weibull_451_rate$estimate[2], log = FALSE)

otter_451_noNA_rate$cauchy <- dcauchy(otter_451_noNA_rate$km_hr, location = fit_cauchy_451_rate$estimate[1], scale = fit_cauchy_451_rate$estimate[2], log = FALSE)
#otter_451_noNA$cauchy2 <- dcauchy(otter_451_noNA$dist, location = fit_cauchy_451$estimate[1], scale = fit_cauchy$estimate_451[2], log = FALSE)


#save loglik
otter_451_noNA_rate$log_lik_ray <- fit_rayleigh_451_rate$loglik
otter_451_noNA_rate$log_lik_gamma <- fit_gamma_451_rate$loglik
otter_451_noNA_rate$log_lik_weibull <- fit_weibull_451_rate$loglik
otter_451_noNA_rate$log_lik_cauchy <- fit_cauchy_451_rate$loglik


#plot over histogram
ggplot(otter_451_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 457 
#create inidividual otter df for model fitting

#individual otter df
otter_457_rate <- resights_dist_recap %>% subset(seaotter == "457")
otter_457_rate <- as.data.frame(otter_457_rate)
otter_457_noNA_rate <- na.omit(otter_457_rate)


#rayleigh
#fit model
fit_rayleigh_457_rate <- fitdistr(otter_457_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_457_rate)

#gamma
#fit model
fit_gamma_457_rate <- fitdistr(otter_457_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_457_rate)

#weibull
#fit model
fit_weibull_457_rate <- fitdistr(otter_457_noNA_rate$km_hr,"weibull")
str(fit_weibull_457_rate)

#cauchy
#fit model
fit_cauchy_457_rate <- fitdistr(otter_457_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_457_rate)

#estimates from models
otter_457_noNA_rate$rayleigh <- drayleigh(otter_457_noNA_rate$km_hr, scale = fit_rayleigh_457_rate$estimate, log = FALSE)

otter_457_noNA_rate$gamma <- dgamma(otter_457_noNA_rate$km_hr, shape  = fit_gamma_457_rate$estimate[1], rate = fit_gamma_457_rate$estimate[2], log = FALSE)

otter_457_noNA_rate$weibull <- dweibull(otter_457_noNA_rate$km_hr, shape = fit_weibull_457_rate$estimate[1], scale = fit_weibull_457_rate$estimate[2], log = FALSE)

otter_457_noNA_rate$cauchy <- dcauchy(otter_457_noNA_rate$km_hr, location = fit_cauchy_457_rate$estimate[1], scale = fit_cauchy_457_rate$estimate[2], log = FALSE)
#otter_457_noNA$cauchy2 <- dcauchy(otter_457_noNA$dist, location = fit_cauchy_457$estimate[1], scale = fit_cauchy$estimate_457[2], log = FALSE)


#save loglik
otter_457_noNA_rate$log_lik_ray <- fit_rayleigh_457_rate$loglik
otter_457_noNA_rate$log_lik_gamma <- fit_gamma_457_rate$loglik
otter_457_noNA_rate$log_lik_weibull <- fit_weibull_457_rate$loglik
otter_457_noNA_rate$log_lik_cauchy <- fit_cauchy_457_rate$loglik


#plot over histogram
ggplot(otter_457_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 466 
#create inidividual otter df for model fitting

#individual otter df
otter_466_rate <- resights_dist_recap %>% subset(seaotter == "466")
otter_466_rate <- as.data.frame(otter_466_rate)
otter_466_noNA_rate <- na.omit(otter_466_rate)


#rayleigh
#fit model
fit_rayleigh_466_rate <- fitdistr(otter_466_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_466_rate)

#gamma
#fit model
fit_gamma_466_rate <- fitdistr(otter_466_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_466_rate)

#weibull
#fit model
fit_weibull_466_rate <- fitdistr(otter_466_noNA_rate$km_hr,"weibull")
str(fit_weibull_466_rate)

#cauchy
#fit model
fit_cauchy_466_rate <- fitdistr(otter_466_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_466_rate)

#estimates from models
otter_466_noNA_rate$rayleigh <- drayleigh(otter_466_noNA_rate$km_hr, scale = fit_rayleigh_466_rate$estimate, log = FALSE)

otter_466_noNA_rate$gamma <- dgamma(otter_466_noNA_rate$km_hr, shape  = fit_gamma_466_rate$estimate[1], rate = fit_gamma_466_rate$estimate[2], log = FALSE)

otter_466_noNA_rate$weibull <- dweibull(otter_466_noNA_rate$km_hr, shape = fit_weibull_466_rate$estimate[1], scale = fit_weibull_466_rate$estimate[2], log = FALSE)

otter_466_noNA_rate$cauchy <- dcauchy(otter_466_noNA_rate$km_hr, location = fit_cauchy_466_rate$estimate[1], scale = fit_cauchy_466_rate$estimate[2], log = FALSE)
#otter_466_noNA$cauchy2 <- dcauchy(otter_466_noNA$dist, location = fit_cauchy_466$estimate[1], scale = fit_cauchy$estimate_466[2], log = FALSE)


#save loglik
otter_466_noNA_rate$log_lik_ray <- fit_rayleigh_466_rate$loglik
otter_466_noNA_rate$log_lik_gamma <- fit_gamma_466_rate$loglik
otter_466_noNA_rate$log_lik_weibull <- fit_weibull_466_rate$loglik
otter_466_noNA_rate$log_lik_cauchy <- fit_cauchy_466_rate$loglik


#plot over histogram
ggplot(otter_466_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 473 
#create inidividual otter df for model fitting

#individual otter df
otter_473_rate <- resights_dist_recap %>% subset(seaotter == "473")
otter_473_rate <- as.data.frame(otter_473_rate)
otter_473_noNA_rate <- na.omit(otter_473_rate)


#rayleigh
#fit model
fit_rayleigh_473_rate <- fitdistr(otter_473_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_473_rate)

#gamma
#fit model
fit_gamma_473_rate <- fitdistr(otter_473_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_473_rate)

#weibull
#fit model
fit_weibull_473_rate <- fitdistr(otter_473_noNA_rate$km_hr,"weibull")
str(fit_weibull_473_rate)

#cauchy
#fit model
fit_cauchy_473_rate <- fitdistr(otter_473_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_473_rate)

#estimates from models
otter_473_noNA_rate$rayleigh <- drayleigh(otter_473_noNA_rate$km_hr, scale = fit_rayleigh_473_rate$estimate, log = FALSE)

otter_473_noNA_rate$gamma <- dgamma(otter_473_noNA_rate$km_hr, shape  = fit_gamma_473_rate$estimate[1], rate = fit_gamma_473_rate$estimate[2], log = FALSE)

otter_473_noNA_rate$weibull <- dweibull(otter_473_noNA_rate$km_hr, shape = fit_weibull_473_rate$estimate[1], scale = fit_weibull_473_rate$estimate[2], log = FALSE)

otter_473_noNA_rate$cauchy <- dcauchy(otter_473_noNA_rate$km_hr, location = fit_cauchy_473_rate$estimate[1], scale = fit_cauchy_473_rate$estimate[2], log = FALSE)
#otter_473_noNA$cauchy2 <- dcauchy(otter_473_noNA$dist, location = fit_cauchy_473$estimate[1], scale = fit_cauchy$estimate_473[2], log = FALSE)


#save loglik
otter_473_noNA_rate$log_lik_ray <- fit_rayleigh_473_rate$loglik
otter_473_noNA_rate$log_lik_gamma <- fit_gamma_473_rate$loglik
otter_473_noNA_rate$log_lik_weibull <- fit_weibull_473_rate$loglik
otter_473_noNA_rate$log_lik_cauchy <- fit_cauchy_473_rate$loglik


#plot over histogram
ggplot(otter_473_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 475 
#create inidividual otter df for model fitting

#individual otter df
otter_475_rate <- resights_dist_recap %>% subset(seaotter == "475")
otter_475_rate <- as.data.frame(otter_475_rate)
otter_475_noNA_rate <- na.omit(otter_475_rate)


#rayleigh
#fit model
fit_rayleigh_475_rate <- fitdistr(otter_475_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_475_rate)

#gamma
#fit model
fit_gamma_475_rate <- fitdistr(otter_475_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_475_rate)

#weibull
#fit model
fit_weibull_475_rate <- fitdistr(otter_475_noNA_rate$km_hr,"weibull")
str(fit_weibull_475_rate)

#cauchy
#fit model
fit_cauchy_475_rate <- fitdistr(otter_475_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_475_rate)

#estimates from models
otter_475_noNA_rate$rayleigh <- drayleigh(otter_475_noNA_rate$km_hr, scale = fit_rayleigh_475_rate$estimate, log = FALSE)

otter_475_noNA_rate$gamma <- dgamma(otter_475_noNA_rate$km_hr, shape  = fit_gamma_475_rate$estimate[1], rate = fit_gamma_475_rate$estimate[2], log = FALSE)

otter_475_noNA_rate$weibull <- dweibull(otter_475_noNA_rate$km_hr, shape = fit_weibull_475_rate$estimate[1], scale = fit_weibull_475_rate$estimate[2], log = FALSE)

otter_475_noNA_rate$cauchy <- dcauchy(otter_475_noNA_rate$km_hr, location = fit_cauchy_475_rate$estimate[1], scale = fit_cauchy_475_rate$estimate[2], log = FALSE)
#otter_475_noNA$cauchy2 <- dcauchy(otter_475_noNA$dist, location = fit_cauchy_475$estimate[1], scale = fit_cauchy$estimate_475[2], log = FALSE)


#save loglik
otter_475_noNA_rate$log_lik_ray <- fit_rayleigh_475_rate$loglik
otter_475_noNA_rate$log_lik_gamma <- fit_gamma_475_rate$loglik
otter_475_noNA_rate$log_lik_weibull <- fit_weibull_475_rate$loglik
otter_475_noNA_rate$log_lik_cauchy <- fit_cauchy_475_rate$loglik


#plot over histogram
ggplot(otter_475_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 501 
#create inidividual otter df for model fitting

#individual otter df
otter_501_rate <- resights_dist_recap %>% subset(seaotter == "501")
otter_501_rate <- as.data.frame(otter_501_rate)
otter_501_noNA_rate <- na.omit(otter_501_rate)


#rayleigh
#fit model
fit_rayleigh_501_rate <- fitdistr(otter_501_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_501_rate)

#gamma
#fit model
fit_gamma_501_rate <- fitdistr(otter_501_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_501_rate)

#weibull
#fit model
fit_weibull_501_rate <- fitdistr(otter_501_noNA_rate$km_hr,"weibull")
str(fit_weibull_501_rate)

#cauchy
#fit model
fit_cauchy_501_rate <- fitdistr(otter_501_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_501_rate)

#estimates from models
otter_501_noNA_rate$rayleigh <- drayleigh(otter_501_noNA_rate$km_hr, scale = fit_rayleigh_501_rate$estimate, log = FALSE)

otter_501_noNA_rate$gamma <- dgamma(otter_501_noNA_rate$km_hr, shape  = fit_gamma_501_rate$estimate[1], rate = fit_gamma_501_rate$estimate[2], log = FALSE)

otter_501_noNA_rate$weibull <- dweibull(otter_501_noNA_rate$km_hr, shape = fit_weibull_501_rate$estimate[1], scale = fit_weibull_501_rate$estimate[2], log = FALSE)

otter_501_noNA_rate$cauchy <- dcauchy(otter_501_noNA_rate$km_hr, location = fit_cauchy_501_rate$estimate[1], scale = fit_cauchy_501_rate$estimate[2], log = FALSE)
#otter_501_noNA$cauchy2 <- dcauchy(otter_501_noNA$dist, location = fit_cauchy_501$estimate[1], scale = fit_cauchy$estimate_501[2], log = FALSE)


#save loglik
otter_501_noNA_rate$log_lik_ray <- fit_rayleigh_501_rate$loglik
otter_501_noNA_rate$log_lik_gamma <- fit_gamma_501_rate$loglik
otter_501_noNA_rate$log_lik_weibull <- fit_weibull_501_rate$loglik
otter_501_noNA_rate$log_lik_cauchy <- fit_cauchy_501_rate$loglik


#plot over histogram
ggplot(otter_501_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 518 
#create inidividual otter df for model fitting

#individual otter df
otter_518_rate <- resights_dist_recap %>% subset(seaotter == "518")
otter_518_rate <- as.data.frame(otter_518_rate)
otter_518_noNA_rate <- na.omit(otter_518_rate)


#rayleigh
#fit model
fit_rayleigh_518_rate <- fitdistr(otter_518_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_518_rate)

#gamma
#fit model
fit_gamma_518_rate <- fitdistr(otter_518_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_518_rate)

#weibull
#fit model
fit_weibull_518_rate <- fitdistr(otter_518_noNA_rate$km_hr,"weibull")
str(fit_weibull_518_rate)

#cauchy
#fit model
fit_cauchy_518_rate <- fitdistr(otter_518_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_518_rate)

#estimates from models
otter_518_noNA_rate$rayleigh <- drayleigh(otter_518_noNA_rate$km_hr, scale = fit_rayleigh_518_rate$estimate, log = FALSE)

otter_518_noNA_rate$gamma <- dgamma(otter_518_noNA_rate$km_hr, shape  = fit_gamma_518_rate$estimate[1], rate = fit_gamma_518_rate$estimate[2], log = FALSE)

otter_518_noNA_rate$weibull <- dweibull(otter_518_noNA_rate$km_hr, shape = fit_weibull_518_rate$estimate[1], scale = fit_weibull_518_rate$estimate[2], log = FALSE)

otter_518_noNA_rate$cauchy <- dcauchy(otter_518_noNA_rate$km_hr, location = fit_cauchy_518_rate$estimate[1], scale = fit_cauchy_518_rate$estimate[2], log = FALSE)
#otter_518_noNA$cauchy2 <- dcauchy(otter_518_noNA$dist, location = fit_cauchy_518$estimate[1], scale = fit_cauchy$estimate_518[2], log = FALSE)


#save loglik
otter_518_noNA_rate$log_lik_ray <- fit_rayleigh_518_rate$loglik
otter_518_noNA_rate$log_lik_gamma <- fit_gamma_518_rate$loglik
otter_518_noNA_rate$log_lik_weibull <- fit_weibull_518_rate$loglik
otter_518_noNA_rate$log_lik_cauchy <- fit_cauchy_518_rate$loglik


#plot over histogram
ggplot(otter_518_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 520 
#create inidividual otter df for model fitting

#individual otter df
otter_520_rate <- resights_dist_recap %>% subset(seaotter == "520")
otter_520_rate <- as.data.frame(otter_520_rate)
otter_520_noNA_rate <- na.omit(otter_520_rate)


#rayleigh
#fit model
fit_rayleigh_520_rate <- fitdistr(otter_520_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_520_rate)

#gamma
#fit model
fit_gamma_520_rate <- fitdistr(otter_520_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_520_rate)

#weibull
#fit model
fit_weibull_520_rate <- fitdistr(otter_520_noNA_rate$km_hr,"weibull")
str(fit_weibull_520_rate)

#cauchy
#fit model
fit_cauchy_520_rate <- fitdistr(otter_520_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_520_rate)

#estimates from models
otter_520_noNA_rate$rayleigh <- drayleigh(otter_520_noNA_rate$km_hr, scale = fit_rayleigh_520_rate$estimate, log = FALSE)

otter_520_noNA_rate$gamma <- dgamma(otter_520_noNA_rate$km_hr, shape  = fit_gamma_520_rate$estimate[1], rate = fit_gamma_520_rate$estimate[2], log = FALSE)

otter_520_noNA_rate$weibull <- dweibull(otter_520_noNA_rate$km_hr, shape = fit_weibull_520_rate$estimate[1], scale = fit_weibull_520_rate$estimate[2], log = FALSE)

otter_520_noNA_rate$cauchy <- dcauchy(otter_520_noNA_rate$km_hr, location = fit_cauchy_520_rate$estimate[1], scale = fit_cauchy_520_rate$estimate[2], log = FALSE)
#otter_520_noNA$cauchy2 <- dcauchy(otter_520_noNA$dist, location = fit_cauchy_520$estimate[1], scale = fit_cauchy$estimate_520[2], log = FALSE)


#save loglik
otter_520_noNA_rate$log_lik_ray <- fit_rayleigh_520_rate$loglik
otter_520_noNA_rate$log_lik_gamma <- fit_gamma_520_rate$loglik
otter_520_noNA_rate$log_lik_weibull <- fit_weibull_520_rate$loglik
otter_520_noNA_rate$log_lik_cauchy <- fit_cauchy_520_rate$loglik


#plot over histogram
ggplot(otter_520_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 526 
#create inidividual otter df for model fitting

#individual otter df
otter_526_rate <- resights_dist_recap %>% subset(seaotter == "526")
otter_526_rate <- as.data.frame(otter_526_rate)
otter_526_noNA_rate <- na.omit(otter_526_rate)


#rayleigh
#fit model
fit_rayleigh_526_rate <- fitdistr(otter_526_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_526_rate)

#gamma
#fit model
fit_gamma_526_rate <- fitdistr(otter_526_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_526_rate)

#weibull
#fit model
fit_weibull_526_rate <- fitdistr(otter_526_noNA_rate$km_hr,"weibull")
str(fit_weibull_526_rate)

#cauchy
#fit model
fit_cauchy_526_rate <- fitdistr(otter_526_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_526_rate)

#estimates from models
otter_526_noNA_rate$rayleigh <- drayleigh(otter_526_noNA_rate$km_hr, scale = fit_rayleigh_526_rate$estimate, log = FALSE)

otter_526_noNA_rate$gamma <- dgamma(otter_526_noNA_rate$km_hr, shape  = fit_gamma_526_rate$estimate[1], rate = fit_gamma_526_rate$estimate[2], log = FALSE)

otter_526_noNA_rate$weibull <- dweibull(otter_526_noNA_rate$km_hr, shape = fit_weibull_526_rate$estimate[1], scale = fit_weibull_526_rate$estimate[2], log = FALSE)

otter_526_noNA_rate$cauchy <- dcauchy(otter_526_noNA_rate$km_hr, location = fit_cauchy_526_rate$estimate[1], scale = fit_cauchy_526_rate$estimate[2], log = FALSE)
#otter_526_noNA$cauchy2 <- dcauchy(otter_526_noNA$dist, location = fit_cauchy_526$estimate[1], scale = fit_cauchy$estimate_526[2], log = FALSE)


#save loglik
otter_526_noNA_rate$log_lik_ray <- fit_rayleigh_526_rate$loglik
otter_526_noNA_rate$log_lik_gamma <- fit_gamma_526_rate$loglik
otter_526_noNA_rate$log_lik_weibull <- fit_weibull_526_rate$loglik
otter_526_noNA_rate$log_lik_cauchy <- fit_cauchy_526_rate$loglik


#plot over histogram
ggplot(otter_526_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 558 
#create inidividual otter df for model fitting

#individual otter df
otter_558_rate <- resights_dist_recap %>% subset(seaotter == "558")
otter_558_rate <- as.data.frame(otter_558_rate)
otter_558_noNA_rate <- na.omit(otter_558_rate)


#rayleigh
#fit model
fit_rayleigh_558_rate <- fitdistr(otter_558_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_558_rate)

#gamma
#fit model
fit_gamma_558_rate <- fitdistr(otter_558_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_558_rate)

#weibull
#fit model
fit_weibull_558_rate <- fitdistr(otter_558_noNA_rate$km_hr,"weibull")
str(fit_weibull_558_rate)

#cauchy
#fit model
fit_cauchy_558_rate <- fitdistr(otter_558_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_558_rate)

#estimates from models
otter_558_noNA_rate$rayleigh <- drayleigh(otter_558_noNA_rate$km_hr, scale = fit_rayleigh_558_rate$estimate, log = FALSE)

otter_558_noNA_rate$gamma <- dgamma(otter_558_noNA_rate$km_hr, shape  = fit_gamma_558_rate$estimate[1], rate = fit_gamma_558_rate$estimate[2], log = FALSE)

otter_558_noNA_rate$weibull <- dweibull(otter_558_noNA_rate$km_hr, shape = fit_weibull_558_rate$estimate[1], scale = fit_weibull_558_rate$estimate[2], log = FALSE)

otter_558_noNA_rate$cauchy <- dcauchy(otter_558_noNA_rate$km_hr, location = fit_cauchy_558_rate$estimate[1], scale = fit_cauchy_558_rate$estimate[2], log = FALSE)
#otter_558_noNA$cauchy2 <- dcauchy(otter_558_noNA$dist, location = fit_cauchy_558$estimate[1], scale = fit_cauchy$estimate_558[2], log = FALSE)


#save loglik
otter_558_noNA_rate$log_lik_ray <- fit_rayleigh_558_rate$loglik
otter_558_noNA_rate$log_lik_gamma <- fit_gamma_558_rate$loglik
otter_558_noNA_rate$log_lik_weibull <- fit_weibull_558_rate$loglik
otter_558_noNA_rate$log_lik_cauchy <- fit_cauchy_558_rate$loglik


#plot over histogram
ggplot(otter_558_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 587 
#create inidividual otter df for model fitting

#individual otter df
otter_587_rate <- resights_dist_recap %>% subset(seaotter == "587")
otter_587_rate <- as.data.frame(otter_587_rate)
otter_587_noNA_rate <- na.omit(otter_587_rate)


#rayleigh
#fit model
fit_rayleigh_587_rate <- fitdistr(otter_587_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_587_rate)

#gamma
#fit model
fit_gamma_587_rate <- fitdistr(otter_587_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_587_rate)

#weibull
#fit model
fit_weibull_587_rate <- fitdistr(otter_587_noNA_rate$km_hr,"weibull")
str(fit_weibull_587_rate)

#cauchy
#fit model
fit_cauchy_587_rate <- fitdistr(otter_587_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_587_rate)

#estimates from models
otter_587_noNA_rate$rayleigh <- drayleigh(otter_587_noNA_rate$km_hr, scale = fit_rayleigh_587_rate$estimate, log = FALSE)

otter_587_noNA_rate$gamma <- dgamma(otter_587_noNA_rate$km_hr, shape  = fit_gamma_587_rate$estimate[1], rate = fit_gamma_587_rate$estimate[2], log = FALSE)

otter_587_noNA_rate$weibull <- dweibull(otter_587_noNA_rate$km_hr, shape = fit_weibull_587_rate$estimate[1], scale = fit_weibull_587_rate$estimate[2], log = FALSE)

otter_587_noNA_rate$cauchy <- dcauchy(otter_587_noNA_rate$km_hr, location = fit_cauchy_587_rate$estimate[1], scale = fit_cauchy_587_rate$estimate[2], log = FALSE)
#otter_587_noNA$cauchy2 <- dcauchy(otter_587_noNA$dist, location = fit_cauchy_587$estimate[1], scale = fit_cauchy$estimate_587[2], log = FALSE)


#save loglik
otter_587_noNA_rate$log_lik_ray <- fit_rayleigh_587_rate$loglik
otter_587_noNA_rate$log_lik_gamma <- fit_gamma_587_rate$loglik
otter_587_noNA_rate$log_lik_weibull <- fit_weibull_587_rate$loglik
otter_587_noNA_rate$log_lik_cauchy <- fit_cauchy_587_rate$loglik


#plot over histogram
ggplot(otter_587_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 595 
#create inidividual otter df for model fitting

#individual otter df
otter_595_rate <- resights_dist_recap %>% subset(seaotter == "595")
otter_595_rate <- as.data.frame(otter_595_rate)
otter_595_noNA_rate <- na.omit(otter_595_rate)


#rayleigh
#fit model
fit_rayleigh_595_rate <- fitdistr(otter_595_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_595_rate)

#gamma
#fit model
fit_gamma_595_rate <- fitdistr(otter_595_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_595_rate)

#weibull
#fit model
fit_weibull_595_rate <- fitdistr(otter_595_noNA_rate$km_hr,"weibull")
str(fit_weibull_595_rate)

#cauchy
#fit model
fit_cauchy_595_rate <- fitdistr(otter_595_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_595_rate)

#estimates from models
otter_595_noNA_rate$rayleigh <- drayleigh(otter_595_noNA_rate$km_hr, scale = fit_rayleigh_595_rate$estimate, log = FALSE)

otter_595_noNA_rate$gamma <- dgamma(otter_595_noNA_rate$km_hr, shape  = fit_gamma_595_rate$estimate[1], rate = fit_gamma_595_rate$estimate[2], log = FALSE)

otter_595_noNA_rate$weibull <- dweibull(otter_595_noNA_rate$km_hr, shape = fit_weibull_595_rate$estimate[1], scale = fit_weibull_595_rate$estimate[2], log = FALSE)

otter_595_noNA_rate$cauchy <- dcauchy(otter_595_noNA_rate$km_hr, location = fit_cauchy_595_rate$estimate[1], scale = fit_cauchy_595_rate$estimate[2], log = FALSE)
#otter_595_noNA$cauchy2 <- dcauchy(otter_595_noNA$dist, location = fit_cauchy_595$estimate[1], scale = fit_cauchy$estimate_595[2], log = FALSE)


#save loglik
otter_595_noNA_rate$log_lik_ray <- fit_rayleigh_595_rate$loglik
otter_595_noNA_rate$log_lik_gamma <- fit_gamma_595_rate$loglik
otter_595_noNA_rate$log_lik_weibull <- fit_weibull_595_rate$loglik
otter_595_noNA_rate$log_lik_cauchy <- fit_cauchy_595_rate$loglik


#plot over histogram
ggplot(otter_595_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 621 
#create inidividual otter df for model fitting

#individual otter df
otter_621_rate <- resights_dist_recap %>% subset(seaotter == "621")
otter_621_rate <- as.data.frame(otter_621_rate)
otter_621_noNA_rate <- na.omit(otter_621_rate)


#rayleigh
#fit model
fit_rayleigh_621_rate <- fitdistr(otter_621_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_621_rate)

#gamma
#fit model
fit_gamma_621_rate <- fitdistr(otter_621_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_621_rate)

#weibull
#fit model
fit_weibull_621_rate <- fitdistr(otter_621_noNA_rate$km_hr,"weibull")
str(fit_weibull_621_rate)

#cauchy
#fit model
fit_cauchy_621_rate <- fitdistr(otter_621_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_621_rate)

#estimates from models
otter_621_noNA_rate$rayleigh <- drayleigh(otter_621_noNA_rate$km_hr, scale = fit_rayleigh_621_rate$estimate, log = FALSE)

otter_621_noNA_rate$gamma <- dgamma(otter_621_noNA_rate$km_hr, shape  = fit_gamma_621_rate$estimate[1], rate = fit_gamma_621_rate$estimate[2], log = FALSE)

otter_621_noNA_rate$weibull <- dweibull(otter_621_noNA_rate$km_hr, shape = fit_weibull_621_rate$estimate[1], scale = fit_weibull_621_rate$estimate[2], log = FALSE)

otter_621_noNA_rate$cauchy <- dcauchy(otter_621_noNA_rate$km_hr, location = fit_cauchy_621_rate$estimate[1], scale = fit_cauchy_621_rate$estimate[2], log = FALSE)
#otter_621_noNA$cauchy2 <- dcauchy(otter_621_noNA$dist, location = fit_cauchy_621$estimate[1], scale = fit_cauchy$estimate_621[2], log = FALSE)


#save loglik
otter_621_noNA_rate$log_lik_ray <- fit_rayleigh_621_rate$loglik
otter_621_noNA_rate$log_lik_gamma <- fit_gamma_621_rate$loglik
otter_621_noNA_rate$log_lik_weibull <- fit_weibull_621_rate$loglik
otter_621_noNA_rate$log_lik_cauchy <- fit_cauchy_621_rate$loglik


#plot over histogram
ggplot(otter_621_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 623 
#create inidividual otter df for model fitting

#individual otter df
otter_623_rate <- resights_dist_recap %>% subset(seaotter == "623")
otter_623_rate <- as.data.frame(otter_623_rate)
otter_623_noNA_rate <- na.omit(otter_623_rate)


#rayleigh
#fit model
fit_rayleigh_623_rate <- fitdistr(otter_623_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_623_rate)

#gamma
#fit model
fit_gamma_623_rate <- fitdistr(otter_623_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_623_rate)

#weibull
#fit model
fit_weibull_623_rate <- fitdistr(otter_623_noNA_rate$km_hr,"weibull")
str(fit_weibull_623_rate)

#cauchy
#fit model
fit_cauchy_623_rate <- fitdistr(otter_623_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_623_rate)

#estimates from models
otter_623_noNA_rate$rayleigh <- drayleigh(otter_623_noNA_rate$km_hr, scale = fit_rayleigh_623_rate$estimate, log = FALSE)

otter_623_noNA_rate$gamma <- dgamma(otter_623_noNA_rate$km_hr, shape  = fit_gamma_623_rate$estimate[1], rate = fit_gamma_623_rate$estimate[2], log = FALSE)

otter_623_noNA_rate$weibull <- dweibull(otter_623_noNA_rate$km_hr, shape = fit_weibull_623_rate$estimate[1], scale = fit_weibull_623_rate$estimate[2], log = FALSE)

otter_623_noNA_rate$cauchy <- dcauchy(otter_623_noNA_rate$km_hr, location = fit_cauchy_623_rate$estimate[1], scale = fit_cauchy_623_rate$estimate[2], log = FALSE)
#otter_623_noNA$cauchy2 <- dcauchy(otter_623_noNA$dist, location = fit_cauchy_623$estimate[1], scale = fit_cauchy$estimate_623[2], log = FALSE)


#save loglik
otter_623_noNA_rate$log_lik_ray <- fit_rayleigh_623_rate$loglik
otter_623_noNA_rate$log_lik_gamma <- fit_gamma_623_rate$loglik
otter_623_noNA_rate$log_lik_weibull <- fit_weibull_623_rate$loglik
otter_623_noNA_rate$log_lik_cauchy <- fit_cauchy_623_rate$loglik


#plot over histogram
ggplot(otter_623_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 653 
#create inidividual otter df for model fitting

#individual otter df
otter_653_rate <- resights_dist_recap %>% subset(seaotter == "653")
otter_653_rate <- as.data.frame(otter_653_rate)
otter_653_noNA_rate <- na.omit(otter_653_rate)


#rayleigh
#fit model
fit_rayleigh_653_rate <- fitdistr(otter_653_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_653_rate)

#gamma
#fit model
fit_gamma_653_rate <- fitdistr(otter_653_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_653_rate)

#weibull
#fit model
fit_weibull_653_rate <- fitdistr(otter_653_noNA_rate$km_hr,"weibull")
str(fit_weibull_653_rate)

#cauchy
#fit model
fit_cauchy_653_rate <- fitdistr(otter_653_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_653_rate)

#estimates from models
otter_653_noNA_rate$rayleigh <- drayleigh(otter_653_noNA_rate$km_hr, scale = fit_rayleigh_653_rate$estimate, log = FALSE)

otter_653_noNA_rate$gamma <- dgamma(otter_653_noNA_rate$km_hr, shape  = fit_gamma_653_rate$estimate[1], rate = fit_gamma_653_rate$estimate[2], log = FALSE)

otter_653_noNA_rate$weibull <- dweibull(otter_653_noNA_rate$km_hr, shape = fit_weibull_653_rate$estimate[1], scale = fit_weibull_653_rate$estimate[2], log = FALSE)

otter_653_noNA_rate$cauchy <- dcauchy(otter_653_noNA_rate$km_hr, location = fit_cauchy_653_rate$estimate[1], scale = fit_cauchy_653_rate$estimate[2], log = FALSE)
#otter_653_noNA$cauchy2 <- dcauchy(otter_653_noNA$dist, location = fit_cauchy_653$estimate[1], scale = fit_cauchy$estimate_653[2], log = FALSE)


#save loglik
otter_653_noNA_rate$log_lik_ray <- fit_rayleigh_653_rate$loglik
otter_653_noNA_rate$log_lik_gamma <- fit_gamma_653_rate$loglik
otter_653_noNA_rate$log_lik_weibull <- fit_weibull_653_rate$loglik
otter_653_noNA_rate$log_lik_cauchy <- fit_cauchy_653_rate$loglik


#plot over histogram
ggplot(otter_653_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 657 
#create inidividual otter df for model fitting

#individual otter df
otter_657_rate <- resights_dist_recap %>% subset(seaotter == "657")
otter_657_rate <- as.data.frame(otter_657_rate)
otter_657_noNA_rate <- na.omit(otter_657_rate)


#rayleigh
#fit model
fit_rayleigh_657_rate <- fitdistr(otter_657_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_657_rate)

#gamma
#fit model
fit_gamma_657_rate <- fitdistr(otter_657_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_657_rate)

#weibull
#fit model
fit_weibull_657_rate <- fitdistr(otter_657_noNA_rate$km_hr,"weibull")
str(fit_weibull_657_rate)

#cauchy
#fit model
fit_cauchy_657_rate <- fitdistr(otter_657_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_657_rate)

#estimates from models
otter_657_noNA_rate$rayleigh <- drayleigh(otter_657_noNA_rate$km_hr, scale = fit_rayleigh_657_rate$estimate, log = FALSE)

otter_657_noNA_rate$gamma <- dgamma(otter_657_noNA_rate$km_hr, shape  = fit_gamma_657_rate$estimate[1], rate = fit_gamma_657_rate$estimate[2], log = FALSE)

otter_657_noNA_rate$weibull <- dweibull(otter_657_noNA_rate$km_hr, shape = fit_weibull_657_rate$estimate[1], scale = fit_weibull_657_rate$estimate[2], log = FALSE)

otter_657_noNA_rate$cauchy <- dcauchy(otter_657_noNA_rate$km_hr, location = fit_cauchy_657_rate$estimate[1], scale = fit_cauchy_657_rate$estimate[2], log = FALSE)
#otter_657_noNA$cauchy2 <- dcauchy(otter_657_noNA$dist, location = fit_cauchy_657$estimate[1], scale = fit_cauchy$estimate_657[2], log = FALSE)


#save loglik
otter_657_noNA_rate$log_lik_ray <- fit_rayleigh_657_rate$loglik
otter_657_noNA_rate$log_lik_gamma <- fit_gamma_657_rate$loglik
otter_657_noNA_rate$log_lik_weibull <- fit_weibull_657_rate$loglik
otter_657_noNA_rate$log_lik_cauchy <- fit_cauchy_657_rate$loglik


#plot over histogram
ggplot(otter_657_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 671 
#create inidividual otter df for model fitting

#individual otter df
otter_671_rate <- resights_dist_recap %>% subset(seaotter == "671")
otter_671_rate <- as.data.frame(otter_671_rate)
otter_671_noNA_rate <- na.omit(otter_671_rate)


#rayleigh
#fit model
fit_rayleigh_671_rate <- fitdistr(otter_671_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_671_rate)

#gamma
#fit model
fit_gamma_671_rate <- fitdistr(otter_671_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_671_rate)

#weibull
#fit model
fit_weibull_671_rate <- fitdistr(otter_671_noNA_rate$km_hr,"weibull")
str(fit_weibull_671_rate)

#cauchy
#fit model
fit_cauchy_671_rate <- fitdistr(otter_671_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_671_rate)

#estimates from models
otter_671_noNA_rate$rayleigh <- drayleigh(otter_671_noNA_rate$km_hr, scale = fit_rayleigh_671_rate$estimate, log = FALSE)

otter_671_noNA_rate$gamma <- dgamma(otter_671_noNA_rate$km_hr, shape  = fit_gamma_671_rate$estimate[1], rate = fit_gamma_671_rate$estimate[2], log = FALSE)

otter_671_noNA_rate$weibull <- dweibull(otter_671_noNA_rate$km_hr, shape = fit_weibull_671_rate$estimate[1], scale = fit_weibull_671_rate$estimate[2], log = FALSE)

otter_671_noNA_rate$cauchy <- dcauchy(otter_671_noNA_rate$km_hr, location = fit_cauchy_671_rate$estimate[1], scale = fit_cauchy_671_rate$estimate[2], log = FALSE)
#otter_671_noNA$cauchy2 <- dcauchy(otter_671_noNA$dist, location = fit_cauchy_671$estimate[1], scale = fit_cauchy$estimate_671[2], log = FALSE)


#save loglik
otter_671_noNA_rate$log_lik_ray <- fit_rayleigh_671_rate$loglik
otter_671_noNA_rate$log_lik_gamma <- fit_gamma_671_rate$loglik
otter_671_noNA_rate$log_lik_weibull <- fit_weibull_671_rate$loglik
otter_671_noNA_rate$log_lik_cauchy <- fit_cauchy_671_rate$loglik


#plot over histogram
ggplot(otter_671_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  


# 673
#create inidividual otter df for model fitting

#individual otter df
otter_673_rate <- resights_dist_recap %>% subset(seaotter == "673")
otter_673_rate <- as.data.frame(otter_673_rate)
otter_673_noNA_rate <- na.omit(otter_673_rate)


#rayleigh
#fit model
fit_rayleigh_673_rate <- fitdistr(otter_673_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_673_rate)

#gamma
#fit model
fit_gamma_673_rate <- fitdistr(otter_673_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_673_rate)

#weibull
#fit model
fit_weibull_673_rate <- fitdistr(otter_673_noNA_rate$km_hr,"weibull")
str(fit_weibull_673_rate)

#cauchy
#fit model
fit_cauchy_673_rate <- fitdistr(otter_673_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_673_rate)

#estimates from models
otter_673_noNA_rate$rayleigh <- drayleigh(otter_673_noNA_rate$km_hr, scale = fit_rayleigh_673_rate$estimate, log = FALSE)

otter_673_noNA_rate$gamma <- dgamma(otter_673_noNA_rate$km_hr, shape  = fit_gamma_673_rate$estimate[1], rate = fit_gamma_673_rate$estimate[2], log = FALSE)

otter_673_noNA_rate$weibull <- dweibull(otter_673_noNA_rate$km_hr, shape = fit_weibull_673_rate$estimate[1], scale = fit_weibull_673_rate$estimate[2], log = FALSE)

otter_673_noNA_rate$cauchy <- dcauchy(otter_673_noNA_rate$km_hr, location = fit_cauchy_673_rate$estimate[1], scale = fit_cauchy_673_rate$estimate[2], log = FALSE)
#otter_673_noNA$cauchy2 <- dcauchy(otter_673_noNA$dist, location = fit_cauchy_673$estimate[1], scale = fit_cauchy$estimate_673[2], log = FALSE)


#save loglik
otter_673_noNA_rate$log_lik_ray <- fit_rayleigh_673_rate$loglik
otter_673_noNA_rate$log_lik_gamma <- fit_gamma_673_rate$loglik
otter_673_noNA_rate$log_lik_weibull <- fit_weibull_673_rate$loglik
otter_673_noNA_rate$log_lik_cauchy <- fit_cauchy_673_rate$loglik


#plot over histogram
ggplot(otter_673_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 685 
#create inidividual otter df for model fitting

#individual otter df
otter_685_rate <- resights_dist_recap %>% subset(seaotter == "685")
otter_685_rate <- as.data.frame(otter_685_rate)
otter_685_noNA_rate <- na.omit(otter_685_rate)


#rayleigh
#fit model
fit_rayleigh_685_rate <- fitdistr(otter_685_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_685_rate)

#gamma
#fit model
fit_gamma_685_rate <- fitdistr(otter_685_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_685_rate)

#weibull
#fit model
fit_weibull_685_rate <- fitdistr(otter_685_noNA_rate$km_hr,"weibull")
str(fit_weibull_685_rate)

#cauchy
#fit model
fit_cauchy_685_rate <- fitdistr(otter_685_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_685_rate)

#estimates from models
otter_685_noNA_rate$rayleigh <- drayleigh(otter_685_noNA_rate$km_hr, scale = fit_rayleigh_685_rate$estimate, log = FALSE)

otter_685_noNA_rate$gamma <- dgamma(otter_685_noNA_rate$km_hr, shape  = fit_gamma_685_rate$estimate[1], rate = fit_gamma_685_rate$estimate[2], log = FALSE)

otter_685_noNA_rate$weibull <- dweibull(otter_685_noNA_rate$km_hr, shape = fit_weibull_685_rate$estimate[1], scale = fit_weibull_685_rate$estimate[2], log = FALSE)

otter_685_noNA_rate$cauchy <- dcauchy(otter_685_noNA_rate$km_hr, location = fit_cauchy_685_rate$estimate[1], scale = fit_cauchy_685_rate$estimate[2], log = FALSE)
#otter_685_noNA$cauchy2 <- dcauchy(otter_685_noNA$dist, location = fit_cauchy_685$estimate[1], scale = fit_cauchy$estimate_685[2], log = FALSE)


#save loglik
otter_685_noNA_rate$log_lik_ray <- fit_rayleigh_685_rate$loglik
otter_685_noNA_rate$log_lik_gamma <- fit_gamma_685_rate$loglik
otter_685_noNA_rate$log_lik_weibull <- fit_weibull_685_rate$loglik
otter_685_noNA_rate$log_lik_cauchy <- fit_cauchy_685_rate$loglik


#plot over histogram
ggplot(otter_685_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 687 
#create inidividual otter df for model fitting

#individual otter df
otter_687_rate <- resights_dist_recap %>% subset(seaotter == "687")
otter_687_rate <- as.data.frame(otter_687_rate)
otter_687_noNA_rate <- na.omit(otter_687_rate)


#rayleigh
#fit model
fit_rayleigh_687_rate <- fitdistr(otter_687_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_687_rate)

#gamma
#fit model
fit_gamma_687_rate <- fitdistr(otter_687_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_687_rate)

#weibull
#fit model
fit_weibull_687_rate <- fitdistr(otter_687_noNA_rate$km_hr,"weibull")
str(fit_weibull_687_rate)

#cauchy
#fit model
fit_cauchy_687_rate <- fitdistr(otter_687_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_687_rate)

#estimates from models
otter_687_noNA_rate$rayleigh <- drayleigh(otter_687_noNA_rate$km_hr, scale = fit_rayleigh_687_rate$estimate, log = FALSE)

otter_687_noNA_rate$gamma <- dgamma(otter_687_noNA_rate$km_hr, shape  = fit_gamma_687_rate$estimate[1], rate = fit_gamma_687_rate$estimate[2], log = FALSE)

otter_687_noNA_rate$weibull <- dweibull(otter_687_noNA_rate$km_hr, shape = fit_weibull_687_rate$estimate[1], scale = fit_weibull_687_rate$estimate[2], log = FALSE)

otter_687_noNA_rate$cauchy <- dcauchy(otter_687_noNA_rate$km_hr, location = fit_cauchy_687_rate$estimate[1], scale = fit_cauchy_687_rate$estimate[2], log = FALSE)
#otter_687_noNA$cauchy2 <- dcauchy(otter_687_noNA$dist, location = fit_cauchy_687$estimate[1], scale = fit_cauchy$estimate_687[2], log = FALSE)


#save loglik
otter_687_noNA_rate$log_lik_ray <- fit_rayleigh_687_rate$loglik
otter_687_noNA_rate$log_lik_gamma <- fit_gamma_687_rate$loglik
otter_687_noNA_rate$log_lik_weibull <- fit_weibull_687_rate$loglik
otter_687_noNA_rate$log_lik_cauchy <- fit_cauchy_687_rate$loglik


#plot over histogram
ggplot(otter_687_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 716
#create inidividual otter df for model fitting

#individual otter df
otter_716_rate <- resights_dist_recap %>% subset(seaotter == "716")
otter_716_rate <- as.data.frame(otter_716_rate)
otter_716_noNA_rate <- na.omit(otter_716_rate)


#rayleigh
#fit model
fit_rayleigh_716_rate <- fitdistr(otter_716_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_716_rate)

#gamma
#fit model
fit_gamma_716_rate <- fitdistr(otter_716_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_716_rate)

#weibull
#fit model
fit_weibull_716_rate <- fitdistr(otter_716_noNA_rate$km_hr,"weibull")
str(fit_weibull_716_rate)

#cauchy
#fit model
fit_cauchy_716_rate <- fitdistr(otter_716_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_716_rate)

#estimates from models
otter_716_noNA_rate$rayleigh <- drayleigh(otter_716_noNA_rate$km_hr, scale = fit_rayleigh_716_rate$estimate, log = FALSE)

otter_716_noNA_rate$gamma <- dgamma(otter_716_noNA_rate$km_hr, shape  = fit_gamma_716_rate$estimate[1], rate = fit_gamma_716_rate$estimate[2], log = FALSE)

otter_716_noNA_rate$weibull <- dweibull(otter_716_noNA_rate$km_hr, shape = fit_weibull_716_rate$estimate[1], scale = fit_weibull_716_rate$estimate[2], log = FALSE)

otter_716_noNA_rate$cauchy <- dcauchy(otter_716_noNA_rate$km_hr, location = fit_cauchy_716_rate$estimate[1], scale = fit_cauchy_716_rate$estimate[2], log = FALSE)
#otter_716_noNA$cauchy2 <- dcauchy(otter_716_noNA$dist, location = fit_cauchy_716$estimate[1], scale = fit_cauchy$estimate_716[2], log = FALSE)


#save loglik
otter_716_noNA_rate$log_lik_ray <- fit_rayleigh_716_rate$loglik
otter_716_noNA_rate$log_lik_gamma <- fit_gamma_716_rate$loglik
otter_716_noNA_rate$log_lik_weibull <- fit_weibull_716_rate$loglik
otter_716_noNA_rate$log_lik_cauchy <- fit_cauchy_716_rate$loglik


#plot over histogram
ggplot(otter_716_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 723 
#create inidividual otter df for model fitting

#individual otter df
otter_723_rate <- resights_dist_recap %>% subset(seaotter == "723")
otter_723_rate <- as.data.frame(otter_723_rate)
otter_723_noNA_rate <- na.omit(otter_723_rate)


#rayleigh
#fit model
fit_rayleigh_723_rate <- fitdistr(otter_723_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_723_rate)

#gamma
#fit model
fit_gamma_723_rate <- fitdistr(otter_723_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_723_rate)

#weibull
#fit model
fit_weibull_723_rate <- fitdistr(otter_723_noNA_rate$km_hr,"weibull")
str(fit_weibull_723_rate)

#cauchy
#fit model
fit_cauchy_723_rate <- fitdistr(otter_723_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_723_rate)

#estimates from models
otter_723_noNA_rate$rayleigh <- drayleigh(otter_723_noNA_rate$km_hr, scale = fit_rayleigh_723_rate$estimate, log = FALSE)

otter_723_noNA_rate$gamma <- dgamma(otter_723_noNA_rate$km_hr, shape  = fit_gamma_723_rate$estimate[1], rate = fit_gamma_723_rate$estimate[2], log = FALSE)

otter_723_noNA_rate$weibull <- dweibull(otter_723_noNA_rate$km_hr, shape = fit_weibull_723_rate$estimate[1], scale = fit_weibull_723_rate$estimate[2], log = FALSE)

otter_723_noNA_rate$cauchy <- dcauchy(otter_723_noNA_rate$km_hr, location = fit_cauchy_723_rate$estimate[1], scale = fit_cauchy_723_rate$estimate[2], log = FALSE)
#otter_723_noNA$cauchy2 <- dcauchy(otter_723_noNA$dist, location = fit_cauchy_723$estimate[1], scale = fit_cauchy$estimate_723[2], log = FALSE)


#save loglik
otter_723_noNA_rate$log_lik_ray <- fit_rayleigh_723_rate$loglik
otter_723_noNA_rate$log_lik_gamma <- fit_gamma_723_rate$loglik
otter_723_noNA_rate$log_lik_weibull <- fit_weibull_723_rate$loglik
otter_723_noNA_rate$log_lik_cauchy <- fit_cauchy_723_rate$loglik


#plot over histogram
ggplot(otter_723_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 774 
#create inidividual otter df for model fitting

#individual otter df
otter_774_rate <- resights_dist_recap %>% subset(seaotter == "774")
otter_774_rate <- as.data.frame(otter_774_rate)
otter_774_noNA_rate <- na.omit(otter_774_rate)


#rayleigh
#fit model
fit_rayleigh_774_rate <- fitdistr(otter_774_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_774_rate)

#gamma
#fit model
fit_gamma_774_rate <- fitdistr(otter_774_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_774_rate)

#weibull
#fit model
fit_weibull_774_rate <- fitdistr(otter_774_noNA_rate$km_hr,"weibull")
str(fit_weibull_774_rate)

#cauchy
#fit model
fit_cauchy_774_rate <- fitdistr(otter_774_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_774_rate)

#estimates from models
otter_774_noNA_rate$rayleigh <- drayleigh(otter_774_noNA_rate$km_hr, scale = fit_rayleigh_774_rate$estimate, log = FALSE)

otter_774_noNA_rate$gamma <- dgamma(otter_774_noNA_rate$km_hr, shape  = fit_gamma_774_rate$estimate[1], rate = fit_gamma_774_rate$estimate[2], log = FALSE)

otter_774_noNA_rate$weibull <- dweibull(otter_774_noNA_rate$km_hr, shape = fit_weibull_774_rate$estimate[1], scale = fit_weibull_774_rate$estimate[2], log = FALSE)

otter_774_noNA_rate$cauchy <- dcauchy(otter_774_noNA_rate$km_hr, location = fit_cauchy_774_rate$estimate[1], scale = fit_cauchy_774_rate$estimate[2], log = FALSE)
#otter_774_noNA$cauchy2 <- dcauchy(otter_774_noNA$dist, location = fit_cauchy_774$estimate[1], scale = fit_cauchy$estimate_774[2], log = FALSE)


#save loglik
otter_774_noNA_rate$log_lik_ray <- fit_rayleigh_774_rate$loglik
otter_774_noNA_rate$log_lik_gamma <- fit_gamma_774_rate$loglik
otter_774_noNA_rate$log_lik_weibull <- fit_weibull_774_rate$loglik
otter_774_noNA_rate$log_lik_cauchy <- fit_cauchy_774_rate$loglik


#plot over histogram
ggplot(otter_774_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 808
#create inidividual otter df for model fitting

#individual otter df
otter_808_rate <- resights_dist_recap %>% subset(seaotter == "808")
otter_808_rate <- as.data.frame(otter_808_rate)
otter_808_noNA_rate <- na.omit(otter_808_rate)


#rayleigh
#fit model
fit_rayleigh_808_rate <- fitdistr(otter_808_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_808_rate)

#gamma
#fit model
fit_gamma_808_rate <- fitdistr(otter_808_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_808_rate)

#weibull
#fit model
fit_weibull_808_rate <- fitdistr(otter_808_noNA_rate$km_hr,"weibull")
str(fit_weibull_808_rate)

#cauchy
#fit model
fit_cauchy_808_rate <- fitdistr(otter_808_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_808_rate)

#estimates from models
otter_808_noNA_rate$rayleigh <- drayleigh(otter_808_noNA_rate$km_hr, scale = fit_rayleigh_808_rate$estimate, log = FALSE)

otter_808_noNA_rate$gamma <- dgamma(otter_808_noNA_rate$km_hr, shape  = fit_gamma_808_rate$estimate[1], rate = fit_gamma_808_rate$estimate[2], log = FALSE)

otter_808_noNA_rate$weibull <- dweibull(otter_808_noNA_rate$km_hr, shape = fit_weibull_808_rate$estimate[1], scale = fit_weibull_808_rate$estimate[2], log = FALSE)

otter_808_noNA_rate$cauchy <- dcauchy(otter_808_noNA_rate$km_hr, location = fit_cauchy_808_rate$estimate[1], scale = fit_cauchy_808_rate$estimate[2], log = FALSE)
#otter_808_noNA$cauchy2 <- dcauchy(otter_808_noNA$dist, location = fit_cauchy_808$estimate[1], scale = fit_cauchy$estimate_808[2], log = FALSE)


#save loglik
otter_808_noNA_rate$log_lik_ray <- fit_rayleigh_808_rate$loglik
otter_808_noNA_rate$log_lik_gamma <- fit_gamma_808_rate$loglik
otter_808_noNA_rate$log_lik_weibull <- fit_weibull_808_rate$loglik
otter_808_noNA_rate$log_lik_cauchy <- fit_cauchy_808_rate$loglik


#plot over histogram
ggplot(otter_808_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

# 809
#create inidividual otter df for model fitting

#individual otter df
otter_809_rate <- resights_dist_recap %>% subset(seaotter == "809")
otter_809_rate <- as.data.frame(otter_809_rate)
otter_809_noNA_rate <- na.omit(otter_809_rate)


#rayleigh
#fit model
fit_rayleigh_809_rate <- fitdistr(otter_809_noNA_rate$km_hr, drayleigh,start = list(scale = 1))
str(fit_rayleigh_809_rate)

#gamma
#fit model
fit_gamma_809_rate <- fitdistr(otter_809_noNA_rate$km_hr,dgamma,  start = list(shape = 5.388633760, rate = 0.010000000), lower = 0.01)
str(fit_gamma_809_rate)

#weibull
#fit model
fit_weibull_809_rate <- fitdistr(otter_809_noNA_rate$km_hr,"weibull")
str(fit_weibull_809_rate)

#cauchy
#fit model
fit_cauchy_809_rate <- fitdistr(otter_809_noNA_rate$km_hr,"cauchy")
str(fit_cauchy_809_rate)

#estimates from models
otter_809_noNA_rate$rayleigh <- drayleigh(otter_809_noNA_rate$km_hr, scale = fit_rayleigh_809_rate$estimate, log = FALSE)

otter_809_noNA_rate$gamma <- dgamma(otter_809_noNA_rate$km_hr, shape  = fit_gamma_809_rate$estimate[1], rate = fit_gamma_809_rate$estimate[2], log = FALSE)

otter_809_noNA_rate$weibull <- dweibull(otter_809_noNA_rate$km_hr, shape = fit_weibull_809_rate$estimate[1], scale = fit_weibull_809_rate$estimate[2], log = FALSE)

otter_809_noNA_rate$cauchy <- dcauchy(otter_809_noNA_rate$km_hr, location = fit_cauchy_809_rate$estimate[1], scale = fit_cauchy_809_rate$estimate[2], log = FALSE)
#otter_809_noNA$cauchy2 <- dcauchy(otter_809_noNA$dist, location = fit_cauchy_809$estimate[1], scale = fit_cauchy$estimate_809[2], log = FALSE)


#save loglik
otter_809_noNA_rate$log_lik_ray <- fit_rayleigh_809_rate$loglik
otter_809_noNA_rate$log_lik_gamma <- fit_gamma_809_rate$loglik
otter_809_noNA_rate$log_lik_weibull <- fit_weibull_809_rate$loglik
otter_809_noNA_rate$log_lik_cauchy <- fit_cauchy_809_rate$loglik


#plot over histogram
ggplot(otter_809_noNA_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = 0.025)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  themeo  

#rbind together
# all fit but 379, and 673 add in once successfully fit

otter_modelfit_rate <- rbind(otter_209_noNA_rate ,  otter_238_noNA_rate ,  otter_225_noNA_rate ,  otter_228_noNA_rate ,  otter_238_noNA_rate ,  otter_249_noNA_rate ,  otter_252_noNA_rate , 
                             otter_269_noNA_rate ,  otter_286_noNA_rate ,  otter_315_noNA_rate ,  otter_327_noNA_rate ,  otter_339_noNA_rate ,  otter_344_noNA_rate ,  otter_353_noNA_rate , 
                             otter_386_noNA_rate ,  otter_433_noNA_rate ,  otter_451_noNA_rate ,  otter_457_noNA_rate ,  otter_466_noNA_rate ,  otter_473_noNA_rate , 
                             otter_475_noNA_rate ,  otter_501_noNA_rate ,  otter_518_noNA_rate ,  otter_520_noNA_rate ,  otter_526_noNA_rate ,  otter_558_noNA_rate ,  otter_587_noNA_rate , 
                             otter_595_noNA_rate ,  otter_621_noNA_rate ,  otter_623_noNA_rate ,  otter_653_noNA_rate ,  otter_657_noNA_rate ,  otter_671_noNA_rate ,  
                             otter_685_noNA_rate ,  otter_687_noNA_rate ,  otter_716_noNA_rate ,  otter_723_noNA_rate ,  otter_774_noNA_rate ,  otter_808_noNA_rate ,  otter_809_noNA_rate)

write.csv(otter_modelfit_rate, "data_output/otter_model_fit_rate.csv")

ggplot(otter_modelfit_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = .01500)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "red")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "blue")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "purple")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "green")+
  facet_wrap(~seaotter, scales = "free_y")+
  themeo 

# calculate log likelihood
otter_loglik_rate <- otter_modelfit_rate %>%  
  group_by(seaotter) %>% 
  summarise(first(log_lik_ray), 
            first(log_lik_gamma),
            first (log_lik_weibull ),
            first(log_lik_cauchy)) 

head(otter_loglik_rate)
otter_loglik_rate<- rename(otter_loglik_rate, LL_rayleigh = `first(log_lik_ray)`, LL_gamma = `first(log_lik_gamma)`, LL_weibull = `first(log_lik_weibull)`, LL_cauchy = `first(log_lik_cauchy)` )



#calculate AIC

otter_loglik_rate$rayAIC <- ((-2*otter_loglik_rate$LL_rayleigh)+ (2*1))

otter_loglik_rate$gammaAIC <- ((-2*otter_loglik_rate$LL_gamma)+ (2*2))

otter_loglik_rate$weibullAIC <- ((-2*otter_loglik_rate$LL_weibull)+(2*2))

otter_loglik_rate$cauchyAIC <- ((-2*otter_loglik_rate$LL_cauchy)+(2*2))


AIC_table_rate <- otter_loglik_rate %>%  dplyr::select(seaotter, rayAIC, gammaAIC, weibullAIC, cauchyAIC)

AIC_rank_rate <- data.frame(AIC_table_rate, t(apply(AIC_table_rate[,c(2:5)], 1, rank, ties.method='min')))  

AIC_diff_rate <- AIC_rank_rate %>%  mutate(AIC_difference = weibullAIC - (pmin(rayAIC,gammaAIC, cauchyAIC))) 
max(AIC_diff_rate$AIC_difference) #4.461328

write.csv(AIC_diff_rate, "data_output/AIC_diff_rate.csv")

#plot zoomed in
library(scales)
library(lemon)
ggplot(otter_modelfit_rate)+
  geom_histogram(aes(x=km_hr, y = ..density..), binwidth = .1)+
  geom_line(aes(x = km_hr, y = rayleigh),stat = "identity", color = "#b58900")+
  geom_line(aes(x = km_hr, y = gamma),stat = "identity", color = "#cb4b16")+
  geom_line(aes(x = km_hr, y = weibull),stat = "identity", color = "#d33682")+
  geom_line(aes(x = km_hr, y = cauchy),stat = "identity", color = "#6c71c4")+
  # scale_y_log10(expand = c(0,0),
  #              labels = trans_format('log10', math_format(10^.x))) +
  # scale_y_log10()+
  facet_rep_wrap(~seaotter, scales = "free", repeat.tick.labels = FALSE)+
  coord_cartesian(xlim = c(0,3.5), ylim = c(0,10))+
  #xlim(c(0,10))+
  themeo 
