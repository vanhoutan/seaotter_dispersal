dd <- dispersal_drivers_fillna %>%  select("seaotter"  , "scale" ,  "sex"  ,                    
                                            "relyear"  ,  "relmonth" ,  "relsiteATOS"  , "relage_d"      ,           
                                            "strandage_d" , "strandsiteATOS" ,"rel_date_real" ,  "mean_daily_ehs_sst"    ,
                                            "condition" , "days_in" , "relyear_first" , "surrogate"   ,             
                                            "max_relnum" , "sst_loess_pred" ,"pdo_loess_pred"  ,                
                                            "enso_loess_pred" , "date"  ,  "mei_rank" , "pdo_index", "percentlive.y")
dd_ehs <- dispersal_drivers_EHS_fillna %>%  select("seaotter"  , "scale" , "scale_loess_pred",  "sex"  ,                    
                                           "relyear"  ,  "relmonth" ,  "relsiteATOS"  , "relage_d"      ,           
                                           "strandage_d" , "strandsiteATOS" ,"rel_date_real" ,  "mean_daily_ehs_sst"    ,
                                           "condition" , "days_in" , "relyear_first" , "surrogate"   ,             
                                           "max_relnum" , "sst_loess_pred" ,"pdo_loess_pred"  ,                
                                           "enso_loess_pred" , "date"  ,  "mei_rank" , "pdo_index", "percentlive.y", "dens.sm",
                                           "X5yrtrend")

colnames(dispersal_drivers_fillna)
 write.csv(dd, "data_output/otter_disperal_drivers.csv")

 ### visualize relationships 
dd_num <- select_if(dd, is.numeric)
dd_ehs_num <- select_if(dd_ehs, is.numeric) 
 
pairs(dd_ehs_num)
 
dd_ehs %>%
  gather(-scale, -sex, -surrogate, -max_relnum, - relsiteATOS, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = scale, color = sex)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
 
dd_ehs %>%
  gather(-scale_loess_pred, -sex, -surrogate,-max_relnum, - relsiteATOS, -date, -rel_date_real, - relyear_first, 
         -relyear, -seaotter, -scale, key = "var", value = "value") %>% 
  arrange() %>% 
  ggplot(aes(x = value, y = scale_loess_pred, color = sex)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()


cond_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = condition, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

days_in_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = days_in, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

dens.sm_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = dens.sm, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

enso_loess_pred_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = enso_loess_pred, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

mean_daily_ehs_sst_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = mean_daily_ehs_sst, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

mei_rank_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = mei_rank, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

pdo_index_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = pdo_index, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

pdo_loess_pred_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = pdo_loess_pred, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

percentlive.y_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = percentlive.y, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

relage_d_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = relage_d, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

relmonth_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = relmonth, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

sst_loess_pred_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = sst_loess_pred, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

strandage_d_ls  <- ggplot(dd_ehs)+
  geom_point(aes(x = strandage_d, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

strandsiteATOS_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = strandsiteATOS, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

X5yrtrend_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = X5yrtrend, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

surrogate_ls <- ggplot(dd_ehs)+
  geom_point(aes(x = surrogate, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  themeo

library(gridExtra)


grid.arrange(cond_ls, days_in_ls, dens.sm_ls, enso_loess_pred_ls, mean_daily_ehs_sst_ls,
             mei_rank_ls, pdo_index_ls, pdo_loess_pred_ls, percentlive.y_ls, relage_d_ls,
             relmonth_ls, sst_loess_pred_ls, strandage_d_ls, strandsiteATOS_ls, X5yrtrend_ls, surrogate_ls, 
            layout_matrix =  rbind(c(1,2,3,4),
             c(5,6,7,8),
             c(9,10,11,12),
             c(13,14,15,16)))

#### modelling
 lm_mod1 <- lm(scale ~ sex + relage_d +  mean_daily_ehs_sst + condition + 
                  enso_loess_pred + percentlive.y, data = dd)
 
 summary(lm_mod1)
 resid1 <- resid(lm_mod1)
 
 plot(density(resid(lm_mod1)))
 
 pred1 <- predict(lm_mod1)

plot(dd$scale, lm_mod1$fitted.values)
plot(dd$scale, lm_mod1$residuals)

 rsq1 <- summary(lm_mod1)$r.squared
 
 ### glm on all otters, raw scale
 glm_gamma_mod1 <- glm(formula = scale ~ sex + relage_d +  mean_daily_ehs_sst + condition + 
                 enso_loess_pred + percentlive.y, 
                 family = Gamma(link = "log"),
                 data = dd)    
summary(glm_gamma_mod1)
resid_glm1 <- resid(glm_gamma_mod1)

plot(density(resid(glm_gamma_mod1)))

pred_glm1 <- predict(glm_gamma_mod1)


Rsq_glm1 <- 1- (glm_gamma_mod1$deviance/ glm_gamma_mod1$null.deviance)

plot(glm_gamma_mod1$y, glm_gamma_mod1$fitted.values)
plot(glm_gamma_mod1$y, glm_gamma_mod1$residuals)

### glm on ehs otters, raw scale
glm_gamma_mod2 <- glm(formula = scale ~ sex + relage_d +  mean_daily_ehs_sst + condition +
                        enso_loess_pred + percentlive.y, 
                      family = Gamma(link = "log"),
                      data = dd_ehs)    
summary(glm_gamma_mod2)
resid_glm2 <- resid(glm_gamma_mod2)

plot(density(resid(glm_gamma_mod2)))

pred_glm2 <- predict(glm_gamma_mod2)


Rsq_glm2 <- 1- (glm_gamma_mod2$deviance/ glm_gamma_mod2$null.deviance)

plot(glm_gamma_mod2$y, glm_gamma_mod2$fitted.values)
plot(glm_gamma_mod2$y, glm_gamma_mod2$residuals)

### glm on ehs otters, loess scale
glm_gamma_mod3 <- glm(formula = scale_loess_pred ~ sex + relage_d +  mean_daily_ehs_sst + condition + 
                        enso_loess_pred + percentlive.y, 
                      family = Gamma(link = "log"),
                      data = dd_ehs)    
summary(glm_gamma_mod3)
resid_glm3 <- resid(glm_gamma_mod3)

plot(density(resid(glm_gamma_mod3)))

pred_glm3 <- predict(glm_gamma_mod3)


Rsq_glm3 <- 1- (glm_gamma_mod3$deviance/ glm_gamma_mod3$null.deviance)

plot(glm_gamma_mod3$y, glm_gamma_mod3$fitted.values)
plot(glm_gamma_mod3$y, glm_gamma_mod3$residuals)


### glm on ehs otters, loess scale
glm_gamma_mod4 <- glm(formula = scale_loess_pred ~ sex + relage_d +  dens.sm + condition + 
                        enso_loess_pred + percentlive.y, 
                      family = Gamma(link = "log"),
                      data = dd_ehs)    
summary(glm_gamma_mod4)
resid_glm4 <- resid(glm_gamma_mod4)

plot(density(resid(glm_gamma_mod4)))

pred_glm4 <- predict(glm_gamma_mod4)


Rsq_glm4 <- 1- (glm_gamma_mod4$deviance/ glm_gamma_mod4$null.deviance)

plot(glm_gamma_mod4$y, glm_gamma_mod4$fitted.values)
plot(glm_gamma_mod4$y, glm_gamma_mod4$residuals)

