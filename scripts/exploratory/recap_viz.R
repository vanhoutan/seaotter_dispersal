library(dplyr)


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

#testmap_dist2_recap <- readRDS( "data_output/testmap_dist2_recap.rds") # distance calculations (km) from lcp
test_dist_diag_recap <- readRDS("data_output/test_dist_diag_recap.rds")  # lcp distance including diagonal values AKA resights
# otter_resight_dist <- readRDS("data_output/otter_resight_dist.rds") # tibble of seaotter and diagonal values without NA
# test_dist_diag_recap_index <- readRDS("data_output/rtest_dist_diag_recap_index.rds") # includes indexing values for diagonals/resights to use in indexing only resight paths



recap_dist <- test_dist_diag_recap %>% dplyr::select(seaotter, release, diag)
recap_dist <- recap_dist %>% unnest()


recap_dist <- as.data.frame(recap_dist)
recap_dist_nona <- na.omit(recap_dist)

# recap_dist$release <- as.character(recap_dist$release)
# recap_dist$seaotter <- as.character(recap_dist$seaotter)
# recap_dist$diag <- as.numeric(recap_dist$diag)

recap_dist_nona_stats <- recap_dist_nona %>% dplyr::group_by(seaotter, release) %>%
  dplyr::summarise(mean_lcp = mean(diag), med_lcp = median(diag))

recap_dist_nona_n <- recap_dist_nona %>% dplyr::group_by(release) %>%
 summarise(n_distinct(seaotter))

recap_dist_nona %>% dplyr::group_by(seaotter, release) %>% 
  dplyr::summarise(mean_lcp = mean(diag), med_lcp = median(diag)) %>% 
  ggplot()+
  geom_violin(aes(x= release, y = med_lcp, group = release), fill = "lightgrey")+
  theme_themeo()



########################
# refit cauchy models for days out rather than individual otter
# separate recaptures from non-recaptures

# wrangle data
lcp_dist_sum <- recap_dist_nona %>% group_by (seaotter) %>% mutate(max_recap = max(release)) # gnereate max recap column
lcp_dist_sum <- lcp_dist_sum %>% mutate(recap = ifelse(max_recap >1, "Y", "N")) # genereate column for y/n for recaptures
lcp_dist_sum <- lcp_dist_sum %>%  group_by(seaotter) %>% mutate(days_out = 1:n()) %>% ungroup() # column showing number of days in wild (recap releases pick up where previous release left off)

lcp_dist_sum_recap <- lcp_dist_sum %>% dplyr::filter(recap == "Y") # create a df for only animals that were recaptured
lcp_dist_sum_recap_short <- lcp_dist_sum_recap %>% dplyr::select("days_out", "diag") # create a short version with only necessary columns
lcp_dist_sum_Nrecap <- lcp_dist_sum %>% dplyr::filter(recap == "N") # create a df for only animals that were NOT recaptured
lcp_dist_sum_Nrecap_short <- lcp_dist_sum_Nrecap %>% dplyr::select("days_out", "diag") # create a short version with only necessary columns

lcp_dist_g_daysout_recp <- lcp_dist_sum_recap_short %>% group_by(days_out) %>% nest() # group by days out and nest
lcp_dist_g_daysout_Nrecp <- lcp_dist_sum_Nrecap_short %>% group_by(days_out) %>% nest() # group by days out and nest

lcp_dist_g_daysout_recp$data <- flatten(lcp_dist_g_daysout_recp$data) # flatten to create a simple list column rather than tibble column
lcp_dist_g_daysout_Nrecp$data <- flatten(lcp_dist_g_daysout_Nrecp$data) # flatten to create a simple list column rather than tibble column

######## cauchy
##### recaps
# fit for one test day first
test_cauchy_daysout <- fitdistr(lcp_dist_g_daysout_recp$data[[1]],"cauchy")

lcp_dist_g_daysout_recp <- lcp_dist_g_daysout_recp %>% mutate(list_length = lengths(data))
lcp_dist_g_daysout_recp_10 <- lcp_dist_g_daysout_recp %>% dplyr::filter(list_length > 9)
lcp_dist_g_daysout_recp_18 <- lcp_dist_g_daysout_recp %>% dplyr::filter(list_length > 17)

# recap_daysout_models <- lcp_dist_g_daysout_recp_18 %>%  mutate(cauchy = map(data, ~ fitdistr(.x,"cauchy"), lower= c(0.01,0.01) ))
# #recap_daysout_models <- lcp_dist_g_daysout_recp_18 %>%  mutate(cauchy = map(data, ~ fitdistr(.x, dcauchy, list(location = 1, scale =0.1)), lower= c(0.01,0.01)) )
# recap_daysout_models$cauchy[1] # test this ran correctly
# recap_daysout_model13 <- fitdistr(lcp_dist_g_daysout_recp$data[[13]], dcauchy, list(location = 1, scale =0.1), lower= c(0.01,0.01))

# recap_daysout_models<- lcp_dist_g_daysout_recp_18 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale =0.1), lower= c(0.01,0.01)) ))
# recap_daysout_models$cauchy[1]
# 
# 
# cauchy_daysout13 <- fitdistr(lcp_dist_g_daysout_recp$data[[13]],dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01))
# 
# lcp_dist_g_daysout_recp_10_15 <- lcp_dist_g_daysout_recp_10 %>% dplyr::filter(list_length < 17)
# cauchy_daysout14 <- fitdistr(lcp_dist_g_daysout_recp$data[[14]],dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01))
# 
# recap_daysout_models_10_15<- lcp_dist_g_daysout_recp_10_15 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01)) ))
# recap_daysout_models_10_15$cauchy[2]

recap_daysout_models <- lcp_dist_g_daysout_recp_10 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01)) ))
recap_daysout_models$cauchy

##### add estimates and parameters
recap_daysout_models_est_par <- recap_daysout_models %>%  
  mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
         scale = map(cauchy, ~.x[1][[1]][[2]]),
         est_cauchy = map2(data, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))

plot(recap_daysout_models_est_par$days_out, recap_daysout_models_est_par$scale)

cauchy_daysout13 <- fitdistr(lcp_dist_g_daysout_recp$data[[13]],dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01))

lcp_dist_g_daysout_recp_10_15 <- lcp_dist_g_daysout_recp_10 %>% dplyr::filter(list_length < 17)
cauchy_daysout14 <- fitdistr(lcp_dist_g_daysout_recp$data[[14]],dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01))

recap_daysout_models_10_15<- lcp_dist_g_daysout_recp_10_15 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale = 1), lower= c(0.01,0.01)) ))
recap_daysout_models_10_15$cauchy[2]

######## cauchy
##### no recaps
# fit for one test day first
test_cauchy_daysout_N <- fitdistr(lcp_dist_g_daysout_Nrecp$data[[1]],"cauchy")

lcp_dist_g_daysout_Nrecp <- lcp_dist_g_daysout_Nrecp %>% mutate(list_length = lengths(data))
lcp_dist_g_daysout_Nrecp_10 <- lcp_dist_g_daysout_Nrecp %>% dplyr::filter(list_length > 9)
lcp_dist_g_daysout_Nrecp_18 <- lcp_dist_g_daysout_Nrecp %>% dplyr::filter(list_length > 17)
lcp_dist_g_daysout_Nrecp_5 <- lcp_dist_g_daysout_Nrecp %>% dplyr::filter(list_length > 5)

# recap_daysout_models <- lcp_dist_g_daysout_recp_18 %>%  mutate(cauchy = map(data, ~ fitdistr(.x,"cauchy"), lower= c(0.01,0.01) ))
# #recap_daysout_models <- lcp_dist_g_daysout_recp_18 %>%  mutate(cauchy = map(data, ~ fitdistr(.x, dcauchy, list(location = 1, scale =0.1)), lower= c(0.01,0.01)) )
# recap_daysout_models$cauchy[1] # test this ran correctly
# recap_daysout_model13 <- fitdistr(lcp_dist_g_daysout_recp$data[[13]], dcauchy, list(location = 1, scale =0.1), lower= c(0.01,0.01))

recap_daysout_modelsN<- lcp_dist_g_daysout_Nrecp_5 %>% mutate(cauchy = map(data, ~ fitdistr(.x,dcauchy, list(location = 1, scale =0.1), lower= c(0.01,0.01)) ))
recap_daysout_modelsN$cauchy

##### add estimates and parameters
recap_daysout_models_est_parN <- recap_daysout_modelsN %>%  
  mutate(location = map(cauchy, ~.x[1][[1]][[1]]),
         scale = map(cauchy, ~.x[1][[1]][[2]]),
         est_cauchy = map2(data, cauchy, ~dcauchy(.x, location = .y[1][[1]][[1]], scale = .y[1][[1]][[2]], log = FALSE)))

plot(recap_daysout_models_est_parN$days_out, recap_daysout_models_est_parN$scale)


# prep data for plotting with ggplot
cauchy_N_recap_params <- recap_daysout_models_est_parN %>% dplyr::select(days_out, location, scale) %>% unnest()
cauchy_N_recap_params$recap <- "N"

cauchy_recap_params <- recap_daysout_models_est_par %>% dplyr::select(days_out, location, scale) %>% unnest()
cauchy_recap_params$recap <- "Y"

cauchy_est <- rbind(cauchy_N_recap_params, cauchy_recap_params)

ggplot()+
  geom_smooth(data = cauchy_est, aes(x = days_out, y = scale, color = recap))+
  geom_point(data = cauchy_est, aes(x = days_out, y = scale, color = recap))+
  coord_cartesian(expand = 0, ylim = c(0,2))+
 facet_wrap(~recap, ncol= 1)+
  themeo
