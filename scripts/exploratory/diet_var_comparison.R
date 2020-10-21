
##### read in packages
library(here)
library(tidyverse)

library(sf)
library(rgdal)
library(raster)

library(forcats)
library(lemon)
library(lubridate)

library(gridExtra)


dd <- read.csv(here::here("data_output/dispersal_drivers.csv"))
rel_met <- read.csv(here::here("data_output/release_metrics_51119.csv"))

str(dd)
str(rel_met)


new_diet_var <- rel_met %>% dplyr::select(seaotter, percentlive, percentlive_mean)


dd_alldiet <- left_join(dd, new_diet_var, by = "seaotter")
dd_alldiet$seaotter <- as.factor(dd_alldiet$seaotter)


diet_sum_plot <- ggplot(dd_alldiet) +
  geom_point(aes(x=fct_rev(seaotter), y = percentlive.x, color = sex), size =2, show.legend = FALSE)+
  coord_flip()+
  xlab("")+
  ylab("% total monthly live diet")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo


diet_mean_plot <- ggplot(dd_alldiet) +
  geom_point(aes(x=fct_rev(seaotter), y = percentlive_mean, color = sex), size =2, show.legend = FALSE)+
  coord_flip()+
  xlab("")+
  ylab("% mean daily live diet")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo


grid.arrange(diet_sum_plot, diet_mean_plot,
             layout_matrix = rbind(c(1,2)))


diet_scale_plot_sum <- ggplot(dd_alldiet)+
  geom_point(aes(x = percentlive.x, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = percentlive.x, y = scale_loess_pred)) +
  xlab("% total monthly live diet")+
  ylab("scale (smoothed)")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo


diet_scale_plot_mean <- ggplot(dd_alldiet)+
  geom_point(aes(x = percentlive_mean, y = scale_loess_pred, color = sex), show.legend = FALSE)+
  geom_smooth(aes(x = percentlive_mean, y = scale_loess_pred)) +
  xlab("% mean daily live diet")+
  ylab("scale (smoothed)")+
  scale_color_manual(values = c("#3d60c0", "#60003a"))+
  themeo


grid.arrange(diet_sum_plot, diet_mean_plot, diet_scale_plot_sum, diet_scale_plot_mean,
             layout_matrix = rbind(c(3,3,3,1),
                                   c(4,4,4,2)))
