dd_EHS <- read.csv(here::here("data_output/dispersal_drivers_EHS.csv"))
dd <- read.csv(here::here("data_output/dispersal_drivers.csv"))


ggplot(dd_EHS)+
  geom_violin(aes(x = sex, y = scale_loess_pred), fill = "lightgrey")+
  themeo


ggplot(dd_EHS)+
  geom_violin(aes(x = sex, y = scale), fill = "lightgrey")+
  themeo


sum(dd$days_in)
