library(tidyverse)
library(here)

otter_resight_dist <- readRDS("data_output/otter_resight_dist.rds") # tibble of seaotter and diagonal values without NA

otter_resight_dist_dist <- otter_resight_dist %>%  dplyr::select(seaotter, dist4)
otter_resight_dist_dist <- otter_resight_dist_dist %>% unnest()


total_dist <- otter_resight_dist_dist %>% group_by(seaotter) %>% summarise(sum_dist = sum(dist4))


post_rel_resights <- read.csv(here::here("data/post_release_resights.csv"))

post_rel_resights_short <- post_rel_resights %>% dplyr::select(seaotter, release, date, time, latitude, longitude)

post_rel_resights_short$date_rel <- lubridate::mdy(post_rel_resights_short$date)


post_rel_resights_shortest <- post_rel_resights_short %>% group_by(seaotter) %>% summarise(days_tracked = n())

sum(post_rel_resights_shortest$days_tracked)


# calculate length of first release
# post_rel_resights_short

# create max_recap column
post_rel_resights_short <- post_rel_resights_short %>% group_by(seaotter) %>% mutate(max_recap = max(release))

# subset just for max recap >1
post_rel_resights_short_recaps <- post_rel_resights_short %>% dplyr::filter(max_recap >1)

# create a first release vs recap_date variable
first_release_length <- post_rel_resights_short_recaps %>% dplyr::filter(release == 1)

first_release_length <- first_release_length %>% group_by(seaotter) %>% summarise(release_date = first(date), recap_date = last(date))

#make dates real dates
first_release_length$release_date <- mdy(first_release_length$release_date)
first_release_length$recap_date <- mdy(first_release_length$recap_date)

# calculate # days between first release and second release
first_release_length <- first_release_length %>% mutate(first_rel_length = recap_date - release_date)

# take average of #days in first release
mean(first_release_length$first_rel_length) # 3.388889 days
median(first_release_length$first_rel_length) # 3 days
range(first_release_length$first_rel_length) # 1- 10 days
sd(first_release_length$first_rel_length) # 2.278774 days
