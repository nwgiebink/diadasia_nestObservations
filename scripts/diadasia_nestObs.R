#Diadasia Nest Observations
#Describe nest activity in D. rinconis aggregation
#Noah Giebink
#giebink@email.arizona.edu
#2019-07-21

#Packages
library(tidyverse)

#Data

nestObs_raw <- read.csv("data/diadasia_nest_observations2.csv")

#clean data and add variable for gaurded 
nestObs_clean <- select(nestObs_raw, -notes) %>%
  mutate(nest_type = ifelse(guard > 0, "guarded", "unguarded"))



#Data tidying: make long, add behavior_duration for guarding
nestObs_clean_tidy <- gather(nestObs_clean, behavior, count,
       -nest, -observation_seconds, -start_time, -day,
       -month, -year, -nest_type) %>%
  uncount(count) %>%
  arrange((nest), desc(nest)) %>%
  mutate(behavior_duration = 
           ifelse(behavior == "guard", "", 0)) %>%
  select(nest, nest_type, behavior, behavior_duration, 
         year, month, day, start_time, observation_seconds)

#write nestObs_clean_tidy as .csv
write_csv(nestObs_clean_tidy, path = "./data/diadasia_nestObs_tidy.csv", col_names = TRUE)


