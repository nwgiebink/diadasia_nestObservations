#Diadasia Nest Observations
#Describe nest activity in D. rinconis aggregation
#Noah Giebink
#giebink@email.arizona.edu
#2019-07-21

#Packages
library(tidyverse)

#Data

nestObs_raw <- read.csv("data/diadasia_nest_observations2_edit.csv")

#clean raw data and add variable for gaurded 
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

#Tidy data without behavior_duration
nestObs_clean_tidy2 <- gather(nestObs_clean, behavior, count,
                             -nest, -observation_seconds, -start_time, -day,
                             -month, -year, -nest_type) %>%
  arrange((nest), desc(nest)) %>%
  select(nest, nest_type, behavior, count, year, month, day, 
         start_time, observation_seconds)

#Write tidy csv for data without behavior duration
write_csv(nestObs_clean_tidy2, path = "./data/diadasia_nestObs_final.csv", col_names = TRUE)


#Data with Guard durations
guard_data <- read.csv("data/diadasia_nestObs_tidy_guardDur.csv")
guard_data <- filter(guard_data, nest_type == "guarded",
                     behavior == "guard") %>%
  select(nest, guard_duration = behavior_duration, year,
         month, day, start_time, observation_seconds)

#Write guard data csv
write_csv(guard_data, path = "./data/diadasia_guardDurations.csv", col_names = TRUE)



#Visualizations

#count of each behavior by nest_type
ggplot(nestObs_clean_tidy2, aes(x = nest_type, y = count, fill = behavior)) +
  geom_boxplot() 

#number of approaches by time of day
approaches <- filter(nestObs_clean_tidy2, behavior == "approach") %>% 
  select(nest, nest_type, behavior, count_approaches = count, year,
         month, day, start_time, observation_seconds)
ggplot(approaches, aes(x = start_time, y = count_approaches)) +
  geom_point() +
  geom_smooth(method = "lm")

#Guarding duration by time of day
ggplot(guard_data, aes(x = start_time, y = guard_duration)) +
  geom_jitter() +
  geom_smooth(method = "lm")

#all behavior by time of day
ggplot(nestObs_clean_tidy2, aes(x = start_time, y = count, color = behavior)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = behavior), se = FALSE)

