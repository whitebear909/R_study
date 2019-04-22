#library loading ####
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(readxl)

df_flights_copy <- mutate(df_flights_copy, lane = paste(df_flights_copy$origin, df_flights_copy$dest, sep="_"))

df_flights_copy %>% 
  group_by(lane) %>% 
  summarise(mean_distance = mean(distance), n = n()) %>% 
  filter(mean_distance <= 1300) %>% 
  summarise(n=n())

df_flights_copy %>% 
  group_by(lane) %>% 
  summarise(mean_distance = mean(distance), n = n(), ratio_1300 = ifelse(mean_distance <= 1300, "1300blow", "1300more")) %>%
  group_by(ratio_1300) %>% 
  summarise(n=n()) %>% 
  mutate(total_ratio = n/sum(n)*100)
  
  
  
