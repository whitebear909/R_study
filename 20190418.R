
df_midwest_raw <- as.data.frame(ggplot2::midwest)
df_midwest_copy <- df_midwest_raw

df_midwest_copy <- mutate(df_midwest_copy, nonadults_ratio = (poptotal-popadults) / poptotal * 100)
head(df_midwest_copy,5)

df_midwest_copy %>% 
  arrange(desc(nonadults_ratio)) %>% 
  head(5) %>% 
  select (county, state, nonadults_ratio)

df_midwest_copy %>% 
  mutate(nonadult_grade = ifelse(nonadults_ratio < 30, "small", ifelse(nonadults_ratio < 40, "middle", "large")))

mutate(df_midwest_copy, popasian_ratio = popasian / poptotal * 100) %>% 
  arrange(popasian_ratio) %>% 
  head(10) %>% 
  select(state, county, popasian_ratio)

df_midwest_copy %>% 
  group_by(county) %>% 
  summarise(county_n = n()) %>% 
  filter(county_n>1)


df_midwest_copy <- mutate(df_midwest_copy,state_county = paste(df_midwest_copy$state, df_midwest_copy$county,sep="_" ))
mutate(df_midwest_copy,nonadults_ratio = (poptotal-popadults) / poptotal * 100) %>% 
  arrange(desc(nonadults_ratio)) %>% 
  head(5) %>% 
  select(state_county, nonadults_ratio)




df_dup_county <- group_by(df_midwest_copy, county) %>% summarise(county_n = n()) %>% 
  filter(county_n>1)

mutate(df_midwest_copy,nonadults_ratio = (poptotal-popadults) / poptotal * 100) %>% 
  arrange(desc(nonadults_ratio)) %>%
  filter(county %in% df_dup_county$county) %>% 
  head(5) %>% 
  select(state_county, nonadults_ratio)


df_flights_copy <- mutate(df_flights_copy, lane = paste(df_flights_copy$origin, df_flights_copy$desc, sep="_"))
df_flights_copy %>% group_by(lane) %>% summarise(mean_distance = mean(distance)) %>% filter(mean_distance < 1300)

group_by(manufacture, as.factor(class), .drop = F)
                          
       str(df_flights_copy)       
       
?paste


