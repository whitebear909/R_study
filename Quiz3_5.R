install.packages("nycflights13")

library(nycflights13)

df_raw = as.data.frame(nycflights13::flights)
df_copy = df_raw

?flights
head(df_copy)

summary(df_copy)


df_airport_raw = as.data.frame(nycflights13::airports)
df_airport_copy = df_airport_raw

head(df_airport_copy)
?airports

df_airport_copy %>% filter(faa == "ICN")
Airport에서 출발하는  항공편들의 평균 출발 지연 시간을 구하시오.
head(df_copy)

v_orgin_name = "Lansdowne Airport"

v_orgin = "JFK"

df_origin  <-  df_airport_copy %>% filter(faa==v_orgin)
#df_origin  <-  df_airport_copy %>% filter(name==v_orgin_name)

df_origin$name


str(df_copy)



df_copy$delay_yn <- ifelse(df_copy$dep_delay > 0 | df_copy$arr_delay > 0, "T", "F")

str(df_copy)

qplot(df_copy$delay_yn)

table(df_temp$dep_time)


head(df_temp)
str(df_temp)
summary(df_temp)

mean(df_temp$dep_delay,na.rm = T)



hist(df_temp$dep_delay)

table(df_temp$origin)




v_year = 2013
v_month = 1
v_day = 5
v_origin = "ICN"
v_dest = "NYC"



df_flight <- df_copy %>% filter(year == v_year & month == v_month & day == v_day & origin == v_origin & dest == v_dest )

str(df_flight)

min(df_copy$year)




