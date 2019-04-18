# nycflights13 패키지에는 2013년 뉴욕의 항공 운항 정보를 담고 있는 flights 데이터가 들어있습니다. flights 데이터가 몇 개의 운항 정보로 구성되는지, 어떤 변수들로 구성되는지 알아본 다음 문제를 해결하세요.

#2013/1/5 출발하는 인천 - 뉴욕행 비행기중 출발 및 도착 delay time의 합의 평균이 가장 적고, 비행시간이 가장 짧은  비행기는?

install.packages("nycflights13")
library(nycflights13)
df_flights_raw = data.frame(nycflights13::flights)
df_flights_copy = df_flights_raw


head(df_copy)

v_year = 2013
v_month = 1
v_day = 5
v_origin = "ICN"
v_dest = "NYC"



df_flight <- df_copy %>% filter(year == v_year & month == v_month & day == v_day & origin == v_origin & dest == v_dest )

str(df_flight)

min(df_copy$year)

## Q1. dest는 도착하는 공항(목적지), distance는 목적지까지의 이동 거리를 나타낸 변수입니다. 목적지별 평균 이동 거리를 나타낸 표를 출력하세요.
df_flights_copy %>% group_by(dest) %>% summarise(distance_mean = mean(distance))





## Q2. 평균 이동 거리가 가장 긴 목적지가 어디인지 알아보세요.

df_flights_copy %>% 
  group_by(dest) %>% 
  summarise(distance_mean = mean(distance)) %>% 
  arrange(desc(distance_mean)) %>% 
  head(1)


# dest 변수의 값은 코드(FAA airport code)로 되어있기 때문에 구체적으로 어떤 공항을 의미하는지 알기 어렵습니다. nycflights13 패키지에 들어있는 airports 데이터는 공항 코드(faa)별 공항명(name)을 담고 있습니다. 

## Q3. 앞 문제에서 알아낸 공항 코드 HNL이 어떤 공항을 의미하는지, airports 데이터를 이용해 알아보세요. dplyr 패키지의 함수를 이용하세요.

df_airports_raw = data.frame(nycflights13::airports)
df_airports_copy = df_airports_raw

df_airports_copy %>% filter(faa == 'HNL')


## Q4. carrier 변수는 항공사 코드(carrier codes)를 담고 있습니다. 어떤 항공사의 운항이 많았는지 알아보려고 합니다. 항공사별 '운항 빈도'를 나타낸 표를 출력하세요.

df_flights_copy %>% 
  group_by(carrier) %>% 
  summarise(n=n())




## Q5. 운항 빈도가 가장 높은 항공사 상위 3곳을 알아보세요.

df_flights_copy %>% 
  group_by(carrier) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(3)





## Q6. 어떤 항공사의 '점유율'이 높았는지 알아보려고 합니다. 항공사별 점유율을 나타낸 표를 출력하세요(힌트 : 점유율 = 전체 빈도에서 해당 구성 요소의 빈도가 차지하는 비율).


v_total <- summarise(df_flights_copy,n=n())
v_total

df_q6 <- df_flights_copy %>% 
  group_by(carrier) %>% 
  summarise(carrier_count=n()) 
df_q6 %>% mutate( p = carrier_count / v_total$n * 100)

df_flights_copy %>% 
  group_by(carrier) %>% 
  summarise(carrier_count=n(), p = carrier_count / summarise(df_flights_copy,n=n())$n*100) 

# Best Code ####
df_flights_copy %>% 
  group_by(carrier) %>% 
  summarise(carrier_count = n()) %>% 
  mutate(ratio = carrier_count / sum(carrier_count)*100)

#or
df_flights_copy %>% 
  count(carrier, sort=T) %>% 
  mutate(ratio = n / sum(n)*100)

mpg_copy %>% 
  group_by(drv) %>% 
  summarise(n = n()) %>% 
  mutate(ratio = n/sum(n)*100) %>% 
  arrange(desc(ratio))
   
mpg_copy %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  mutate(ratio = n/sum(n)*100) %>% 
  arrange(desc(ratio)) %>% 
  head(5)

mpg_copy %>% 
  group_by(manufacturer, class) %>% 
  summarise(n = n()) %>% 
  mutate(ratio = n/sum(n)*100) %>% 
  arrange((manufacturer)) %>% 

mpg_copy %>% 
  group_by(manufacturer, class) %>% 
  summarise(n = n()) %>% 
  mutate(ratio = n/sum(n)*100) %>% 
  filter(class == "suv") %>% 
  arrange(ratio) %>% 
  head(3)
  




## Q7. 점유율이 가장 높은 항공사 5곳이 어디인지 알아보세요.
df_q7 <- 
df_q6 %>% 
  mutate( p = carrier_count / v_total$n * 100) %>% 
  arrange(desc(p)) %>% 
  head(5)






# carrier 변수의 값은 코드로 되어있기 때문에 구체적으로 어떤 항공사를 의미하는지 알기 어렵습니다. nycflights13 패키지의 airlines 데이터는 항공사 코드(carrier)별 항공사 이름(name)을 담고 있습니다. 

## Q8. 앞 문제를 통해 알아낸 점유율 1위 항공사 코드인 UA가 어떤 항공사를 의미하는지, airlines 데이터를 이용해 알아보세요. dplyr 패키지 함수를 이용하세요.

df_airlines_raw = data.frame(nycflights13::airlines)
df_airlines_copy = df_airlines_raw

str(df_airlines_copy)

left_join(df_q7, df_airlines_copy, by=c("carrier","carrier"))


