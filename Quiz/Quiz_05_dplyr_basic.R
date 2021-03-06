
# 1-1. Part1 ~ Part2의 문제를 해결하는데 필요한 패키지를 '전부' 로드하세요.
library(dplyr)
library(ggplot2)




# 1-2. data.frame()과 c()를 중첩하여 아래와 같은 형태의 데이터 프레임을 만들어 출력하세요.

# name   math  history
# *********************
# kim    73    62
# lee    82    86
# park   61    94

df_q1  <-  as.data.frame(name = c("kim", "lee", "park"),
                      math = c(73,82,61),
                      history = c(62,86,94))





# 1-3. 앞에서 만든 데이터 프레임을 이용해 세 사람의 평균 수학 점수(math)를 담은 변수를 만들어 출력하세요.




# 1-4. 첨부한 raw_score.xlsx 파일을 R에서 분석할 수 있도록 불러들인 후 일부를 출력하여 잘 불러들여졌는지 확인하세요. science 평균 점수를 출력하세요.





# 1-5. english에서 1등과 꼴등의 점수를 출력하세요.





## ggplot2 패키지에는 미국 동북중부 437개 지역(county)의 인구통계 정보를 담은 midwest 데이터가 들어 있습니다. midwest 데이터를 사용해 문제를 해결하세요.

# 2-1. midwest 복사본을 만든 후 midwest 데이터가 몇 개의 row와 몇 개의 변수로 구성되어 있는지, 어떤 특징을 지닌 변수들로 구성되어 있는지 알아보세요. 하나의 함수만 이용하세요.

df_midwest_raw  <-  as.data.frame(ggplot2::midwest)
df_midwest_copy <- df_midwest_raw
str(df_midwest_copy)


# 2-2. popwhite는 백인 인구, popamerindian은 미국 원주민(American Indians) 인구를 나타낸 변수입니다. popwhite는 white로, popamerindian은 indian으로 변수명을 수정하세요.

df_midwest_copy <- rename(df_midwest_copy, white = popwhite, indian = popamerindian)
str(df_midwest_copy)



# 2-3. dplyr 패키지의 함수를 이용해 '백인 대비 미국 원주민 인구 백분율' 파생변수를 만드세요.

df_midwest_copy <- mutate(df_midwest_copy, ratio_white_indian = indian / white *100)
str(df_midwest_copy)


# 2-4. '백인 대비 미국 원주민 인구 백분율'이 전체 평균 이상이면 "large", 그 외에는 "small"을 부여하는 파생변수를 만드세요.

df_midwest_copy <- 
  mutate(df_midwest_copy, ratio_mean_type = ifelse( ratio_white_indian >= mean(df_midwest_copy$ratio_white_indian),"large", "small"))

mean(df_midwest_copy$ratio_white_indian)

str(df_midwest_copy)



# 2-5. "large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도표와 빈도 막대 그래프를 만들어 확인해 보세요.

df_midwest_copy %>% group_by(ratio_mean_type) %>% summarise(n_count=n())
qplot(df_midwest_copy$ratio_mean_type)

df_ratio_mean_type

qplot(df_copy$delay_yn)

?qplot



# 2-6. '백인 대비 미국 원주민 인구 백분율'이 가장 낮은 20개 지역을 추출해서 별도의 데이터를 만드세요.


df_low_region <- arrange(df_midwest_copy, ratio_white_indian) %>% head(20) 



# 2-7. 앞에서 추출한 지역 중 대학 진학자 비율(percollege)이 가장 낮은 5개 지역(county)이 어디인지 알아보세요. 출력 결과에는 지역 이름과 대학 진학자 비율만 나타나게 하세요.

arrange(df_low_region, percollege) %>% head(5) %>% select(county, percollege)






## nycflights13 패키지에는 2013년 뉴욕의 항공 운항 정보를 담고 있는 flights 데이터가 들어있습니다. nycflights13 패키지의 flights 데이터를 이용해 문제를 해결하세요.

# 3-1. hour 변수는 항공편이 몇 시에 출발했는지, 출발 시각을 담고 있습니다. 항공편이 오전(AM)과 오후(PM)에 각각 몇 편 출발했는지 나타낸 표를 출력하세요.

df_nflights_raw <- as.data.frame(nycflights13::flights)
df_nflights_copy <- df_nflights_raw 

qplot(df_nflights_copy$hour)

df_nflights_copy <- mutate(df_nflights_copy, AM_PM = ifelse(df_nflights_copy$hour>=12,"PM","AM"))

table(df_nflights_copy$AM_PM)

# 3-2. 오전과 오후의 항공편 출발 백분율을 나타낸 표를 출력하세요.

df_nflights_copy %>% 
  group_by(AM_PM) %>% 
  summarise(n=n()) %>% 
  mutate(ratio = n/sum(n)*100) 





# 3-3. 오전에 출발하는 항공편이 어떤 공항으로 많이 가는지 알아보려고 합니다. 오전 출발 항공편의 목적지별 빈도와 점유율을 나타낸 표를 출력하세요. 표에는 상위 5개 목적지만 출력되게 설정하세요.


df_nflights_copy %>% 
  group_by(dest, AM_PM) %>% 
  summarise(n=n()) %>% 
  mutate(ratio = n/sum(n)*100) %>% 
  filter(AM_PM == "AM") %>% 
  arrange(desc(ratio)) %>% 
  head(5)












