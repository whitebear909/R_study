library(dplyr)
library(ggplot2)

## 비율 구하기
mpg %>% 
  group_by(drv) %>% 
  summarise(n = n()) %>% 
  mutate(ratio = n/sum(n)*100)

# 세부 집단별 비율 구하기
mpg %>% 
  group_by(drv, class) %>%
  summarise(n = n()) %>% 
  mutate(ratio = n/sum(n)*100)


## count() 활용
mpg %>% 
  count(drv)

mpg %>% 
  count(drv, sort = T)

mpg %>% 
  count(drv, class)


## count() 활용 - 세부 집단별 비율 구하기
# 분모에 사용할 group 지정
mpg %>% 
  count(manufacturer, class) %>%
  group_by(manufacturer) %>%       
  mutate(ratio = n/sum(n)*100)


## 빈도 0 카운트하기 - group_by(.drop = F)

# 방법1. 집단 변수를 factor 타입으로 변환
mpg$class <- as.factor(mpg$class)
class(mpg$class)

mpg %>% 
  group_by(manufacturer, class, .drop = F) %>%
  summarise(n = n())

# 방법2. 변수를 factor로 변환하여 지정
mpg %>% 
  group_by(manufacturer, class = as.factor(class), .drop = F) %>%
  summarise(n = n())


## 지역명 중복 문제

# 중복 없는 지역명 변수 만들기
midwest <- midwest %>% 
  mutate(state_county = paste(state, county))

midwest %>% select(state, county, state_county)


# 중복 지역만 추출해서 분석하기

# (1) 중복 지역 목록 생성
name <- midwest %>% 
  group_by(county) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2)

# (2) 중복 지역 대상 추출해 분석
midwest %>% 
  filter(county %in% name$county) %>% 
  arrange(county) %>% 
  select(state_county, poptotal)

