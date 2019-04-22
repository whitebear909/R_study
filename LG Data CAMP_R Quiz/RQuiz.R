#################################################
# 각 문제 아래에 정답 코드를 입력하세요.
# 문제를 해결하는데 필요한 패키지를 로드하세요.
#
# 소속 : 판토스
# 이름 : 박인수
#
#################################################

## Q1. 네 사람의 시험 점수를 담고 있는 변수를 만들어 출력하세요. 각 사람의 시험 점수는 다음과 같습니다.

# 점수 : 80, 76, 90, 92
v_score = c(80,76,90,92)
v_score
## Q2. 앞 문제에서 만든 변수를 이용해서 네 사람의 평균 점수를 담고 있는 변수를 만들어 출력하세요.
v_score_mean  <-  mean(v_score)


## Q3. data.frame()과 c()를 중첩하여 학생들의 이름, 시험 점수의 두 변수로 구성된 데이터 프레임을 만들어 출력하세요. 이름과 시험 점수는 다음과 같습니다.

# 이름 : lee, park, kim
# 점수 : 80, 76, 90

df_score <- data.frame(name = c("lee", "park", "kim"),
                       score = c(80,76,90))


## Q4. 앞에서 만든 데이터 프레임을 이용해 학생들의 평균 시험 점수를 담고 있는 변수를 만들어 출력하세요.

v_core_mean2 <- mean(df_score$score) 


##############################################################
# ggplot2 패키지의 mpg 데이터를 이용해 아래 문제를 해결하세요.
# Q5~Q7은 각각 하나의 dplyr 구문만 사용해야 합니다.
##############################################################


# Q5. 자동차 동류(class)별 고속도로 연비(hwy) 평균을 내림차순으로 정렬하여 출력하세요.
library(ggplot2)
library(dplyr)

df_mpg <- as.data.frame(ggplot2::mpg)
df_mpg_new <- df_mpg

df_mpg_new %>% 
  group_by(class) %>% 
  summarise(class_hwy = mean(hwy)) %>% 
  arrange(desc(class_hwy))

## Q6. volkswagen에서 생산한 자동차 중 고속도로 연비(hwy)와 도시 연비(cty)의 차이가 가장 큰 자동차 다섯대의 데이터를 출력하세요.

df_mpg_new %>%
  filter(manufacturer == "volkswagen") %>% 
  mutate(hwy_cty = hwy - cty) %>% 
  arrange(desc(hwy_cty)) %>% 
  head(5)
  
## Q7. 구동 방식(drv)별 빈도와 구성 비율을 오름차순으로 정렬하여 출력하세요.
df_mpg_new %>%
  group_by(drv) %>% 
  summarise(n=n()) %>% 
  mutate(ratio = n / sum(n)*100) %>% 
  arrange(ratio)
  






##############################################################
# carData 패키지의 Arrests 데이터는 '마리화나 소지자 체포 이력'으로 구성되어 있습니다. carData 패키지를 설치해 불러온 뒤 Arrests 데이터를 이용해 아래 문제를 해결하세요.
##############################################################

# Q8. 어떤 연도에 가장 많은 죄수가 체포되었는지 알아보세요.   

install.packages("carData")
df_arrest_raw <- as.data.frame(carData::Arrests)
df_arrest_new <- df_arrest_raw

df_arrest_new %>% 
  group_by(year) %>% 
  summarise(year_sum = sum(checks)) %>% 
  arrange(desc(year_sum)) %>% 
  head(1)

# Q9. 18세 미만은 청소년, 18세 이상은 성인이라고 했을 때, 각 집단의 백분율을 알아보세요.

df_arrest_new %>% 
  mutate(type = ifelse(age>=18, "adult", "young")) %>%  
  group_by(type) %>% 
  summarise(n = n()) %>%
  mutate(ratio = n / sum(n)*100)
  




# Q10. # colour는 인종, released는 석방 여부를 나타낸 변수입니다. 인종별 석방 비율을 알아보세요.

df_arrest_new %>% 
  group_by(colour, released) %>% 
  summarise(n = n()) %>%
  mutate(ratio = n / sum(n)) %>% 
  filter(released != "No")



