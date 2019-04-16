## ggplot2 패키지의 mpg 데이터를 활용해 아래 문제를 해결하세요.

library(ggplot2)

mpg <- as.data.frame(ggplot2::mpg)

## Q1. mpg 데이터에 '도시 연비(cty)와 고속도로 연비(hwy)가 얼마나 차이가 있는지 나타낸 변수'를 추가한 후 일부를 출력해 변수가 잘 추가되었는지 확인하세요.

mpg_copy <- mpg
mpg_copy$diff <- mpg_copy$hwy - mpg_copy$cty
head(mpg_copy)

## Q2. cty가 자동차 전체 평균보다 높으면 'high', 그 외에는 'low' 등급을 부여한 변수를 추가하세요. 등급별 자동차 수를 파악할 수 있도록 빈도표와 그래프를 출력한 후 각 등급의 자동차 수를 입력하세요.

## 정답 입력 ##
mpg_copy$grade <- ifelse(mean(mpg_copy$cty) < mpg_copy$cty, "high", "low" )
mean(mpg_copy$cty)
head(mpg_copy)
table(mpg_copy$grade)
# high 등급 : 118 
# low 등급  : 116






## Q3. 자동차 종류(class)가 'minivan'인 자동차만 추출해 별도의 데이터를 만든 후 cty 평균을 출력하세요.
class_minivan <- mpg_copy %>% filter(class == "minivan")
mean(class_minivan$cty)






## Q4. cty와 hwy가 모두 25 이하인 자동차만 추출해 별도의 데이터를 만드세요.

class_25 <- mpg_copy %>% filter(cty <= 25 & hwy <= 25)
head(class_25)



## Q5. Q4에서 추출한 데이터에서 자동차 종류(class)가 'suv'인 자동차는 몇 대인가요? 

## 정답 입력 - 'suv' 자동차 수 : 60

str(class_25 %>% filter(class == "suv"))
count(class_25 %>% filter(class == "suv"))


## Q6. 제조사(manufacturer)가 'hyundai'인 자동차의 제조사명, 자동차 종류(class), 도시 연비(cty)를 출력하세요.

mpg_copy %>% filter(manufacturer == 'hyundai') %>% select(manufacturer, class, cty)

