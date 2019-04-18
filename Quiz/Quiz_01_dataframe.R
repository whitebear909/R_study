install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)


## Q1. 숫자 7을 담고 있는 변수를 만들어 출력하세요.

v_1 <- 7
v_1


## Q2. 문자 cup을 담고 있는 변수를 만들어 출력하세요. 

v_str1 <- "cup"
v_str1


## Q3. 남자 세 명의 나이를 담고 있는 변수를 만들어 출력하세요. 각 사람의 나이는 다음과 같습니다. 

# 나이 : 21, 36, 42
v_age <- c(21,36,42)
v_age

## Q4. 앞 문제에서 만든 변수를 이용해서 남자 세 명의 평균 나이를 구하세요.

mean(v_age)


## Q5. 남자 세 명의 평균 나이를 담고 있는 변수를 만들어 출력하세요.

v_age_mean <- mean(v_age)
v_age_mean

## Q6. 여자 세 명의 나이를 담고 있는 변수를 만들어 출력하세요. 각 사람의 나이는 다음과 같습니다. 

# 나이 : 45, 53, 62

v_age_woman <- c(45,53,62)
v_age_woman

## Q7. 여자 세 명의 평균 나이를 담고 있는 변수를 만들어 출력하세요.

v_age_woman_mean <- mean(v_age_woman)
v_age_woman_mean

## Q8. 남자와 여자의 평균 나이를 담고 있는 두 변수를 이용해서 남녀 나이 평균 차이를 구하세요.

v_age_mean - v_age_woman_mean

## Q9. 학생 세 명의 이름, 키, 몸무게를 담고 있는 변수를 각각 만들어 출력하세요.

# 이름   : kim, lee, park
# 키     : 165, 182, 157
# 몸무게 : 62, 89, 53

v_q9_name <- c("kim", "lee", "park")
v_q9_tall <- c(165, 182, 157)
v_q9_weight <- c(62, 89, 53)






## Q10. 앞에서 만든 세 변수를 이용해 데이터 프레임을 만들어 출력하세요.

df_q10 <- data.frame(v_q9_name, v_q9_tall, v_q9_weight)
df_q10 <- rename(df_q10, name = v_q9_name, tall = v_q9_tall, weight = v_q9_weight)

df_q10

## Q11. data.frame()과 c()를 중첩하여 이름, 키, 몸무게 세 변수로 구성된 데이터 프레임을 만들어 출력하세요.

df_q11 <- data.frame(name = c("kim", "lee", "park"),
                       tall = c(165, 182, 157),
                       weight = c(62, 89, 53)
                       )

df_q11

## Q12. 앞에서 만든 데이터 프레임을 이용해 학생들의 평균 키, 평균 몸무게를 담고 있는 변수를 각각 만들어 출력하세요.

v_q12_tall_mean <- mean(df_q11$tall)
v_q12_weight_mean <- mean(df_q11$weight)

v_q12_tall_mean
v_q12_weight_mean




