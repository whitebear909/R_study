## Q1. 아래 학생 두 명의 이름과 시험 점수가 있습니다. data.frame()과 c()를 중첩해 데이터 프레임을 만들어 출력하세요.

# 이름      : kim, park
# 시험 점수 : 84, 65

df_q1 <- data.frame(name = c("kim", "park"),
                    score = c(84, 65))

df_q1


## Q2. 앞에서 만든 데이터 프레임을 csv 파일로 저장하세요.

write.csv(df_q1, file = "q1_csv", row.names = F)

?write.csv

## Q3. 앞에서 만든 데이터 프레임을 삭제한 후 삭제되었는지 확인하기 위해 데이터 프레임을 출력하세요.

rm(df_q1)
df_q1

## Q4. 저장한 csv 파일을 불러온 후 score 평균을 출력하세요.

df_q4 <- read.csv(file = "q1_csv")
mean(df_q4$score)


## Q5. 첨부된 raw.xlsx는 세 가지 과목(r, python, html)의 시험 점수를 담고 있습니다. 이 파일을 불러온 다음 잘 불러들여졌는지 확인하세요.
install.packages("readxl")
library(readxl)

df_q5 <- read_excel("d:/R_study/Quiz/raw.xlsx", sheet = 1, col_names = T)
df_q5

## Q6. 데이터의 특징을 파악할 수 있도록 앞 부분 일부를 출력하세요.
head(df_q5)

## Q7. 데이터가 몇 개의 row와 몇 개의 변수로 구성되어 있는지, 어떤 특징을 지닌 변수들로 구성되어 있는지 알아보세요. 하나의 함수만 이용하세요.
str(df_q5)

## Q8. 모든 변수의 요약 통계량을 출력하세요.
summary(df_q5)

## 위 문제의 출력 결과를 보고 아래 질문에 답하세요.

# (1) python 과목의 중앙값은 얼마인가?
# (2) r 과목에서 1등한 학생의 점수는 몇 점인가?
# (3) html 과목의 평균 점수는 얼마인가?

## 정답 입력 ##
# (1) 54
# (2) 90
# (3) 91.2
