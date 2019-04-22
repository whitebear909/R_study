# 한국복지패널 데이터
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)

raw_welfare <- read.spss("D:/R_study/01_data/실전_복지패널/data_spss_Koweps2014.sav", to.data.frame = T)

table(raw_welfare)
str(raw_welfare)
View(raw_welfare)
dim(raw_welfare)
head(raw_welfare)

welfare <- rename(raw_welfare, 
                  sex = h0901_4,
                  birth = h0901_5,
                  income = h09_din)
welfare

welfare_1 <- select(welfare, sex, birth, income)

boxplot(welfare_1)$state

class(welfare_1$sex)

summary(welfare_1$sex)


#항목 명 변경####

table(welfare_1$sex)

welfare_1$sex <- ifelse(welfare_1$sex == 1, "male", "female")

table(welfare_1$sex)

#이상치 검증 ####
summary(welfare_1$income)

# 소득이 0이거나 모르는 경우 결측 처리
welfare_1$income <- ifelse(welfare_1$income %in% c(0,9999), NA, welfare_1$income)

# 결측치 검즟 ####
table(is.na(welfare_1$income))
summary(welfare_1$income)
str(welfare_1)

summarise(welfare_1$income)

welfare_1 %>% group_by(sex) %>% summarise(sex_sum=sum(income))

str(welfare_1)

welfare_1 <- mutate(welfare_1, age = 2014-welfare_1$birth )


#태어난 연도 결측 및 이상치 검증 ####
table(welfare_1$birth)
qplot(welfare_1$birth)
summary(welfare_1$birth) #연속변수의 이상치 검증

welfare_1 <- mutate(welfare_1, age = 2014-welfare_1$birth+1 )
summary(welfare_1$age)
qplot(welfare_1$birth)

# 나이별 소득 평균분석 ####
#  나이별 소득 평균표 생성 ####

table_age_income <- welfare_1 %>% group_by(age) %>% summarise(income_sum=sum(income))

ggplot(data = table_age_income, aes(x = sex, y=age))

?ggplot

welfare_1 %>% group_by(sex) %>% summarise(sex_sum=sum(income))

