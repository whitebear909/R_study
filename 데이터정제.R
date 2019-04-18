
df <- data.frame(sex = c("M","F",NA, "M", "F"),
                 score = c(5,4,3,4,NA))
df

is.na(df)
table(is.na(df)) #결측치 빈도 출력
table(is.na(df$sex))

mean(df$score)

filter(df, is.na(score))

filter(df, !is.na(score))

df_nomiss <- filter(df, !is.na(score))

df_nomiss <- filter(df, !is.na(sex) & !is.na(score))

mean(df_nomiss$score)

df_nomiss <- na.omit(df) # 주의!!!! 분석 대상만 제거하는 방식 추천


mean(df$score, na.rm = T)
sum(df$score, na.rm = T)

#결측치 생성
exam <- read.csv("csv_exam.csv")
exam[c(3,8,15),"math"] <- NA

exam %>% summarise(mean_math = mean(math)) #NA로 오류

exam %>% summarise(mean_math = mean(math, na.rm = T)) #NA제거 처리

#결측치 대체법 imputation ####
# 1.대표값 (평균, 최빈값) 일괄대체
# 2.분석값으로 대체

#평균 대체
mean(exam$math, na.rm = T)

exam$math <- ifelse(is.na(exam$math),mean(exam$math, na.rm = T), exam$math )

mean(exam$math)


mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),"hwy"] <- NA

table(is.na(mpg$drv))

table(is.na(mpg$hwy))

filter(mpg, !is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(hwy_mean = mean(hwy))

# 이상치 정체 ####
# 이상치를 결측치로 변경 함(이상치를 결측치로 간주함)

outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))

table(outlier$sex)

table(outlier$score)

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex) 

outlier$sex

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score) 

filter(outlier, !is.na(outlier$sex)&!is.na(outlier$score)) %>% 
  group_by (sex) %>% 
  summarise(sex_mean = mean(score))

