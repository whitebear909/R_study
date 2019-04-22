

#install.packages("remotes")
remotes::install_github("mrchypark/seoulsurvey") 
#install.packages("seoulsurvey")

data(pakage = "seoulsurvey")


# 서울시 거주 외국인 데이터 로드
library(ggplot2)
library(dplyr)
library(readxl)
df_raw <- read_excel("d:/R_study/seouldata.xlsx", sheet = 1, col_names = T)
df_new <- df_raw

# 데이터 탐색
View(df_new)

# 사용할 데이터 선정
# 만족도 데이터의 이상치 검증
df_new <- rename(df_new, satisfaction = q4)
summary(df_new$satisfaction)
hist(df_new$satisfaction)

# >> 만족도가 평균 이하인 집단으로 분석대상을 정의 함
df_new %>% 
  summarise(n=n())

# 총 2500명 중 만족도가 평균이하인 집단은 940명 존재 함
df_new_define <-  filter(df_new, satisfaction < 6.883)  
df_new_define %>% 
  summarise(n=n()) 



hist(df_new_define$satisfaction)



# 1) 점수 비율을 산정하여 Target 대상을 좁힘

df_new_define %>% 
  summarise(n=n())

# 총 960명 존재 896명 95.3%가 점수 5, 6점 대상임

df_new_define_table <- df_new_define %>% 
  group_by(df_new_define$satisfaction) %>% 
  summarise(n=n()) %>% 
  mutate(ratio = n /sum(n) *100)

df_new_define2 <- filter(df_new_define, satisfaction > 4 & satisfaction < 7)

#### 2. 대상 분석
####   1) 출신국가 분석
df_new_define2 <- rename(df_new_define2, nation = sq1)
table(df_new_define2$nation)


df_new_define2_table <- df_new_define2 %>% 
  group_by(nation) %>% 
  summarise(n=n()) %>% 
  mutate(ratio = n /sum(n) *100) %>% 
  arrange(desc(ratio))

df_nation = data.frame(nation = c(1,2,3,4,5,6,7,8,9,10),
                       nation_name = c(
                         '한국계중국인',
                         '중        국',
                         '일        본',
                         '타   이   완',
                         '베   트   남',
                         '아시아  기타',
                         '미        국',
                         '영미권  기타',
                         '유   럽   권',
                         '기타(위 보기 제외 지역)'
                       ))

df_new_define2_table <-  left_join(df_new_define2_table, df_nation, by = "nation") 

####   2) 서울거주의 어려움 분석
df_new_define2 <- rename(df_new_define2,
                         language = q8_1,
                         loneliness = q8_2,
                         child_care = q8_3,
                         culture = q8_4,
                         food = q8_5,
                         Prejudice_discrimination = q8_6,
                         economic_opportunity = q8_7,
                         relationship_Koreans = q8_8,
                         public_administration = q8_9,
                         educational_opportunity = q8_10,
                         medical_institutions_use = q8_11,
                         residential_space = q8_12
)
# 9점은 해당사항없음 이므로 0점으로 전환함

df_new_define2$language <- ifelse(df_new_define2$language == 9, 0, df_new_define2$language)
df_new_define2$loneliness <- ifelse(df_new_define2$loneliness == 9, 0, df_new_define2$loneliness)
df_new_define2$child_care <- ifelse(df_new_define2$child_care == 9, 0, df_new_define2$child_care)
df_new_define2$culture <- ifelse(df_new_define2$culture == 9, 0, df_new_define2$culture)
df_new_define2$food <- ifelse(df_new_define2$food == 9, 0, df_new_define2$food)
df_new_define2$Prejudice_discrimination <- ifelse(df_new_define2$Prejudice_discrimination == 9, 0, df_new_define2$Prejudice_discrimination)
df_new_define2$economic_opportunity <- ifelse(df_new_define2$economic_opportunity == 9, 0, df_new_define2$economic_opportunity)
df_new_define2$relationship_Koreans <- ifelse(df_new_define2$relationship_Koreans == 9, 0, df_new_define2$relationship_Koreans)
df_new_define2$public_administration <- ifelse(df_new_define2$public_administration == 9, 0, df_new_define2$public_administration)
df_new_define2$educational_opportunity <- ifelse(df_new_define2$educational_opportunity == 9, 0, df_new_define2$educational_opportunity)
df_new_define2$medical_institutions_use <- ifelse(df_new_define2$medical_institutions_use == 9, 0, df_new_define2$medical_institutions_use)
df_new_define2$residential_space <- ifelse(df_new_define2$residential_space == 9, 0, df_new_define2$residential_space)

df_new_define3 <- df_new_define2 %>% summarise(language = sum(df_new_define2$language),
                                               loneliness = sum(df_new_define2$loneliness),
                                               child_care = sum(df_new_define2$child_care),
                                               culture = sum(df_new_define2$culture),
                                               food = sum(df_new_define2$food),
                                               Prejudice_discrimination = sum(df_new_define2$Prejudice_discrimination),
                                               economic_opportunity = sum(df_new_define2$economic_opportunity),
                                               relationship_Koreans = sum(df_new_define2$relationship_Koreans),
                                               public_administration = sum(df_new_define2$public_administration),
                                               educational_opportunity = sum(df_new_define2$educational_opportunity),
                                               medical_institutions_use = sum(df_new_define2$medical_institutions_use),
                                               residential_space = sum(df_new_define2$residential_space)
)


str(df_new_define3)

