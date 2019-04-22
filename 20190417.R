#library loading ####
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

#Data Reading ####
#ex) 
# df_finalexam <- read_excel("01_data/finalexam.xlsx", sheet=1, col_names = T)
# exam <- read.csv("01_data/csv_exam.csv", header = T)

#Data Type Changing ####
# mpg <- as.data.frame(ggplot2::mpg) 

#Data writing ####
#ex) 
# write.csv(df_finalexam, file="01_data/ouput_newdata.csv")

#Data column Changing ####
#ex) 
# df_new = rename(df_new, city = cty)
# df_new = rename(df_new, highway = hwy)

#if esle ####
#ex) and &, or | 
#mpg$test <- ifelse(mpg$total>20, "pass", "fail")

#filter & select ####
#ex)
# exam %>% filter(class ==1 & math >= 50)
# exam %>% filter(english >=90 | math >= 90)

# Data check function ####

head(exam)
head(exam, 10)

tail(exam)
tail(exam, 10)

View(exam)

dim(exam)

str(exam)

summary(exam)



exam <- read.csv("d:/R_study/01_data/csv_exam.csv")

str(exam)

mpg <- as.data.frame(ggplot2::mpg)


library(dplyr)
mpg %>% 
  filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

exam %>% arrange(class,desc(math))  

test <- exam %>% mutate(total = math + english + science) %>%  head 

test

# q1 ####
mpg_copy = mpg
mpg_copy <- mpg %>% mutate(total = cty + hwy)

# q2 ####
mpg_copy <- mpg_copy %>% mutate(mean = total / 2)
mpg_copy

# q3 ####
mpg_copy %>% arrange(desc(mean)) %>% head(3)

# q4 ####
mpg %>% mutate(total = cty + hwy,
                mean = total / 2
                ) %>% arrange(desc(mean)) %>% head(3)
  
mpg %>% group_by(manufacturer) %>% summarise(cty = sum(cty,na.rm = T))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            n = n()) %>% 
  filter(class %in% c(1,2))

# q1 ####
mpg_copy %>% group_by(class) %>% summarise(cty_mean = mean(cty)) 

# q2 ####
mpg_copy %>% group_by(class) %>% summarise(cty_mean = mean(cty)) %>% arrange(desc(cty_mean))

# q3 ####
mpg_copy %>% group_by(manufacturer) %>% summarise(hwy_mean = mean(hwy)) %>% arrange(desc(hwy_mean)) %>% head(3)

# q4 ####
mpg_copy %>% 
  filter(class == "compact") %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))




