test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,70,80,90,85))

test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,80))

test1

test2

total <- left_join(test1,test2, by = "id")

total

name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))

name

exam

exam_teacher <- left_join(exam, name, by = "class")

group_a <- data.frame(id = c(1,2,3,4,5),
                      test  = c(60,80,70,90,85))

group_b <- data.frame(id = c(6,7,8,9,10),
                      test  = c(60,80,70,90,85))

group_total <- bind_rows(group_a, group_b)

group_total

fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = F)
mpg_price_fl <- left_join(mpg,fuel,by = "fl")

select(mpg_price_fl,model, fl, price_fl) %>% head(5)

boxplot(mpg$hwy)$stats

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)

table(is.na(mpg$hwy))

boxplot(mpg$hwy)$stats

#혼자해보기

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93), "drv"] <- "k"
mpg[c(29,43,129,203),"cty"] <- c(3,4,39,42)

table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("f", "r", "4"), mpg$drv, NA)

table(mpg$drv)

#Q2
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)$stats

#Q3
library(dplyr)

filter(mpg, !is.na(cty) & !is.na(drv)) %>% group_by(drv) %>% 
    summarise(mean(cty))






filter(outlier, !is.na(outlier$sex)&!is.na(outlier$score))


