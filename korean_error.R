
data <- read_excel(path = "./data/Korean_Women_Volley.xlsx")
str(data)

plot(x = data$height, y = data$weight)

plot(x = data$age, y = data$weight)

# 두 변수간에 선형관계인지 확인 하는 모델
cor.test(x = data$height, y = data$weight)

library(rpart)
fit <- rpart(formula = weight ~ height + age,
             data = data, control = rpart.control(minsplit = 5))
summary(fit)

fitPrun <- prune.rpart(tree = fit, cp = 0.12368082)
fitPrun
rpart.plot(x=fit)

tePred <-  predict(fitPrun, newdata = data, type = 'vector')

teRead <- data$weight

source(file = 'D:/R_study/머신러닝 R Script/Errors_fun.R')
getErrors(real = teReal, pred = tePred)

