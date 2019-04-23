library(readxl)
# Working Directory 확인
getwd()

# 데이터 로드
data <- read.csv(file = "./data/titanic/train.csv", header = TRUE)
str(data)
view(data)

# 필요한 칼럼만 추출함
data_new <- data[,c(2:3,5:8)]

# 범주형 vector로 변경 함
data_new$Survived <- as.factor(x = data_new$Survived)
# data_new$Pclass <- as.factor(x = data_new$Pclass)
# data_new$sex <- as.factor(x = data_new$sex)

# 훈련셋과 시험셋 구분
set.seed(seed = 423)

index <- sample(x = 1:2, 
                size = 891, 
                replace = TRUE, 
                prob = c(0.7, 0.3))

print(index)

trainSet <- data_new[index == 1,]
testSet <- data_new[index == 2,]

# 범주형 vector로 변경 함
library(rpart)
fit <- rpart(formula = Survived ~ ., data = trainSet)

summary(fit)

print(fit)

rpart.plot(x=fit)


source(file = 'D:/R_study/머신러닝 R Script/ROC_AUROC_fun.R')

tePred <- predict(object = fit,
                  newdata = testSet,
                  type = 'class')

teReal <- testSet$Survived
?confusionMatrix
confusionMatrix(data = tePred, 
                reference = teReal,
                positive = "1")
# f1 점수
F1_Score(y_true = teReal, y_pred = tePred, positive =  '1')

# ROC 커브와 AUROC
getROC(real = teReal, pred = tePred)
