
# --------------------------------------------------------------------------------
# 분류모형 : 혼동 행렬 실습
# --------------------------------------------------------------------------------

# iris 데이터로 의사결정나무를 적합한 다음 혼동행렬로 성능평가 지표를 살펴봅니다. 

# 필요한 패키지를 불러옵니다. 
library(tidyverse)

# iris 데이터의 구조를 확인합니다. 
str(object = iris)

# 요약데이터를 출력합니다. 
summary(object = iris)

# 목표변수로 사용할 Species는 3개의 레벨을 갖는 범주형입니다. 
# 이진분류를 위해 setosa를 제외합니다. 
dataSet <- iris[iris$Species != 'setosa', ]

# dataSet의 Species의 레벨을 확인합니다. 
levels(x = dataSet$Species)

# dataSet의 Species를 문자형 벡터로 치환한 다음 범주형으로 재치환합니다. 
# 이렇게 함으로써 레벨을 2개로 줄일 수 있으며 
# 모형 적합 후 혼동행렬을 실행했을 때 이진분류로 실행됩니다. 
dataSet$Species <- dataSet$Species %>% as.character() %>% as.factor()
levels(x = dataSet$Species)


# 의사결정나무를 적합합니다. 
library(rpart)
fitTree <- rpart(formula = Species ~ ., data = dataSet)

# 적합된 모형을 출력합니다. 
summary(object = fitTree)


# 적합한 모형으로 추정값을 생성합니다. 
pred <- predict(object = fitTree, newdata = dataSet, type = 'class')

# 실제값을 real 벡터로 지정합니다. 
real <- dataSet$Species


# 혼동행렬에 필요한 패키지를 불러옵니다. 
library(caret)

# 혼동행렬을 출력합니다. 
confusionMatrix(data = pred, reference = real)


# --------------------------------------------------------------------------------
# 분류모형 : F1 점수 실습 
# --------------------------------------------------------------------------------

# iris 데이터로 의사결정나무를 적합한 결과로 F1 점수를 확인합니다. 
# F1 점수는 혼동행렬에서 확인되는 민감도와 정밀도의 조화평균입니다. 

# 필요한 패키지를 불러옵니다. 
library(MLmetrics)

# F1 점수를 계산합니다. 
F1_Score(y_true = real, y_pred = pred)


# --------------------------------------------------------------------------------
# 분류모형 : ROC 커브 및 AUROC 실습 
# --------------------------------------------------------------------------------

# iris 데이터로 의사결정나무를 적합한 결과로 ROC 커브를 그리고 AUROC를 계산합니다. 
# ROC 커브는 x축에 1-특이도, y축에 민감도를 놓고 그린 그림입니다. 
# 왼쪽 모서리에 가까울수록 분류모형의 성능이 우수합니다. 

# AUROC는 ROC 곡선 아래 면적을 의미합니다. 
# 1-특이도 및 민감도가 각각 0~1의 값을 가지므로 AUROC의 최대값은 0입니다. 


# 필요한 패키지를 불러옵니다. 
library(ROCR)

# pred와 real이 범주형일 수 있으므로 숫자형 벡터로 변환합니다. 
pred <- pred %>% as.numeric()
real <- real %>% as.numeric()


# ROC 커브를 그리기 위해 prediction object를 생성합니다. 
# pred와 real이 범주형일 수 있으므로 숫자형 벡터로 변환합니다. 
predObj <- prediction(predictions = pred, 
                      labels = real)

# predObj 객체를 활용하여 performance 객체를 생성합니다. 
perform <- performance(prediction.obj = predObj, 
                       measure = 'tpr', 
                       x.measure = 'fpr')

# ROC 커브를 그립니다. 
plot(x = perform, main = 'ROC curve with iris dataset')

# 편의상 왼쪽 아래 모서리에서 오른쪽 위 모서리를 잇는 대각선을 추가합니다. 
lines(x = c(0, 1), y = c(0, 1), col = 'red', lty = 2)


# --------------------------------------------------------------------------------

# AUROC에 필요한 패키지를 불러옵니다. 
library(pROC)

# AUROC를 계산합니다. 
auroc <- auc(real, pred)
print(x = auroc)

# AUROC를 ROC 그래프 오른쪽 아래에 추가합니다. 
text(x = 0.9, y = 0, labels = str_c('AUROC : ', auroc), col = 'red', font = 2)


# --------------------------------------------------------------------------------
# 회귀모형 : 다양한 오차 측정 실습 
# --------------------------------------------------------------------------------

# women 데이터셋으로 단순선형회귀모형을 적합하고 4가지 성능지표를 계산해보겠습니다. 

# women 데이터셋의 구조를 파악합니다. 
str(object = women)

# 요약데이터를 출력합니다. 
summary(object = women)

# 2차원 평면에 산점도를 그려봅니다.
plot(x = women$height, y = women$weight)

# 회귀모형을 적합합니다. 
fitSReg <- lm(formula = weight ~ height, data = women)

# 적합된 회귀모형을 출력합니다. 
summary(object = fitSReg)

# 회귀모형을 산점도 위에 추가합니다. 
abline(reg = fitSReg, col = 'red', lty = 2)


# 추정값을 생성합니다. 
pred <- predict(object = fitSReg, newdata = women, type = 'response')

# 실제값을 real 벡터로 생성홥니다. 
real <- women$weight

# 실제값과 추정값의 차이(오차)를 생성합니다. 
error <- real - pred


# MSE를 계산합니다. 
error^2 %>% mean()

# RMSE를 계산합니다. 
error^2 %>% mean() %>% sqrt()

# MAE를 계산합니다. 
error %>% abs() %>% mean()

# MAPE를 계산합니다. 
((error %>% abs()) / (real %>% abs())) %>% mean()


## End of Document
