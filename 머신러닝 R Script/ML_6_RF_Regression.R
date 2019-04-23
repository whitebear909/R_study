
# --------------------------------------------------------------------------------
# 랜덤 포레스트 회귀모형 적합 실습 
# --------------------------------------------------------------------------------

# 랜덤 포레스트는 관측값(행)과 변수들(열)을 임의로 샘플링하므로(부트스트래핑), 
# 항상 같은 결과를 얻으려면 set.seed()를 지정해야 합니다. 
# ntree 인자에 별도의 값을 지정하지 않으면 기본값인 500개의 나무모형을 적합합니다. 
# mtry 인자에 별도의 값을 지정하지 않으면 분류모형의 경우 p/3의 몫이 할당됩니다. 
# 이번 실습에서는 ntree에 1000, mtry에 3을 할당하겠습니다. 


# Rdata를 불러옵니다. 
load(file = './data/Cadata_House.Rdata')

# 필요한 패키지를 불러옵니다. 
library(tidyverse)
library(randomForest)

# 시드를 설정합니다. 
set.seed(seed = 1234)

# 랜덤 포레스트 회귀모형을 적합합니다. 
# xtest와 ytest에 시험셋을 할당하지 않으면 OOB 에러 추정만 확인할 수 있습니다. 
fitRFR <- randomForest(x = trainSet[, -1], 
                       y = trainSet[, 1], 
                       xtest = testSet[, -1], 
                       ytest = testSet[, 1], 
                       ntree = 1000, 
                       mtry = 3, 
                       importance = TRUE, 
                       do.trace = 50, 
                       keep.forest = TRUE)

# 모형 적합 결과를 확인합니다. 
print(x = fitRFR)


# 모형 적합 결과를 그래프로 출력합니다. 
# 실습 데이터셋의 목표변수가 'MedianHouseValue'이므로 분류모형과 달리 회귀모형은 
# MSE 기준으로 하나의 곡선이 그려집니다. 
plot(x = fitRFR)

## 초기 100개의 나무를 평균한 이후 전체 모형의 성능이 크게 변하지 않았습니다. 


# 랜덤포레스트는 일종의 블랙박스 모형이므로 하나의 최종 모형을 적합할 수 없습니다. 
# 하지만 '변수의 중요도'를 출력하면 전체 결과에 상대적으로 크게 영향을 미치는 
# 변수의 순서를 확인할 수 있습니다. 

# 회귀모형에서 변수의 중요도는, 해당 변수가 개별 나무모형에서 제외되었을 때의 
# MSE의 증가량 및 부모마디에서 가지가 분할할 때 순수도 개선에 기여한 크기를 
# 내림차순으로 제시합니다. 

# 변수 중요도 테이블을 출력합니다. 
importance(x = fitRFR)

## MedianIncome 변수를 제외하면 MSE가 180.32100% 증가한다는 의미합니다. 


# 변수의 중요도를 그래프로 출력합니다. 내림차순으로 정렬되어 출력되므로 한 눈에 파악됩니다.
varImpPlot(x = fitRFR, main = 'Random Forest Regression Model with Cadata House Dataset')


# 랜덤 포레스트 회귀모형에 사용된 전체 개별 나무모형의 끝마디 수를 히스토그램으로 그립니다.
fitRFR %>% 
  treesize(terminal = TRUE) %>% 
  hist(main = 'Histogram of Number of Terinal Nodes')


# --------------------------------------------------------------------------------
# 랜덤 포레스트 회귀모형의 성능 지표를 확인합니다. 
# --------------------------------------------------------------------------------

# 각종 회귀모형 성능지표를 출력하는 R파일을 불러옵니다. 
source(file = './code/extra/Errors_fun.R')


# 시험셋을 할당하여 목표변수의 추정값을 구합니다. 
tePred <- predict(object = fitRFR, newdata = testSet, type = 'response')

# [참고] 모형 적합 시 randomForest() 함수에 testSet을 할당했다면 아래와 같은 방법으로 
# 시험셋의 목표변수 추정확률을 가져올 수 있습니다. 
# tePred <- fitRFR$test$predicted

# 시험셋의 실제값을 teReal에 할당합니다. 
teReal <- testSet$MedianHouseValue

# 실제값과 추정값으로 산점도를 그려봅니다. 
plot(x = teReal, y = tePred)

# 성능지표를 출력합니다. 
getErrors(real = teReal, pred = tePred)


# --------------------------------------------------------------------------------

# 선형회귀분석 모형을 불러옵니다.
fitLReg <- readRDS(file = './data/Linear_Regression.RDS')

# 선형회귀모형으로 목표변수의 추정값을 구합니다. 
tePredLReg <- predict(object = fitLReg, newdata = testSet, type = 'response')

# 성능지표를 출력합니다. 
getErrors(real = teReal, pred = tePredLReg)


## 랜덤 포레스트 회귀모형의 오차값이 크게 감소되었다는 것을 알 수 있습니다. 


# --------------------------------------------------------------------------------
# 랜덤 포레스트 회귀모형 튜닝 실습 
# --------------------------------------------------------------------------------

# 최적의 하이퍼 파라미터(나무의 수, 입력변수의 수)를 찾기 위해 튜닝합니다. 
# 탐색할 나무의 수와 입력변수의 수를 분석가가 선정한 다음 그리드(grid)를 생성합니다.
# 그리드 객체를 생성할 때, expand.grid() 함수를 사용하며 데이터프레임이 반환됩니다.   
# 그리드를 출력하면 두 개의 하이퍼 파라미터들의 조합인 것을 알 수 있습니다. 
# 튜닝하는 과정에서 그리드의 각 행을 반복하며 실행합니다. 
# 회귀모형의 경우, MSE 평균이 가장 낮을 때의 하이퍼 파라미터를 선정합니다. 

# 탐색할 나무의 수와 입력변수의 수를 설정합니다. 
grid <- expand.grid(ntree = c(300, 500, 1000), 
                    mtry = c(3, 4, 5, 6, 7))

# grid 객체를 출력합니다. 
print(x = grid)

## 나무의 수 3종과 입력변수의 수 5종으로 설정되었으므로 총 15개의 조합이 생성됩니다.


# 미리 설정된 나무의 수와 입력변수의 수를 바꿔가며 최적의 모형을 탐색합니다. 
# 각 모형의 MSE 평균을 저장할 빈 데이터프레임을 생성합니다. 
tuned <- data.frame()

# 반복문을 실행할 횟수를 정합니다. 
n <- nrow(x = grid)

# 반복문을 실행합니다. 
for (i in 1:n) {
  
  # 랜덤 포레스트는 임의로 훈련셋과 입력변수를 선택하므로 
  # Seed를 고정하여 항상 같은 결과가 나오도록 합니다. 
  set.seed(seed = 1234)
  
  # 진행경과 확인을 위해 하이퍼 파리미터 후보를 출력합니다. 
  cat('\n', i, '행 실행 중! [ mtree:', grid[i, 'ntree'], ', mtry:', grid[i, 'mtry'], ']\n\n')
  
  # grid의 조합을 반복하여 분류모형을 적합합니다. 
  fit <- randomForest(x = trainSet[, -1], 
                      y = trainSet[, 1], 
                      xtest = testSet[, -1], 
                      ytest = testSet[, 1], 
                      ntree = grid[i, 'ntree'], 
                      mtry = grid[i, 'mtry'], 
                      importance = TRUE, 
                      do.trace = 50, 
                      keep.forest = TRUE)
  
  # 1000개 개별 나무모형의 MSE를 할당합니다. 
  MSEs <- fit$mse
  
  # MSE의 평균을 구합니다. 
  avgMSE <- mean(x = MSEs)
  
  # grid 번호와 오분류율로 새로운 데이터프레임을 생성합니다. 
  df <- data.frame(index = i, averageMSE = avgMSE)
  
  # 최종 데이터프레임에 추가합니다. 
  tuned <- rbind(tuned, df)
  
}


# 모형 튜닝에 오랜 시간이 소요되므로 미리 실행한 결과를 Rdata로 저장하고, 
# 강의시간에는 결과를 불러오도록 하겠습니다. 
# save(list = 'tuned', file = './data/RFR_BestModel_Tuned.Rdata')
load(file = './data/RFR_BestModel_Tuned.Rdata')


# 튜닝 결과 데이터프레임을 출력하여 확인합니다. 
print(x = cbind(grid, tuned))

# 그래프로 그려봅니다. 
plot(x = tuned, xlab = '', ylab = 'Average of MSE')

# MSE 최소값에 맞춰 수평선을 추가합니다. 
abline(h = min(x = tuned$averageMSE), col = 'red', lty = 2)

# MSE가 최소값인 행번호를 지정합니다. 
loc <- (tuned$averageMSE == min(x = tuned$averageMSE)) %>% which()
print(x = loc)

# MSE가 최소값인 하이퍼 파라미터를 찾습니다. 
bestPara <- grid[loc, ]
print(x = bestPara)

## 나무의 수 1000, 입력변수의 수 5일 때 가장 좋은 모형이 적합되었습니다. 


# --------------------------------------------------------------------------------

# 베스트모형을 적합합니다. 
bestRFR <- randomForest(x = trainSet[, -1], 
                        y = trainSet[, 1], 
                        xtest = testSet[, -1], 
                        ytest = testSet[, 1], 
                        ntree = bestPara$ntree, 
                        mtry = bestPara$mtry, 
                        importance = TRUE, 
                        do.trace = 50, 
                        keep.forest = TRUE)

# 모형 적합 결과를 확인합니다. 
print(x = bestRFR)


# 모형 적합 결과를 그래프로 출력합니다. 
# 실습 데이터셋의 목표변수가 'MedianHouseValue'이므로 분류모형과 달리 회귀모형은 
# MSE 기준으로 하나의 곡선이 그려집니다. 
plot(x = bestRFR)

## 초기 50개의 나무를 평균한 이후 전체 모형의 성능이 크게 변하지 않았습니다. 


# 변수 중요도 테이블을 출력합니다. 
importance(x = bestRFR)

# 변수의 중요도를 그래프로 출력합니다. 내림차순으로 정렬되어 출력되므로 한 눈에 파악됩니다.
varImpPlot(x = bestRFR, main = 'Random Forest Regression Model with Cadata House Dataset')

# 튜닝 전 회귀모형과 비교해보겠습니다.
varImpPlot(x = fitRFR, main = 'Random Forest Regression Model with Cadata House Dataset')


# --------------------------------------------------------------------------------
# 베스트 모형의 성능지표를 확인합니다. 
# --------------------------------------------------------------------------------

# 베스트모형의 시험셋 추정값을 teProbBest에 할당합니다. 
tePredBest <- bestRFR$test$predicted

# 시험셋의 실제값과 베스트모형의 추정값으로 산점도를 그려봅니다. 
plot(x = teReal, y = tePredBest)

# 성능지표를 출력합니다. 
getErrors(real = teReal, pred = tePredBest)

# 기본모형의 성능지표와 비교합니다. 
getErrors(real = teReal, pred = tePred)


## End of Document
