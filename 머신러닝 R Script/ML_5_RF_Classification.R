
# --------------------------------------------------------------------------------
# 랜덤 포레스트 분류모형 적합 실습 
# --------------------------------------------------------------------------------

# 랜덤 포레스트는 관측값(행)과 변수들(열)을 임의로 샘플링하므로(부트스트래핑), 
# 항상 같은 결과를 얻으려면 set.seed()를 지정해야 합니다. 
# ntree 인자에 별도의 값을 지정하지 않으면 기본값인 500개의 나무모형을 적합합니다. 
# mtry 인자에 별도의 값을 지정하지 않으면 분류모형의 경우 양의 제곱근이 할당됩니다. 
# 이번 실습에서는 ntree에 1000, mtry에 3을 할당하겠습니다. 


# Rdata를 불러옵니다. 
load(file = './data/UniversalBank.Rdata')

# 필요한 패키지를 불러옵니다. 
library(tidyverse)
library(randomForest)

# 시드를 설정합니다. 
set.seed(seed = 1234)

# 랜덤 포레스트 분류모형을 적합합니다. 
# xtest와 ytest에 시험셋을 할당하지 않으면 OOB 에러 추정만 확인할 수 있습니다. 
fitRFC <- randomForest(x = trainSet[, -8], 
                       y = trainSet[, 8], 
                       xtest = testSet[, -8], 
                       ytest = testSet[, 8], 
                       ntree = 1000, 
                       mtry = 3, 
                       importance = TRUE, 
                       do.trace = 50, 
                       keep.forest = TRUE)

# 모형 적합 결과를 확인합니다. 
print(x = fitRFC)

# OOB 에러 추정값을 그래프로 그립니다. 
plot(x = fitRFC$err.rate[, 1], 
     ylab = 'OOB Error', 
     type = 'l')


# 오분류된 수를 확인합니다. 
sum(fitRFC$predicted != trainSet$PersonalLoan)

# 오분류율을 계산합니다. 
sum(fitRFC$predicted != trainSet$PersonalLoan) / nrow(x = trainSet)


# 모형 적합 결과를 그래프로 출력합니다. 
# 실습 데이터셋의 목표변수가 'best'(red)와 'good'(green)이므로 두 개의 오분류율
# 곡선이 그려지고, 이를 가중평균한 곡선이 검정색으로 그려집니다. 
plot(x = fitRFC)

## 초기 50개의 나무를 평균한 이후 전체 모형의 성능이 크게 변하지 않았습니다. 


# 랜덤포레스트는 일종의 블랙박스 모형이므로 하나의 최종 모형을 적합할 수 없습니다. 
# 하지만 '변수의 중요도'를 출력하면 전체 결과에 상대적으로 크게 영향을 미치는 
# 변수의 순서를 확인할 수 있습니다. 

# 분류모형에서 변수의 중요도는, 해당 변수가 개별 나무모형에서 제외되었을 때 
# OOB 오차를 증가시키는 %를 계산합니다. 
# 아울러, 정확도의 평균 감소량과 지니지수의 평균 감소량을 제시합니다. 

# 변수 중요도 테이블을 출력합니다. 
importance(x = fitRFC)

## fixed.acidity 변수를 제외하면 best 레벨에 대한 오분류 에러가 42.85% 증가한다는
## 의미합니다. 


# 변수의 중요도를 그래프로 출력합니다. 내림차순으로 정렬되어 출력되므로 한 눈에 파악됩니다.
varImpPlot(x = fitRFC, main = 'Random Forest Classification Model with Bank Dataset')


# 앙상블 방식의 분류모형은 마진(Margin) 그래프로 성능을 가늠할 수 있습니다. 
# 마진이란, 전체 레코드별로 정분류율에서 오분류율의 최대 비율을 뺀 값입니다. 
# 따라서 마진이 1이면, 하나의 레코드에 대해 모든 개별 나무모형이 정분류했다는 것입니다. 
# 반대로 마진이 -1이면, 하나의 레코드에 대해 모든 개별 나무모형이 오분류했다는 것입니다. 
# 색상은 목표변수의 레벨을 나타냅니다. (best는 red, good은 blue)
plot(x = margin(x = fitRFC))

# 두 개의 레벨별 마진의 평균을 구하여 비교해보겠습니다. 
mg <- margin(x = fitRFC)

# mg의 class(속성)는 'margin'이므로 dplyr 패키지 함수가 적용되지 않습니다. 
# 따라서 mg의 레벨은 names()로 추출하고, 마진 값은 as.numeric()으로 변환하여 
# 새로운 데이터프레임을 생성합니다. 
mgDf <- data.frame(class = names(x = mg), 
                   margin = as.numeric(x = mg))

# class별로 평균을 출력합니다. 
mgDf %>% 
  group_by(class) %>% 
  dplyr::summarise(margin = mean(x = margin))


# 랜덤 포레스트 분류모형에 사용된 전체 개별 나무모형의 끝마디 수를 히스토그램으로 그립니다.
fitRFC %>%
  treesize(terminal = TRUE) %>% 
  hist(main = 'Histogram of Number of Terinal Nodes')


# --------------------------------------------------------------------------------
# 랜덤 포레스트 분류모형의 성능을 확인합니다. 
# --------------------------------------------------------------------------------

# ROC 그래프와 AUROC를 출력하는 R함수를 불러옵니다. 
source(file = './code/extra/ROC_AUROC_fun.R')


# 시험셋을 할당하여 추정확률을 생성합니다.
# 랜덤 포레스트의 경우, 적합된 모형에 predict() 함수를 이용하여 시험셋을 예측할 때 
# type 인자에 'vote'를 지정하면 전체 나무모형에서 해당 라벨로 표시된 비중을 반환합니다. 
# 아래 라인을 실행하면 0일 확률과 1일 확률을 행렬로 제시합니다. 
# 우리가 알고자 하는 확률은 1일 확률이므로 두 번째 컬럼만 가져오면 됩니다. 
teProb <- predict(object = fitRFC, newdata = testSet, type = 'vote') %>% `[`(, 2)

# 시험셋의 추정값(라벨)만 생성하려면 type 인자에 'response'를 할당하면 됩니다. 
tePred <- predict(object = fitRFC, newdata = testSet, type = 'response')


# [참고] 모형 적합 시 randomForest() 함수에 testSet을 할당했다면 아래와 같은 방법으로 
# 시험셋의 목표변수 추정확률을 가져올 수 있습니다. 
# teProb <- fitRFC$test$votes %>% `[`(, 2)
# tePred <- fitRFC$test$predicted


# 시험셋의 실제값을 teReal에 할당합니다. 
teReal <- testSet$PersonalLoan

# 혼동행렬을 출력합니다. 
confusionMatrix(data = tePred, reference = teReal, positive = '1')

# F1 점수를 확인합니다.
F1_Score(y_pred = tePred, y_true = teReal, positive = '1')

# 추정확률로 ROC 그래프를 그리고 AUROC를 확인합니다. 
getROC(real = teReal, pred = teProb)


# --------------------------------------------------------------------------------

# 의사결정나무 가지치기 모형을 불러옵니다.
fitPrun <- readRDS(file = './data/Decision_Tree_Pruned.RDS')

# 나무모형에 시험셋을 적용하여 추정값을 만듭니다. 
tePredPrun <- predict(object = fitPrun, newdata = testSet, type = 'class')

# 혼동행렬을 출력합니다. 
confusionMatrix(data = tePredPrun, reference = teReal, positive = '1')

# F1 점수를 확인합니다.
F1_Score(y_pred = tePredPrun, y_true = teReal, positive = '1')

# 추정 레이블로 ROC 그래프를 그리고 AUROC를 확인합니다. 
getROC(real = teReal, pred = tePredPrun)


## 가지치기 모형보다 랜덤 포레스트 모형의 성능이 소폭 우수하다는 것을 알 수 있습니다.


# --------------------------------------------------------------------------------
# 랜덤 포레스트 분류모형 튜닝 실습 
# --------------------------------------------------------------------------------

# 최적의 하이퍼 파라미터(나무의 수, 입력변수의 수)를 찾기 위해 튜닝합니다. 
# 탐색할 나무의 수와 입력변수의 수를 분석가가 선정한 다음 그리드(grid)를 생성합니다.
# 그리드 객체를 생성할 때, expand.grid() 함수를 사용하며 데이터프레임이 반환됩니다.   
# 그리드를 출력하면 두 개의 하이퍼 파라미터들의 조합인 것을 알 수 있습니다. 
# 튜닝하는 과정에서 그리드의 각 행을 반복하며 실행합니다. 
# 분류모형의 경우, 오분류율이 가장 낮을 때의 하이퍼 파라미터를 선정합니다. 

# 탐색할 나무의 수와 입력변수의 수를 설정합니다. 
grid <- expand.grid(ntree = c(300, 500, 1000), 
                    mtry = c(3, 4, 5, 6, 7))

# grid 객체를 출력합니다. 
print(x = grid)

## 나무의 수 3종과 입력변수의 수 5종으로 설정되었으므로 총 15개의 조합이 생성됩니다.


# 미리 설정된 나무의 수와 입력변수의 수를 바꿔가며 최적의 모형을 탐색합니다. 
# 각 모형의 오분류율을 저장할 빈 데이터프레임을 생성합니다. 
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
  fit <- randomForest(x = trainSet[, -8], 
                      y = trainSet[, 8], 
                      xtest = testSet[, -8], 
                      ytest = testSet[, 8], 
                      ntree = grid[i, 'ntree'], 
                      mtry = grid[i, 'mtry'], 
                      importance = TRUE, 
                      do.trace = 50, 
                      keep.forest = TRUE)
  
  # 오분류된 수를 확인합니다. 
  mcSum <- sum(fit$predicted != trainSet$PersonalLoan)
  
  # 오분류율을 계산합니다. 
  mcRate <- mcSum / nrow(x = trainSet)
  
  # grid 번호와 오분류율로 새로운 데이터프레임을 생성합니다. 
  df <- data.frame(index = i, misClassRate = mcRate)
  
  # 최종 데이터프레임에 추가합니다. 
  tuned <- rbind(tuned, df)
  
}


# 모형 튜닝에 오랜 시간이 소요되므로 미리 실행한 결과를 Rdata로 저장하고, 
# 강의시간에는 결과를 불러오도록 하겠습니다. 
# save(list = 'tuned', file = './data/RFC_BestModel_Tuned.Rdata')
load(file = './data/RFC_BestModel_Tuned.Rdata')


# 튜닝 결과 데이터프레임을 출력하여 확인합니다. 
print(x = cbind(grid, tuned))

# 그래프로 그려봅니다. 
plot(x = tuned, xlab = '', ylab = 'MisClassification Rate')

# 오분류율 최소값에 맞춰 수평선을 추가합니다. 
abline(h = min(x = tuned$misClassRate), col = 'red', lty = 2)

# 오분류율이 최소값인 행번호를 지정합니다. 
loc <- (tuned$misClassRate == min(x = tuned$misClassRate)) %>% which()
print(x = loc)

# 오분류율이 최소값인 하이퍼 파라미터를 찾습니다. 
bestPara <- grid[loc, ]
print(x = bestPara)

## 나무의 수 500, 입력변수의 수 4일 때 가장 좋은 모형이 적합되었습니다. 


# --------------------------------------------------------------------------------

# 베스트모형을 적합합니다. 
bestRFC <- randomForest(x = trainSet[, -8], 
                        y = trainSet[, 8], 
                        xtest = testSet[, -8], 
                        ytest = testSet[, 8], 
                        ntree = bestPara$ntree, 
                        mtry = bestPara$mtry, 
                        importance = TRUE, 
                        do.trace = 50, 
                        keep.forest = TRUE)

# 모형 적합 결과를 확인합니다. 
print(x = bestRFC)

# 오분류된 수를 확인합니다. 
sum(bestRFC$predicted != trainSet$PersonalLoan) / nrow(x = trainSet)


# 모형 적합 결과를 그래프로 출력합니다. 
# 실습 데이터셋의 목표변수가 'best'(red)와 'good'(green)이므로 두 개의 오분류율
# 곡선이 그려지고, 이를 가중평균한 곡선이 검정색으로 그려집니다. 
plot(x = bestRFC)

## 초기 50개의 나무를 평균한 이후 전체 모형의 성능이 크게 변하지 않았습니다. 


# 변수 중요도 테이블을 출력합니다. 
importance(x = bestRFC)

# 변수의 중요도를 그래프로 출력합니다. 내림차순으로 정렬되어 출력되므로 한 눈에 파악됩니다.
varImpPlot(x = bestRFC, main = 'Random Forest Classification Best Model with Bank Dataset')

# 튜닝 전 분류모형과 비교해보겠습니다.
varImpPlot(x = fitRFC, main = 'Random Forest Classification Model with Bank Dataset')


# --------------------------------------------------------------------------------
# 베스트 모형의 성능지표를 확인합니다. 
# --------------------------------------------------------------------------------

# 베스트모형의 시험셋 추정확률과 추정값(라벨)을 teProbBest, tePredBest에 할당합니다. 
teProbBest <- bestRFC$test$votes %>% `[`(, 2)
tePredBest <- bestRFC$test$predicted

# 혼동행렬을 출력합니다. 
confusionMatrix(data = tePredBest, reference = teReal, positive = '1')

# F1 점수를 확인합니다.
F1_Score(y_pred = tePredBest, y_true = teReal, positive = '1')

# 추정확률로 ROC 그래프를 그리고 AUROC를 확인합니다. 
getROC(real = teReal, pred = teProbBest)


## End of Document
