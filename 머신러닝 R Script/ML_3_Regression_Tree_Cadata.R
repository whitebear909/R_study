
# --------------------------------------------------------------------------------
# 회귀나무모형 실습 
# --------------------------------------------------------------------------------

# 목표변수가 연속형인 회귀나무모형을 적합하고 선형회귀모형과 추정 성능을 비교합니다. 
# 분석에 사용할 데이터는 인터넷 상에서 공유되고 있는 cadata.dat 파일로 마을 단위로 
# 다양한 입력변수와 주택가격 중위수를 포함하고 있습니다. 


# 필요한 패키지를 불러옵니다. 
library(tidyverse)

# 데이터를 불러옵니다. 
# 원래 위치 : https://raw.githubusercontent.com/jbryer/CompStats/master/Data/cadata.dat
house <- read.table(file = 'https://goo.gl/jsvkYS', header = TRUE)

# 데이터의 구조를 파악합니다. 
str(object = house)

# 처음 10행만 미리보기 합니다. 
head(x = house, n = 10L)

## 데이터셋은 20640행, 9열을 갖는 데이터프레임입니다. 
## 데이터셋 중에서 MedianHouseValue는 주택가격 중위수이며 목표변수로 정합니다. 

## 강의시간에 다루기에는 데이터가 크므로 4의 배수에 해당하는 행만 추출하겠습니다. 
house <- house[seq(from = 4, to = 20640, by = 4), ]

# 요약통계량을 확인합니다. 
summary(object = house)


# 전체 데이터셋의 70%를 훈련용, 30%를 시험용 데이터로 분리합니다. 
# 같은 결과를 얻기 위해 seed를 설정합니다. 
set.seed(seed = 1234)

# 전체 데이터를 임의로 샘플링하기 위해 다음과 같이 처리합니다. 
index <- sample(x = 1:2, 
                size = nrow(x = house), 
                prob = c(0.7, 0.3), 
                replace = TRUE)

# index가 1일 때 trainSet, 2일 때 testSet에 할당합니다.
trainSet <- house[index == 1, ]
testSet <- house[index == 2, ]

# 훈련셋 목표변수의 평균을 확인합니다. 
trainSet$MedianHouseValue %>% mean()

# 시험셋 목표변수의 빈도와 비중을 차례로 확인합니다. 
testSet$MedianHouseValue %>% mean()


# 랜덤 포레스트 모형에 사용하기 위해 Rdata 파일로 저장합니다. 
save(list = c('trainSet', 'testSet'), file = './data/Cadata_House.Rdata')


# --------------------------------------------------------------------------------
# 회귀나무모형을 적합합니다. 
# --------------------------------------------------------------------------------

# 필요한 패키지를 불러옵니다.
library(rpart)

# 회귀모형을 적합합니다. 
fitRegT <- rpart(formula = MedianHouseValue ~ .,
                 data = trainSet,
                 method = 'anova',
                 control = rpart.control(cp = 0.01))

# 적합된 모형을 살펴봅니다.
summary(object = fitRegT)

## 출력되는 결과가 많지만 우리가 눈여겨봐야 할 항목은 두 번째와 세 번째 표입니다. 

## 두 번째 표는 비용복잡도(CP) 파라미터별로 가지가 분리되는 횟수, 실제 오차 및 
## 교차검정 오차를 제시합니다. 
## 우리는 모형을 적합할 때 비용복잡도 파라미터를 기본값인 0.01로 정했기 때문에
## 그 기준으로 분리된 개수가 12입니다. 이는 끝마디의 수가 13이라는 것을 의미합니다. 

## 세 번째 표는 변수별 중요도를 보여줍니다. 이번 데이터셋에서는 MedianIncome, 
## Longtitude, Latitude, MedianHouseAge 순으로 분산이 낮은 모형을 적합하는 데 
## 기여했다는 것을 알 수 있습니다. 

## 나머지 내용은 나무모형에 관한 설명인데, 텍스트로는 한 눈에 들어오지 않습니다. 
## 따라서 나무모형을 그림으로 그려보는 것이 좋습니다. 

## 다만 처음 분기되는 내용에 대해 간략하게 설명하면 다음과 같습니다. 
## 뿌리마디에는 3,640 건의 관측값이 있는데, 비용복잡도는 0.3362856이고, 
## 평균은 207460.1, MSE는 1.360066e+10입니다. 

## 왼쪽 자식마디로 2,870건, 오른쪽 자식마디로는 770건이 할당되었습니다.
## 여러 가지 분리규칙 중에서 Improve가 가장 큰 분리규칙이 사용되었습니다. 
## 즉, MedianIncome가 5.01985 미만인 건은 왼쪽 자식마디로 이동하고, 
## 반대로 5.01985 이상인 건은 오른쪽 자식마디로 이동하였습니다. 


# 나무모형을 텍스트로 출력합니다.
print(x = fitRegT)

## 출력 결과에서 오른쪽 끝에 *가 추가된 것이 끝마디입니다. 
## 모두 세어보니 13개인 것을 알 수 있습니다. 


# 이번에는 나무모형으로 그립니다. 
plot(x = fitRegT, 
     compress = TRUE,  # 좌우 폭을 줄입니다. 
     uniform = TRUE,   # 부모마디와 자식마디 간 높이를 일정하게 맞춥니다. 
     branch = 0.4,     # 0에 가까울수록 가지의 각도가 커집니다. 
     margin = 0.05,    # 그림의 여백을 늘리기 때문에 그래프가 작아집니다. 
     main = 'Regression Tree for Median House Value')

# 분리기준을 텍스트로 추가합니다.
text(x = fitRegT, 
     use.n = TRUE,     # 끝마디의 관측값을 범주별로 출력합니다. 
     all = FALSE,      # 전체 마디의 관측값을 범주별로 출력을 해제합니다.
     cex = 1.0)        # 글자의 크기를 키웁니다. 


# rpart.plot 패키지에 있는 함수를 이용하여 보기 좋은 나무모형을 그려봅니다. 
# 필요한 패키지를 불러옵니다. 
library(rpart.plot)

# 더 보기 좋은 나무 모형을 그립니다.
rpart.plot(x = fitRegT)

## 이 그래프의 단점은 끝마디에 속한 관측값의 수를 알 수 없다는 것입니다. 
## 하지만 기본 그래프에 비해서 목표변수 추정값의 평균과 함께 전체 관측값 중 
## 해당 끝마디의 비중을 한 번에 알 수 있습니다. 


# 다른 함수로 나무모형을 그려보겠습니다. 
prp(x = fitRegT, 
    faclen = 0,    # 0을 할당하면 목표변수의 레이블을 출력합니다. 
    extra = 101,   # 101을 할당하면 끝마디에 레코드의 수와 비중을 출력합니다. 
    cex = 1.0)


# 나만의 팔레트를 지정해서 끝마디의 색상을 다르게 출력해보겠습니다. 
# 필요한 패키지를 불러옵니다. 
library(RColorBrewer)

# 나만의 팔레트를 설정합니다. 
myPal <- brewer.pal(n = 9, name = 'Oranges')

# 나만의 팔레트를 추가하여 나무모형을 그려보겠습니다. 
prp(x = fitRegT, 
    faclen = 0,    # 0을 할당하면 목표변수의 레이블을 출력합니다. 
    extra = 101,   # 101을 할당하면 끝마디에 레코드의 수와 비중을 출력합니다. 
    cex = 1.0, 
    box.pal = myPal)


# --------------------------------------------------------------------------------

# 이제 가지치기가 필요한지 판단해야 할 차례입니다. 

# 비용복잡도 표를 출력합니다. 
printcp(x = fitRegT)

## 분리횟수가 13인 현재 나무모형은 xerror이 가장 낮으므로 가지치기가 필요 없습니다. 
## 하지만 가지치기 실습을 위해 정지규칙을 일부 수정하여 나무모형을 다시 적합합니다. 
## 정지규칙을 cp = 0.001, maxdepth = 30으로 변경합니다. 

# 분류모형을 다시 적합합니다. 
fitDeep <- rpart(formula = MedianHouseValue ~ ., 
                 data = trainSet, 
                 method = 'anova', 
                 control = rpart.control(minsplit = 20, 
                                         cp = 0.001, 
                                         maxdepth = 30))

# 적합된 모형을 살펴봅니다.
printcp(x = fitDeep)

# xerror의 최소값을 선택합니다. 
minXerror <- fitDeep$cptable[which.min(x = fitDeep$cptable[, 'xerror']), 'xerror']
print(x = minXerror)


# 교차확인 상대오차 그래프를 그려봅니다. 
plotcp(x = fitDeep)

# minXerror을 그래프 위에 추가합니다. 
abline(h = minXerror, col = 'red', lty = 2)


# xerror의 최소값일 때 CP를 선택합니다. 
bestCP <- fitDeep$cptable[which.min(x = fitDeep$cptable[, 'xerror']), 'CP']
print(x = bestCP)

# bestCP 기준으로 가지치기(pruning)를 합니다. 
fitPrun <- prune.rpart(tree = fitDeep, cp = bestCP)


# 가지치기 전 나무모형을 그림으로 출력합니다. 
prp(x = fitDeep, 
    faclen = 0, 
    extra = 101, 
    cex = 1.0, 
    box.pal = myPal[fitDeep$frame$yval])

# 가지치기 후 나무모형을 그림으로 출력합니다. 
prp(x = fitPrun, 
    faclen = 0, 
    extra = 101, 
    cex = 1.0, 
    box.pal = myPal[fitPrun$frame$yval])


# --------------------------------------------------------------------------------
# 회귀나무모형과 선형회귀모형의 추정 성능
# --------------------------------------------------------------------------------

# 각종 회귀모형 성능지표를 출력하는 R파일을 불러옵니다. 
#source(file = './code/extra/Errors_fun.R')
source(file = 'D:/R_study/머신러닝 R Script/Errors_fun.R')


# 회귀나무모형의 추정값을 만듭니다. 
tePredRegT <- predict(object = fitRegT, newdata = testSet, type = 'vector')

# 실제값을 trReal에 할당합니다. 
teReal <- testSet$MedianHouseValue

# 실제값과 추정값으로 산점도를 그려봅니다. 
plot(x = teReal, y = tePredRegT)

# 성능지표를 출력합니다. 
getErrors(real = teReal, pred = tePredRegT)


# --------------------------------------------------------------------------------

# 선형회귀분석을 이용하여 회귀모형을 적합합니다. 
fitLReg <- lm(formula = MedianHouseValue ~ ., data = trainSet)

# 모형 적합 결과를 확인합니다. 
summary(object = fitLReg)

## F검정의 p-value가 0.05보다 작으므로 회귀모형이 유의하다고 판단합니다. 
## 모든 입력변수의 회귀계수들의 t검정 p-value도 0.05보다 작습니다. 
## 입력변수가 2개 이상이므로 Adjusted R-squared를 참고하면 0.6585입니다. 


# 선형회귀모형으로 목표변수의 추정값을 구합니다. 
tePredLReg <- predict(object = fitLReg, newdata = testSet, type = 'response')

# 성능지표를 출력합니다. 
getErrors(real = teReal, pred = tePredLReg)


# --------------------------------------------------------------------------------

# 성능지표를 한 눈에 보도록 하겠습니다. 
result <- rbind(Regression_Tree = getErrors(real = teReal, pred = tePredRegT),
                Linear_Regression = getErrors(real = teReal, pred = tePredLReg))

# 결과를 출력합니다. 
print(x = result)

## 선형회귀분석이 회귀나무의 추정 성능이 조금 더 우수한 것으로 보입니다. 


# 랜덤 포레스트 모형과 비교하기 위해 fitLReg을 RDS로 저장합니다. 
saveRDS(object = fitLReg, file = 'D:/R_study/머신러닝 R Script/data/Linear_Regression.RDS')

## End of Document
