install.packages("caret")
install.packages("MLmetrics")
install.packages("tidyverse")
install.packages("rpart.plot")
install.packages("tree")
install.packages("e1071")
install.packages("pROC")


# --------------------------------------------------------------------------------
# 이지분리 의사결정나무 실습
# --------------------------------------------------------------------------------

# 목표변수의 범주가 두 개인 이지분리 의사결정나무를 적합하고 가지치기를 통해 
# 모형을 완성한 다음, 나무모형을 해석하는 방법까지 확인해보는 실습입니다. 

# 분석에 사용할 데이터는 인터넷 상에서 공유되고 있는 UniversalBank.csv 파일로 
# 다양한 금융 관련 컬럼 및 개인대출을 받은 여부를 포함하고 있습니다. 


# 필요한 패키지를 불러옵니다. 
library(tidyverse)

# 데이터를 불러옵니다. 
# 원래 위치 : https://raw.githubusercontent.com/gchoi/Dataset/master/UniversalBank.csv
bank <- read.csv(file = 'https://goo.gl/vE8GyN')

# 데이터셋의 구조를 파악합니다. 
str(object = bank)

# 처음 10행만 미리보기 합니다. 
head(x = bank, n = 10L)

## 데이터셋은 5000행, 14열을 갖는 데이터프레임입니다. 
## 데이터셋 중에서 PersonalLoan은 개인대출 받은 여부이므로 목표변수로 정합니다. 
## 나머지 컬럼 중에서 ID는 모형 적합에 필요 없으므로 삭제합니다. 
## 또한 ZIP.Code 역시 이번 실습에서는 제외합니다. 필요하다고 판단되면 포함시켜도 됩니다. 

# 불필요한 컬럼 삭제합니다. (ID & ZIP.Code)
bank <- bank[, -c(1, 5)]

# 요약통계량을 확인합니다. 
summary(object = bank)


# 목표변수를 범주형 벡터로 변환합니다. (PersonalLoan)
bank$PersonalLoan <- as.factor(x = bank$PersonalLoan)

# 목표변수의 범주별 비중을 확인합니다. 
bank$PersonalLoan %>% table() %>% prop.table()

## 목표변수의 비중을 확인해보니 개인대출을 받은 고객은 전체의 10% 미만입니다. 
## 목표변수가 한 쪽으로 치우친 불균형된 데이터셋이라는 것을 알 수 있습니다. 


# 전체 데이터셋의 70%를 훈련용, 30%를 시험용 데이터로 분리합니다. 
# 같은 결과를 얻기 위해 seed를 설정합니다. 
set.seed(seed = 1234)

# 전체 데이터를 임의로 샘플링하기 위해 다음과 같이 처리합니다. 
index <- sample(x = 1:2, 
                size = nrow(x = bank), 
                prob = c(0.7, 0.3), 
                replace = TRUE)

# index가 1일 때 trainSet, 2일 때 testSet에 할당합니다.
trainSet <- bank[index == 1, ]
testSet <- bank[index == 2, ]

# 훈련셋 목표변수의 빈도와 비중을 차례로 확인합니다. 
trainSet$PersonalLoan %>% table() %>% prop.table()

# 시험셋 목표변수의 빈도와 비중을 차례로 확인합니다. 
testSet$PersonalLoan %>% table() %>% prop.table()


# 랜덤 포레스트 모형에 사용하기 위해 Rdata 파일로 저장합니다. 
#save(list = c('trainSet', 'testSet'), file = './data/UniversalBank.Rdata')
save(list = c('trainSet', 'testSet'), file = 'D:/R_study/머신러닝 R Script/data/UniversalBank.Rdata')



# --------------------------------------------------------------------------------
# 이지분리 의사결정나무 모형을 적합합니다. 
# --------------------------------------------------------------------------------

# 필요 패키지를 불러옵니다.
library(rpart)

# 분류모형을 적합합니다. 
fitTree <- rpart(formula = PersonalLoan ~ ., 
                 data = trainSet, 
                 method = 'class', 
                 parms = list(split = 'gini'),
                 control = rpart.control(minsplit = 20, 
                                         cp = 0.01, 
                                         maxdepth = 10))

# 적합된 모형을 살펴봅니다.
summary(object = fitTree)

## 출력되는 결과가 많지만 우리가 눈여겨봐야 할 항목은 두 번째와 세 번째 표입니다. 

## 두 번째 표는 비용복잡도(CP) 파라미터별로 가지가 분리되는 횟수, 실제 오차 및 
## 교차검정 오차를 제시합니다. 
## 우리는 모형을 적합할 때 비용복잡도 파라미터를 기본값인 0.01로 정했기 때문에
## 그 기준으로 분리된 개수가 9입니다. 이는 끝마디의 수가 10이라는 것을 의미합니다. 

## 세 번째 표는 변수별 중요도를 보여줍니다. 이번 데이터셋에서는 Education, Income, 
## Family, CCAvg, CDAccount, Mortgage, Age 순으로 오분류율이 낮은 모형을 적합하는 데
## 기여했다는 것을 알 수 있습니다. 

## 나머지 내용은 나무모형에 관한 설명인데, 텍스트로는 한 눈에 들어오지 않습니다. 
## 따라서 나무모형을 그림으로 그려보는 것이 좋습니다. 

## 다만 처음 분기되는 내용에 대해 간략하게 설명하면 다음과 같습니다. 
## 뿌리마디에는 3,525 건의 관측값이 있는데, '0'이 3,185건이고 '1'이 340건이므로
## 추정 라벨은 '0'이고 예상 손실은 0.0964539가 됩니다. 
## 왼쪽 자식마디로 2,755건이 할당되고, 오른쪽 자식마디로는 770건이 할당되었습니다.
## 여러 가지 분리규칙 중에서 Improve가 가장 큰 분리규칙이 사용되었습니다. 
## 즉, Income이 110.5 미만인 건은 왼쪽 자식마디로 이동하고, 110.5 이상인 건은 
## 오른쪽 자식마디로 이동하였습니다. 


# 나무모형을 텍스트로 출력합니다.
print(x = fitTree)

## 출력 결과에서 오른쪽 끝에 *가 추가된 것이 끝마디입니다. 
## 모두 세어보니 10개인 것을 알 수 있습니다. 


# 이번에는 나무모형으로 그립니다. 
plot(x = fitTree, 
     compress = TRUE,  # 좌우 폭을 줄입니다. 
     uniform = TRUE,   # 부모마디와 자식마디 간 높이를 일정하게 맞춥니다. 
     branch = 0.4,     # 0에 가까울수록 가지의 각도가 커집니다. 
     margin = 0.05,    # 그림의 여백을 늘리기 때문에 그래프가 작아집니다. 
     main = 'Classification Tree for Universal Bank')

# 분리기준을 텍스트로 추가합니다.
text(x = fitTree, 
     use.n = TRUE,     # 끝마디의 관측값을 범주별로 출력합니다. 
     all = TRUE,       # 전체 마디의 관측값을 범주별로 출력합니다. 
     cex = 1.2)        # 글자의 크기를 키웁니다. 


# rpart.plot 패키지에 있는 함수를 이용하여 보기 좋은 나무모형을 그려봅니다. 
# 필요한 패키지를 불러옵니다. 
library(rpart.plot)

# 더 보기 좋은 나무 모형을 그립니다.
rpart.plot(x = fitTree)

## 이 그래프의 단점은 끝마디에 속한 관측값의 수를 알 수 없다는 것입니다. 
## 하지만 기본 그래프에 비해서 분류 결과와 오분류율 및 전체 관측값 중 해당 끝마디의 
## 비중을 한 번에 알 수 있습니다. 


# 다른 함수로 나무모형을 그려보겠습니다. 
prp(x = fitTree, 
    faclen = 0,    # 0을 할당하면 목표변수의 레이블을 출력합니다. 
    extra = 101,   # 101을 할당하면 끝마디에 목표변수의 범주별 빈도수를 출력합니다. 
    cex = 1.0)


# 끝마디의 색상을 범주별로 다르게 하면 보기 좋을 것입니다. 
# 필요한 패키지를 불러옵니다. 
library(RColorBrewer)

# RColorBrewer 패키지가 제공하는 전체 팔레트를 출력합니다. 
display.brewer.all()

# 확대하고 싶은 팔레트를 하나 선택하여 크게 출력합니다. 
display.brewer.pal(n = 8, name = 'Set2')

# 나만의 팔레트를 설정합니다. 
myPal <- brewer.pal(n = 3, name = 'Set2')

# 나만의 팔레트를 추가하여 나무모형을 그려보겠습니다. 
prp(x = fitTree, 
    faclen = 0,    # 0을 할당하면 목표변수의 레이블을 출력합니다. 
    extra = 101,   # 101을 할당하면 끝마디에 목표변수의 범주별 빈도수를 출력합니다. 
    cex = 1.0, 
    box.pal = myPal[fitTree$frame$yval])


# --------------------------------------------------------------------------------

# 의사결정나무는 지역 모형(Local Model)입니다. 
# 의사결정나무를 2개의 연속형 입력변수로 적합한 다음 2차원 평면에 그리면 
# 전체 데이터셋에서 순수도 높은 세부 그룹별로 구역을 나눌 수 있습니다. 

# rpart 패키지에는 관련 함수가 없으므로 tree 패키지의 partition.tree() 함수를 이용합니다.
# 필요한 패키지를 불러옵니다.
library(tree)

# Income과 Education으로 입력변수로 하는 의사결정나무를 적합합니다. 
fitTree2 <- tree(formula = PersonalLoan ~ Income + Education, data = bank)

# Income과 Education으로 산점도를 그립니다. 
# 산점도의 채우기 색은 PersonalLoan으로 지정합니다. 
plot(x = bank$Income, 
     y = bank$Education, 
     col = grey(level = c(0.8, 0.3))[bank$PersonalLoan], 
     pch = 20, 
     xlab = 'Income', 
     ylab = 'Education')

# 의사결정나무 규칙으로 화면을 분할하고 중앙에 평균값을 출력합니다. 
partition.tree(tree = fitTree2, 
               ordvars = c('Income', 'Education'), 
               col = 'red', 
               cex = 1.2, 
               font = 2, 
               add = TRUE)


# --------------------------------------------------------------------------------

# 나무모형을 검토하였으면, 이제 가지치기가 필요한지 판단해야 할 차례입니다. 

# 비용복잡도 표를 출력합니다. 
printcp(x = fitTree)

## 분리횟수가 9인 현재 나무모형은 xerror가 가장 낮으므로 가지치기가 필요 없습니다. 
## 하지만 가지치기 실습을 위해 정지규칙을 일부 수정하여 나무모형을 다시 적합합니다. 
## 정지규칙을 cp = 0.001, maxdepth = 30으로 변경합니다. 

# 분류모형을 다시 적합합니다. 
fitDeep <- rpart(formula = PersonalLoan ~ ., 
                 data = trainSet, 
                 method = 'class', 
                 parms = list(split = 'gini'),
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
# 가지치기 전 나무모형과 가지치기 후 나무모형의 성능 비교
# --------------------------------------------------------------------------------
getwd()
# ROC 그래프와 AUROC를 출력하는 R함수를 불러옵니다. 

source(file = 'D:/R_study/머신러닝 R Script/ROC_AUROC_fun.R')

#source(file = './code/extra/ROC_AUROC_fun.R')


# 각 나무모형에 시험셋을 적용하여 추정값을 만듭니다. 
tePredDeep <- predict(object = fitDeep, newdata = testSet, type = 'class')
tePredPrun <- predict(object = fitPrun, newdata = testSet, type = 'class')

# 시험셋의 실제값을 teReal에 할당합니다. 
teReal <- testSet$PersonalLoan


# 첫 번째 모형의 혼동행렬 지표들을 확인합니다.
confusionMatrix(data = tePredDeep, reference = teReal, positive = '1')
confusionMatrix(data = tePredPrun, reference = teReal, positive = '1')

# F1 점수를 확인합니다.
F1_Score(y_pred = tePredDeep, y_true = teReal, positive = '1')
F1_Score(y_pred = tePredPrun, y_true = teReal, positive = '1')

# 추정 레이블로 ROC 그래프를 그리고 AUROC를 확인합니다. 
getROC(real = teReal, pred = tePredDeep)
getROC(real = teReal, pred = tePredPrun)


## 시험셋으로 분류 성능을 확인한 결과 가지치기 후 나무모형의 성능이 더 좋습니다.
## 의사결정나무 알고리즘을 사용할 때에는 항상 가지치기를 염두에 두어야 하겠습니다.


# 랜덤 포레스트 모형과 비교하기 위해 가지치기 모형을 RDS로 저장합니다. 
saveRDS(object = fitPrun, file = './data/Decision_Tree_Pruned.RDS')


## End of Document
