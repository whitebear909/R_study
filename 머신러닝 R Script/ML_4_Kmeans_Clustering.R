
# --------------------------------------------------------------------------------
# 비계층적 군집분석 실습 : k-means Clustering
# --------------------------------------------------------------------------------

# k-means 군집분석은 전체 데이터를 분석가가 지정해준 k개의 군집으로 묶습니다.  
# 먼저 임의로 k개의 중심을 생성한 후, 각 중심과 전체 관측치 간 거리를 계산하여 
# 가장 가까운 군집으로 속하게 합니다. 모든 관측값에 대해 군집 할당을 마치면 
# 새로 생성된 군집들의 중심을 새로 계산한 다음, 위의 작업을 반복합니다. 
# 중심이 변하지 않을 때까지 위 과정을 반복합니다. 

# k-means 군집분석은 stats 패키지의 kmeans() 함수로 실행할 수 있습니다. 
# factoextra 패키지의 함수에 kmeans()함수를 적용하면 보기 좋은 시각화도 가능합니다.


# 필요한 패키지를 불러옵니다. 
library(tidyverse)
library(factoextra)

# iris 데이터셋에서 Species 컬럼을 제외하고 표준화합니다. 
irisScaled <- scale(x = iris[-5])

# iris 데이터셋에 대해 군집의 수를 3으로 설정하여 k-means 군집분석을 실시합니다. 
# FUNcluster 인자에 'kmeans'라는 문자열을 할당해야 하는 점에 주의하시기 바랍니다. 
fitKms <- eclust(x = irisScaled, 
                 FUNcluster = 'kmeans', 
                 k = 3, 
                 seed = 123)

# 군집분석 결과를 출력합니다. 
print(x = fitKms)

# 군집결과를 실제값과 비교합니다. 
# 군집분석은 비지도학습이지만 알고리즘의 이해를 돕기 위해 실제값과 비교하는 것입니다. 
table(fitKms$cluster, iris$Species)


# --------------------------------------------------------------------------------

# 최적의 군집 수 k를 찾습니다. 

# WSS 기준으로 최적의 군집 수를 확인합니다. 
# FUNcluster 인자에 kmeans라고 함수명을 할당해야 하는 점에 주의하시기 바랍니다. 
fviz_nbclust(x = irisScaled, 
             FUNcluster = kmeans, 
             method = 'wss', 
             linecolor = 'red')

# Silhouette 기준으로 최적의 군집 수를 확인합니다. 
fviz_nbclust(x = irisScaled, 
             FUNcluster = kmeans, 
             method = 'silhouette', 
             linecolor = 'red')

## 실루엣 점수로 실행하면 명확한 기준을 제시하므로 판단하기 좋습니다. 


# 군집분석 결과를 출력합니다. 
fviz_cluster(object = fitKms, 
             repel = TRUE, 
             ellipse = TRUE, 
             ellipse.type = 'norm', 
             ggtheme = theme_bw())

## ellipse.type 인자에는 'convex', 'conficence', 'norm', 'euclid' 등을 할당할 수 있습니다. 


# 군집별 실루엣 점수를 확인합니다. 
fviz_silhouette(sil.obj = fitKms)

## 실루엣 점수는 클수록 군집 내 관측값 간 응집력이 좋은 것입니다. 
## 군집 결과 평균보다 아래에 있는 관측값들은 이웃 군집과의 거리가 가까운 것을 의미합니다. 
## 그리고 음수를 갖는 관측값들은 군집이 잘못 묶였다고 판단할 수 있습니다. 


# 음수 실루엣 점수를 갖는 관측값을 확인합니다. 
fitKms$silinfo$widths[fitKms$silinfo$widths$sil_width < 0, ]

# 원본 데이터셋에서 찾아 확인합니다. 
iris[112, ]
iris[128, ]

# 원본 데이터셋에서 각 컬럼별 평균과 비교합니다. 
iris %>% 
  group_by(Species) %>% 
  summarise(SLM = mean(x = Sepal.Length),
            SWM = mean(x = Sepal.Width),
            PLM = mean(x = Petal.Length),
            PWM = mean(x = Petal.Width))


# --------------------------------------------------------------------------------

# 군집별 특성을 확인합니다. 
fitKms$centers

# 군집별 특성을 원래 데이터셋의 스케일로 변환합니다. 
mu <- sapply(X = iris[1:4], FUN = mean)
sd <- sapply(X = iris[1:4], FUN = sd)

# 원래 데이터셋의 스케일로 복원된 군집의 중심을 확인합니다. 
centers2 <- t(x = t(x = fitKms$centers) * sd + mu) %>% round(digits = 2L)


# 그래프 창을 2*2로 분할합니다. 
par(mfrow = c(2, 2))

# 데이터셋의 입력변수의 수를 지정합니다. 
p <- ncol(x = irisScaled)

# 군집별 입력변수의 평균을 막대그래프로 그립니다. 
for (i in 1:p) {
  bp <- barplot(height = centers2[, i], 
                names.arg = 1:nrow(fitKms$centers), 
                col = 'gray80', 
                ylim = c(0, max(centers2[, i]) * 1.1), 
                xlab = 'Number of Clusters',
                ylab = 'Length of Width',
                main = str_c('Cluster Features of ', colnames(x = centers2)[i]))
  
  text(x = bp,
       y = centers2[, i], 
       labels = centers2[, i],
       pos = 3)
}

# 그래프 창을 원래대로 복원합니다. 
par(mfrow = c(1, 1))


## End of Document 
