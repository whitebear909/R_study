install.packages("psych")
library(psych)


# 상관분석 Corrlation Analysis
# 두 양적자료간에 직선의 관계 = 선형의 관계가 
# 있는지를 통계적으로 분석하는 방법

# 관계 = 관련성

# 자료
# 양적 자료 : 2개, 쌍(paired)으로 되어 있어야 함 (id, x, y)
# 예제 데이터 : cars, attitude

# 1. 산점도(Scatter plot)
# plot(data)

plot(cars)

str(cars)

# 2. 산점행렬도(SPM : Scatter plot Matrix) ----
plot(attitude)

# 3. 상관계수
cor(cars)
cor(attitude)
round(cor(attitude), digits = 3)

# 4. 상관분석 ----
# 귀무가설 : speed와 dist 간에는 관련성이 없다.
# 대립가설 : speed와 dist 간에는 관련성이 있다.
# cor.test(data$variable, data$variable, method = "pearson")
# method : "kendall", "spearman"
cor.test(cars$speed, cars$dist,
         method = "pearson")
# 결론 : 유의 확률이 0.000이므로 유의수준 0.05에서
# speed와 dist 간에는 통계적으로 유의한 양의 상관관계가 있는 것으로 나타났다.
# 그 정도는 매우 높다.

#psych::corr.test(data, method)
psych::corr.test(attitude, method = "pearson")
