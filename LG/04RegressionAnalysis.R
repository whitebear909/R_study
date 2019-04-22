# Regression Analysis : 회귀분석(예측)
# 인과관계를 분석할때 원인/결과, 상관분석과는 다름

# 어떤 자료들이 다른 양적자료에게 영향을 주는지를 분석하는 방법
# 통계학의 꽃중의 꽃

# 독립변수 = 설명변수 = Feature = 영향을 주는 변수 = input
# 종속변수 = 반응변수 = Label = 영향을 받는 변수 = ouput

# 기본
# 독립변수 : 양적자료
# 종속변수 : 양적자료

# 독립변수에 질적자료 : 더미변수(dummy variable)
# 종속변수에 질적자료 : 로지스틱 회귀분석

# 독립변수 : 양적자료 1개
# 종속변수 : 양적자료 1개
# 단순선형 회귀분석 (독립변수가 1개이므로)
# result <- lm(formula = 종속변수 ~ 독립변수, data = )
# lm :Linear Model
# summary(result)

result <- lm(formula = dist ~ speed, data = cars)
summary(result)

# 1단계 : 회귀모형은 통계적으로 타당한가>
# 귀무가설 : 회귀모형이 타당하지 않다.
# 대립가설 : 회귀모형이 타당하다.
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12
# p-value 가 0.000 따라서 대립가설이 맞음. 회귀모형은 타당하다

# 2단계 : 독립변수는 종속변수에게 영향을 주는가?
# 귀무가설 : 독립변수는 종속변수에게 영향을 주지 않는다.
# 대립가설 : 독립변수는 종속변수에게 영향을 준다.
# Coefficients:
#         Estimate Std.    Error   t value  Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
#  speed        3.9324     0.4155   9.464   1.49e-12 ***
# p value 0.000, 즉 대립가설 독립변수는 종속변수에게 영향을 준다.

# 3단계 : 독립변수는 종속변수에게 어떠한 영향을 주는가?
# speed의 회귀계수: 3.932 (기울기)
# speed의 기본단위가 1증가하면 dist는 약 3.932 증가시키는 영향을 준다.

# speed가 1mph 증가하면 dist는 약 3.932 feet 정도 증가된다.

# 4단계 : 회의모혀의 설명력 = 독립변수의 설명력
# 독립변수가 종속변수의 다름을 어느정도 설명하고 있는가?
# 결정계수 = R-Square 
# R-squared:  0.651 65.1% 
# speed가 dist의 다름을 약 65.1% 정도 설명하고 있다.
# 독립변수가 많으면 Adjusted R square가 높은게 좋다. 하나면 R-square

# 5단계 : 예측(prediction)
# predict(model, newdata = data.frame(독립변수명 = ))
predict(result,
        newdata = data.frame(speed = c(200, 300, 400)))

