# Two sample test
# 두 개의 독립적인 모집단의 양적자료의 평균이
# 한 쪽이 더 큰지, 작은지, 같지 않은지를 
# 통계적으로 분석하는 방법


# 자료:
# 질적자료 : 1개, 두개의 집단으로 이루어져야 함(A,B)
# 양적자료 : 1개
# 귀무가설 : A수면제의 수면시간의 평균과
#            B수면제의 수면시간은 같다.
# 수면제 제품에 따라 수면시간에 차이가 없다.
# 대립가설 : A수면제의 수면시간의 평균과
#            B수면제의 수면시간은 같지 않다.
# 수면제 제품에 따라 수면시간에 차이가 있다.

# 1단계 : 정규성 검정
# 귀무가설 : 각집단의 수면시간은 정규분포를 따른다.
# 대립가설 : 각집단의 수면시간은 정규분포를 따르지 않는다.
# by(data$variable, data$variable, function.name)
# by(양적자료, 질적자료, 함수명)
# 

by(sleep$extra, sleep$group, shapiro.test)

# 두집단 모두 정규성 확인 됨.

# 2단계 : 등분산성 검정
# 귀무가설 : 등분산이다.
# 대립가설 : 이분산이다.
# var.test(data$variable ~ data$variable)
# var.test(양적자료 ~ 질적자료)

# 중위수 절대편차 : mad(data$variable)
# 중위수와 얼마나 다를까를 알려주는 값

# 표준편차
# 평균과 얼마나 다를까를 알려주는 값

# 이상치가 있을경우는 중위수가 더 다름을 잘 설명해줌
# 표준편차는 이상치에 잘 흔들린다.


# 평균의 종말 책추천
# 평균은 다름을 추정하는 지표이다.

var.test(sleep$extra ~ sleep$group)
 

by(sleep$extra, sleep$group, var)

# 등분산 만족 0.798, 79.8% 귀무, 등분산

# 3단계: 등분산이 가정된 Two sample t-test
# t.test(data$vatiable ~ data$variable,
#        alternative = ,
#       var.equal = TRUE        )

t.test(sleep$extra ~ sleep$group,
       alternative = "two.sided",
       var.equal = TRUE        )

# 유의 확률이 0.079이므로
# 유의 수준 0.05에서
# 두 수면제 제품의 수면시간에는 
# 통계적으로 유의한 차이는 없는 것으로 나타났다.

# 3단계 : 만약에
# 1단계 정규성 가정을 만족하고 2단계 등분산이 깨졌다면
# 이분산이 가정된 Two sample t-test를 사용해야 함
# t.test(data$vatiable ~ data$variable,
#        alternative = ,
#       var.equal = FASE        )

t.test(sleep$extra ~ sleep$group,
       alternative = "two.sided",
       var.equal = FALSE        )


# 만약에 1단계 정규성 가정이 깨졌다면 2단계에서 
# Wilcoxon's Rank sum test
# wilcox.test(data$variable ~ data$variable,
#              alternative = )

wilcox.test(sleep$extra ~ sleep$group,
            alternative = "two.sided")


# 참고 : 결과 추출하기
result <-  t.test(sleep$extra ~ sleep$group,
                  alternative = "two.sided",
                  var.equal = TRUE        )

str(result)

result$p.value # p.value = 유의확률
result$statistic #t 통계량 = 검정통계량
