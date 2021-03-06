## 아래 코드를 실행해서 결측치와 이상치가 삽입된 mpg 데이터를 생성한 다음 문제를 해결하세요.
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(42, 65, 124, 131, 153, 212), "cty"] <- NA
mpg[c(12, 23, 46, 111, 230), "fl"] <- c("zz", "zz", "kaj1", " ", "138")
mpg[c(86, 127, 150), "displ"] <- c(10, 13, 28)

# Q1. cty 변수에 결측치가 몇 개 있는지 알아보세요.



# Q2. cty 변수의 결측치를 제외하고, 변속기 종류(trans)별 cty 평균을 나타낸 표를 출력하세요.







# Q3. 연료 종류를 의미하는 fl 변수는 다섯 가지 값(c, d, e, p, r)으로 구성됩니다. fl 변수에 이상치가 있는지 확인한 다음, 이상치를 제외하고 연료 종류별 hwy 평균을 구하세요.

table(mpg$fl)

mpg$fl <- ifelse(mpg$fl %in% c("c", "d", "e", "p", "r"), mpg$fl,NA)

table(mpg$hwy)

filter(mpg, !is.na(fl)) %>% group_by(fl) %>% summarise(fl_mean = mean(hwy))

filter(mpg, fl %in% c("c", "d", "e", "p", "r")) %>% group_by(fl) %>% summarise(fl_mean = mean(hwy))




# Q4. 상자그림을 이용해 엔진의 배기량을 의미하는 displ 변수의 이상치 기준을 구한 후 displ 변수에 몇 개의 이상치가 있는지 알아보세요. 그런 다음 이상치를 결측 처리 하세요.









# Q5. displ 변수의 이상치를 제외하고 연료 종류별 평균 엔진 배기량을 알아보세요. 평균 엔진 배기량을 구할 때 mean() 함수의 'na.rm' 파라미터를 활용하세요.





