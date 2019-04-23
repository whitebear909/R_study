
# --------------------------------------------------------------------------------
# 실제값과 추정값을 할당하면 각종 성능 지표를 반환하는 함수를 생성합니다. 
# --------------------------------------------------------------------------------

# 패키지를 불러옵니다. 
library(tidyverse)


# 회귀모형의 에러를 계산하는 사용자 정의 함수를 생성합니다. 
getErrors <- function(real, pred) {
  
  # 오차를 계산합니다. 
  error <- real - pred
  
  # MSE를 계산합니다. 
  MSE <- (error)^2 %>% mean() 
  # cat('1. MSE  :', MSE, '\n')
  
  # RMSE를 계산합니다.  
  RMSE <- (error)^2 %>% mean() %>% sqrt()
  # cat('2. RMSE :', RMSE, '\n')
  
  # MAE를 계산합니다. 
  MAE <- error %>% abs() %>% mean()
  # cat('3. MAE  :', MAE, '\n')
  
  # MAPE를 계산합니다. 
  MAPE <- ((error %>% abs()) / (real %>% abs())) %>% mean()
  # cat('4. MAPE :', MAPE, '\n')
  
  # 하나의 데이터프레임으로 저장합니다. 
  errors <- data.frame(MSE, RMSE, MAE, MAPE)
  
  # 결과를 반환합니다. 
  return(errors)
  
}

