
# --------------------------------------------------------------------------------
# ROC 커브 및 AUROC 반환 함수 생성
# --------------------------------------------------------------------------------

# 패키지를 불러옵니다. 
library(tidyverse)
library(caret)
library(MLmetrics)

# 분류모형의 ROC 커브 및 AUROC를 반환하는 사용자 정의 함수를 생성합니다. 
getROC <- function(real, pred) {
  
  # ROC 커브에 필요한 패키지를 불러옵니다. 
  library(ROCR)
  
  # pred와 real이 범주형일 수 있으므로 숫자형 벡터로 변환합니다. 
  pred <- pred %>% as.numeric()
  real <- real %>% as.numeric()
  
  # ROC 커브를 그리기 위해 prediction object를 생성합니다. 
  # pred와 real이 범주형일 수 있으므로 숫자형 벡터로 변환합니다. 
  predObj <- prediction(predictions = pred, 
                        labels = real)
  
  # predObj 객체를 활용하여 performance 객체를 생성합니다. 
  perform <- performance(prediction.obj = predObj, 
                         measure = 'tpr', 
                         x.measure = 'fpr')
  
  # ROC 커브를 그립니다. 
  plot(x = perform, main = 'ROC curve')
  
  # 편의상 왼쪽 아래 모서리에서 오른쪽 위 모서리를 잇는 대각선을 추가합니다. 
  lines(x = c(0, 1), y = c(0, 1), col = 'red', lty = 2)
  
  
  # AUROC에 필요한 패키지를 불러옵니다. 
  library(pROC)
  
  # AUROC를 계산합니다. 
  auroc <- auc(real, pred)
  print(x = auroc)
  
  # AUROC를 ROC 그래프 오른쪽 아래에 추가합니다. 
  text(x = 0.9, 
       y = 0, 
       labels = str_c('AUROC : ', round(x = auroc, digits = 4L)), 
       col = 'red', 
       font = 2)
  
}


## End of Document
