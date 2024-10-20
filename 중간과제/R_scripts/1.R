## 필요한 패키지 설치 및 로드
install.packages("tree", "rpart", "randomForest", "nnet", "neuralnet", "httpgd")

## 0. 데이터 불러오기
data("Boston")
# 데이터 확인
str(Boston)
summary(Boston)
# 데이터 분할: 훈련용 70%, 평가용 30%
idx <- sample(1:nrow(Boston), size=nrow(Boston)*0.7, replace=F)
Boston_train <- Boston[idx,]
Boston_test <- Boston[-idx,]
dim(Boston_train) ; dim(Boston_test)
################################################################################################################
## 1. 다중회귀분석 (Multiple Linear Regression)

lm.fit <- lm(medv ~ ., data = Boston_train)
summary(lm.fit)
# 변수선택법
lm.fit2 <- step(lm.fit, method="both")
summary(lm.fit2)

# 예측 및 MSE 계산
lm.yhat2 <- predict(lm.fit2, newdata=Boston_test)
lm_mse <- mean((lm.yhat2 - Boston_test$medv)^2) # 평균제곱오차(MSE) 계산
################################################################################################################
## 2. 의사결정트리 (Decision Tree)

# tree 패키지 사용
library(tree)
tree.fit <- tree(medv ~ ., data = Boston_train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0)

# 예측 및 MSE 계산
tree.yhat <- predict(tree.fit, newdata=Boston_test)
tree_mse <- mean((tree.yhat - Boston_test$medv)^2)

# rpart 의사결정트리 분석
library(rpart)
rpart.fit <- rpart(medv ~ ., data = Boston_train)
summary(rpart.fit)
# 의사결정트리 시각화
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(rpart.fit, digits=3, type=0, extra=1, fallen.leaves=F, cex=1)

# 예측 및 MSE 계산
rpart.yhat <- predict(rpart.fit, newdata=Boston_test)
rpart_mse <- mean((rpart.yhat - Boston_test$medv)^2)
################################################################################################################
## 3. 인공 신경망 (Artificial Neural Network)

# 정규화 함수 작성하기
normalize <- function(x){ return((x-min(x))/(max(x)-min(x)))}
Boston_train_norm <- as.data.frame(sapply(Boston_train, normalize))
Boston_test_norm <- as.data.frame(sapply(Boston_test, normalize))

# nnet 함수 인공 신경망 분석
library(nnet)
nnet.fit <- nnet(medv ~ ., data=Boston_train_norm, size=5) # 인공 신경망 적합하기
nnet.yhat <- predict(nnet.fit, newdata=Boston_test_norm, type="raw") # 예측결과생성
nnet_mse <- mean((nnet.yhat - Boston_test_norm$medv)^2)

# neuralnet 함수 인공 신경망 분석
library(neuralnet)
neural.fit <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, data=Boston_train_norm, hidden=5)
neural.results <- compute(neural.fit, Boston_test_norm[1:13]) # 예측결과생성
neural.yhat <- neural.results$net.result
nn_mse <- mean((neural.yhat - Boston_test_norm$medv)^2)
################################################################################################################
## 4. 랜덤 포레스트 (randomForest)

library(randomForest)
set.seed(1)
rf.fit <- randomForest(medv ~ ., data=Boston_train, mtry=6, importance=T)
rf.fit
importance(rf.fit)
varImpPlot(rf.fit)
rf.yhat <- predict(rf.fit, newdata=Boston_test) # 평가 데이터 이용, 예측결과생성
rf_mse <- mean((rf.yhat-Boston_test$medv)^2)
################################################################################################################
## 모델 비교 (MSE)
# 역정규화 함수 정의
denormalize <- function(x, min_val, max_val){
  return(x * (max_val - min_val) + min_val)
}

# 훈련 데이터의 최소값과 최대값 계산
train_min <- apply(Boston_train, 2, min)
train_max <- apply(Boston_train, 2, max)

# nnet과 neuralnet의 예측값 역정규화
nnet.yhat_denorm <- denormalize(nnet.yhat, train_min["medv"], train_max["medv"])
nn.yhat_denorm <- denormalize(neural.yhat, train_min["medv"], train_max["medv"])

# MSE 계산
nnet_mse_denorm <- mean((nnet.yhat_denorm - Boston_test$medv)^2)
nn_mse_denorm <- mean((nn.yhat_denorm - Boston_test$medv)^2)

# 모델별 MSE 비교
cat(
  "\n모델별 MSE 비교\n",
  "다중회귀분석:", lm_mse, "\n",
  "tree 의사결정트리:", tree_mse, "\n",
  "rpart 의사결정트리:", rpart_mse, "\n",
  "nnet 인공 신경망:", nnet_mse_denorm, "\n",
  "neuralnet 인공 신경망:", nn_mse_denorm, "\n",
  "랜덤 포레스트:", rf_mse, "\n"
)
################################################################################################################
################## 랜덤 포레스트가 가장 낮은 MSE를 보여주므로 가장 높은 예측 정확도를 가지고 있다. ###################
################################################################################################################