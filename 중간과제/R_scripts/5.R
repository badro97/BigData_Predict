library(caret)
idx <- createDataPartition(iris$Species, p=0.7, list=F)
iris_train <- iris[idx,]
iris_test <- iris[-idx,]
table(iris_train$Species)
table(iris_test$Species)


library(rpart) # 의사결정트리 기법 적용하기 위한 rpart 패키지 로드
library(e1071) # 나이브 베이즈 기법 적용하기 위한 e1071 패키지 로드
library(randomForest) # 랜덤 포레스트 기법 적용하기 위한 패키지 로드
rpart.iris <- rpart(Species ~. , data=iris_train) # 의사결정트리 머신러닝 적용
bayes.iris <- naiveBayes(Species ~. , data=iris_train) # 나이브 베이즈 머신러닝 적용
rdf.iris <- randomForest(Species ~. , data=iris_train, importance=T) # 랜덤 포레스트적용

#각 머신러닝 기법별로 예측 범주값 벡터 생성하기
rpart.pred <- predict(rpart.iris, newdata=iris_test, type="class")
bayes.pred <- predict(bayes.iris, newdata=iris_test, type="class")
rdf.pred <- predict(rdf.iris, newdata=iris_test, type="response")

# 혼동 행렬 생성
table(iris_test$Species, rpart.pred) # 의사결정트리 적용 결과 혼동 행렬
table(iris_test$Species, bayes.pred) # 나이브 베이즈 적용 결과 혼동 행렬
table(iris_test$Species, rdf.pred) # 랜덤 포레스트 적용 결과 혼동 행렬

# 모델 성능 평가지표 비교
confusionMatrix(rpart.pred, iris_test$Species, positive="versicolor")
confusionMatrix(bayes.pred, iris_test$Species, positive="versicolor")
confusionMatrix(rdf.pred, iris_test$Species, positive="versicolor")