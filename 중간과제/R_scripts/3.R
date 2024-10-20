## 데이터 준비
# USArrests 데이터셋 로드
data("USArrests")
# 데이터 구조 확인
str(USArrests)
# 데이터 요약 통계량 확인
summary(USArrests)

## princomp 함수로 주성분 분석 실시
pc1 <- princomp(USArrests, cor=T)
summary(pc1)
plot(pc1)

pc1$center # 평균
pc1$scale # 표준편차
pc1$loadings # 가중치

pc1$scores # 주성분 점수

## 주성분 점수 산포도 (Z1, Z2)
plot(pc1$scores[,1], pc1$scores[,2], xlab="Z1", ylab="Z2")
abline(v=0, h=0, col="gray")

## 행렬도 (Z1, Z2)
biplot(pc1, cex=0.7)
abline(v=0, h=0, col="gray")
