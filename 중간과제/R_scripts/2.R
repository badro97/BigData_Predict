## 필요한 패키지 설치 및 로드
install.packages("ggplot2", "cluster", "dplyr", "caret")

## 1. 데이터 준비
# iris 데이터셋 로드
data(iris)

# 데이터 확인
str(iris)
summary(iris)

# 목표변수 (Species) 제외
iris2 <- iris[,1:4]

km.out.withness <- c()
km.out.between <- c()
################################################################################################################
## 2. 군집수를 k=2~7까지 변화시켜가며 클러스터링 시행
for (i in 2:7) {
    set.seed(1)
    km.out <- kmeans(iris2, centers=i)
    km.out.withness[i-1] <- km.out$tot.withinss     # 군집 내 제곱합 저장
    km.out.between[i-1] <- km.out$betweenss         # 군집 간 제곱합 저장
}
data.frame(km.out.withness, km.out.between)

km.out.k3 <- kmeans(iris2, center=3)
km.out.k3$centers # 각 군집의 중심점 출력
km.out.k3$cluster # 각 관측지의 할당된 군집번호 출력
km.out.k3$size # 각 군집의 데이터 관측치 개수 출력
table(km.out.k3$cluster, iris$Species) # 군집결과와 원래 품종 개수 비교

## k-means 클러스터링 결과 시각화
plot(iris2[,1:2], col=km.out.k3$cluster, pch=ifelse(km.out.k3$cluster==1, 16, ifelse(km.out.k3$cluster==2, 17, 18)), cex=2) ; points(km.out.k3$centers, col=1:3, pch=16:18, cex=5)
################################################################################################################