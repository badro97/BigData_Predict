## 필요한 패키지 설치 및 로드
install.packages(c("psych","corrplot","dplyr","car","ggplot2","dplyr"))
library(psych)
library(corrplot)
library(car)
library(ggplot2)
library(dplyr)


## 데이터 불러오기
data <- read.csv("전반적 만족데이터.csv", header = TRUE)

## 데이터 구조 확인
str(data)
summary(data)


## 결측값 확인
colSums(is.na(data))

## 결측값을 중앙값으로 대체
data["나이별"] <- lapply(data["나이별"], function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
})
summary(data)

## 기술통계 분석
describe(data)

## 상관관계 분석 (만족도 기준)
cor_matrix <- cor(data)
correlation_matrix <- cor(data[,c("교육서비스","행정서비스","물리적환경",
                                  "학과발전가능성","취업요인","전반적만족")])
print(cor_matrix)
print(correlation_matrix)

## 상관관계 히트맵
corrplot(cor_matrix, method="color", tl.col="black", tl.srt=45, addCoef.col="black")
corrplot(correlation_matrix, method="color", type ="upper", 
         tl.col="black", tl.srt=45, addCoef.col="black")



## 다중공선성 확인 (VIF)
model_vif <- lm(전반적만족 ~ 교육서비스 + 행정서비스 + 물리적환경 + 
                  학과발전가능성 + 취업요인, data = data)
vif(model_vif)



## 다중회귀분석
model <- lm(전반적만족 ~ 교육서비스 + 행정서비스 + 물리적환경 + 
              학과발전가능성 + 취업요인, data = data)

## 회귀분석 결과 출력
summary(model)

## 모델의 회귀계수 시각화
coefficients <- data.frame(
  Variable = names(model$coefficients), 
  Estimate = model$coefficients
) %>% filter(Variable != "(Intercept)")  # Intercept 제외
print(coefficients)


# 회귀 계수 시각화 with 수치 레이블
ggplot(coefficients, aes(x = Variable, y = Estimate, fill = Estimate > 0)) + 
  geom_bar(stat = "identity") + 
  # 양수인 경우 레이블을 막대 오른쪽에 추가
  geom_text(data = subset(coefficients, Estimate > 0),
            aes(label = round(Estimate, 2)),
            hjust = -0.1, # 막대 오른쪽에 배치
            color = "black") +
  # 음수인 경우 레이블을 막대 왼쪽에 추가
  geom_text(data = subset(coefficients, Estimate <= 0),
            aes(label = round(Estimate, 2)),
            hjust = 1.1, # 막대 왼쪽에 배치
            color = "black") +
  theme_minimal() + 
  coord_flip() + 
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  labs(title = "다중회귀분석 회귀계수 시각화", x = "변수", y = "예측 계수") +
  theme(legend.position = "none") +
  # y축 확장하여 레이블이 막대 밖에 표시될 공간 확보
  expand_limits(y = c(min(coefficients$Estimate) - 0.1, max(coefficients$Estimate) + 0.1))
