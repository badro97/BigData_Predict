install.packages("arules")
install.packages("arulesViz", dependencies = TRUE) # install 오류로 dependencies = TRUE 옵션 설정
library(arules)
library(arulesViz)

## 데이터 준비
data(Groceries)
data(package = "arules")

# 데이터 구조 확인
Groceries
inspect(Groceries[1:10])
# 데이터 요약 통계량 확인
summary(Groceries)

# 각 아이템 빈도 확인
sort(itemFrequency(Groceries, type = "absolute"), decreasing = T)
round(sort(itemFrequency(Groceries, type = "relative"), decreasing = T), 3)

# 아이템 빈도 시각화
itemFrequencyPlot(Groceries, topN=10, type="absolute")
itemFrequencyPlot(Groceries, topN=10, type="relative")
################################################################################################################
## 연관성 분석
# 규칙 길이 분포
apriori(Groceries)
result_rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5, minlen=2))
summary(result_rules)
inspect(result_rules[1:5])

# 향상도(lift) 연관규칙
rules_lift <- sort(result_rules, by="lift", decreasing = T)
inspect(rules_lift[1:5])

# 신뢰도 기준 정렬
rules_conf <- sort(result_rules, by="confidence", decreasing = T)
inspect(rules_conf[1:5])

# 특정 아이템 부분집합
milk_rule <- subset(rules_lift, items %in% "whole milk")
milk_rule
inspect(milk_rule[1:5])
rhs.milk_rule <- subset(rules_lift, rhs %in% "whole milk")
rhs.milk_rule
inspect(milk_rule[1:5])

# apriori 인자 지정
wholemilk_rule <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5, minlen=2), appearance=list(default="lhs", rhs="whole milk"))
wholemilk_rule <- sort(wholemilk_rule, by="lift", decreasing = T)
inspect(wholemilk_rule[1:5])
################################################################################################################
## 연관 분석 결과 시각화
plot(wholemilk_rule[1:10], method="graph", measure="lift", shading="confidence")