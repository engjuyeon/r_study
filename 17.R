# 연관 분석 : 범주형 데이터 분석, 비지도학습
# -> 통계적 학습 방법
# 지지도support 가지치기의 기준 (의미 있는 관계인지) 중요한 것을 알 수 있다.
# 신뢰도confidence 조건부확률 (유용한 규칙일 가능성이 높다)
# 향상도lift(상관관계) (1를 기준) 향상도가 1에 가까우면 두 상품이 독립 / 1보다 작으면 음의 상관성 / 1보다 크면 양의 상관성


# 연관 분석 순서 #
# itemset 생성
# 매트릭스, 리스트, 데이터프레임 => transaction 행렬(transaction matrix)로 변환
# apriori알고리즘으로 support, confidence, lift를 계산하여 filtering
# subset으로 조건 충족하는 가설 만족 확인
# inspect를 이용한 itemset확인

# 탐색적 기법 / 범주형 변수의 조합(lhs -> x, rhs -> y)
# count
# 품목 수가 증가하면 계산은 기하급수적 증가하는 단점이 있다.

################################################################################ 실습 1

library(arules)
a_matrix <- matrix(c(1,1,1,0,0,
                     1,1,0,0,0,
                     1,1,0,1,0,
                     0,0,1,0,1,
                     1,1,0,1,1), ncol = 5)
dimnames(a_matrix) <- list(c("a", "b", "c", "d", "e"),
                           paste("Tr", c(1:5), sep = ""))
a_matrix

#
trans2 <- as(a_matrix, "transactions") # as는 arules 패키지 함수 # transaction으로 변환
trans2
inspect(trans2) # 열로 저장이 됨

# file loading
tran <- read.transactions("tran.txt", format = "basket", sep = ",")
tran
inspect(tran)
# supp :지지도, conf : 신뢰도 # 설정 값 이상인 것만 나옴
rule <- apriori(tran, parameter = list(supp = 0.3, conf = 0.1)) # 16건의 rule(s) 
rule
inspect(rule)
rule2 <- apriori(tran, parameter = list(supp = 0.1, conf = 0.1)) # 35건의 rule(s)
inspect(rule2)
rule3 <- apriori(tran, parameter = list(supp = 0.1, conf = 0.8)) # 6건의 rule(s)
inspect(rule3)

################################################################################ 실습 2

data(Groceries)
str(Groceries) # 행 : 거래, 열 :  식품
Groceries # class 4형식
summary(Groceries)
inspect(head(Groceries,3))
size(head(Groceries))
LIST(head(Groceries, 3)) # transaction 구성을 list형태로 변환
itemFrequencyPlot(Groceries, topN = 15) # itemFrequency를 상대도수로 표현
itemFrequencyPlot(Groceries, topN = 15, type = "absolute") # 실제개수로 표현
gdf <- as(Groceries, 'data.frame') # transacion 구성을 data.frame형태로 변환
head(gdf)
inspect(Groceries[1:6])

# 1~200 중에 item size가 4보다 큰 경우를 출력
inspect(subset(Groceries[1:200], size(Groceries[1:200])>4)) # 65건
inspect(subset(Groceries[1:200], size(Groceries[1:200])>6)) # 36건

# 열 품목 확인하기
itemFrequency(Groceries[, 1:5]) # 5번째까지의 열 품목
itemFrequency(Groceries[, 1:15]) # 15번째까지의 열 품목

itemFrequencyPlot(Groceries, supp = 0.1) # support가 0.1보다 큰 것들

wholemilk_rules <- apriori(data = Groceries, parameter = list(supp= 0.001, conf = 0.08),
                           appearance = list(rhs = "whole milk")) 
# rhs(right hand set)이 whole milk인 경우 # 3765건의 rule(s)

inspect(wholemilk_rules)

arules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.08, minlen = 1))
inspect(arules)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5))
inspect(rules)
rules_conf <- sort(rules, by="conf" , decreasing = T) # 신뢰도 기준으로 정렬
inspect(head(rules_conf))

butter <- subset(arules, lhs %in% 'butter')
inspect(butter)
# install.packages("arulesViz")
library(arulesViz)
plot(arules, method = "graph", control = list(type = "items"))
plot(arules[1:5])
plot(arules[1:5], method = "graph", engine = 'interactive')
plot(arules[1:15], method = "graph", engine = 'interactive')

subrules2 <- head(sort(arules, by="lift"), 10)
inspect(subrules2)
plot(subrules2, method = "graph")
plot(subrules2,  method = "graph", engine = 'interactive')
plot(rules[1:15], method = "matrix", engine = '3d')
plot(subrules2, method = "matrix")

amilk <- subset(arules, lhs %in% 'whole milk')
inspect(amilk)
berries <-  subset(arules, lhs %in% 'berries')
inspect(berries)
beryog <-  subset(arules, lhs %in% c('berries','yogurt'))
inspect(beryog)

wholemilk <- subset(arules, rhs %in% 'whole milk')
inspect(wholemilk)
plot(wholemilk, method = "graph")

rules <- apriori(Groceries, parameter= list(supp = 0.001, conf = 0.3), appearance = list(rhs = "sugar"))
inspect(rules)
inspect(head(sort(rules, decreasing = T, by = "lift"),100))
plot(rules, method = "graph", engine = 'interactive')

################################################################################ 문제 1
data(Adult)
summary(Adult)
str(Adult)
inspect(head(Adult))
# 최소 supp = 0.5 , conf = 0.8 지정하여 연관규칙 생성
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.7)) # 115건
inspect(rules)
# 연관규칙 분석 결과를 연관 네트워크 형태로 시각화하시오
plot(rules, method = "graph", engine = 'interactive')
# lhs가 white인 규칙만 subset으로 작성하고 연관어를 시각화 하시오
whiterules <- subset(rules, lhs %in% 'race=White')
inspect(whiterules)
plot(whiterules, method = "graph", engine = 'interactive')
# lhs가 백인이거나 미국인을 대상으로 subset 작성하고 연관어 시각화하시오
whiteAmerules <- subset(rules, lhs %in% c('race=White','native-country=United-States'))
inspect(whiteAmerules)
plot(whiteAmerules, method = "graph", engine = 'interactive')
# rhs가 husband인 단어를 포함한 규칙을 subset으로 작성하고 연관어를 시각화하시오
rules <- apriori(Adult, parameter = list(supp = 0.3, conf = 0.8))
husbandrules <- subset(rules, rhs %in% 'relationship=Husband')
inspect(husbandrules)
plot(husbandrules, method = "graph", engine = 'interactive') # 색 진하기 conf, 동그라미 크기 supp
# 결과를 support, confidence 를 기준으로 내림차순 정렬한 다음 상위 3개만 출력하시오
inspect(head(sort(husbandrules, decreasing = T, by = "lift"),3))


plot(rules) # 산포도
plot(rules, method = "group")



################################################################################ 실습 3


data("AdultUCI")
str(AdultUCI)
# 연관분석은 data.fame -> transaction matrix 범주형에 대하여 분석()

# 연속형 변수를 번주화하여 연관분석에 참여시키기
# as.factor : 범주형 변수로
# ordered : 순서가 있는 범주형 변수로
# cut 데이터 분할
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)), # 값 들 사이를 컷해서 labels 별로 입력 
                             labels = c("young","middle", "senior", "old"))

AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], c(0,25,40,60,168)),
                             labels = c("part-time","full-time", "over-time", "workaholic"))

AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]], 
                                          c(-Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]]>0]), Inf)),
                                        labels = c("None", "Low", "High"))

AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]],
                                          c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]]>0]), Inf)),
                                      labels = c("None", "Low", "High"))

AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL
str(AdultUCI)

library(arules)
adult_new <- as(AdultUCI, "transactions")
str(adult_new)
summary(adult_new)
basket_rules <- apriori(adult_new, parameter= list(supp = 0.08, conf = 0.5, target = "rules"))
inspect(basket_rules[1:10])

p <- inspectDT(basket_rules)
htmlwidgets::saveWidget(p, "arules_2.html", selfcontained = F)
browseURL("arules_2.html")

############################################################################### 문제 2
# 가족관계(relationship) 및 교육수준(education)의 소득(income)과의 연관성 확인

# rule <- subset(basket_rules, items %pin% "relationship" & items %pin% "education" & items %pin% "income")
# inspect(rule)
# plot(rule, method = "graph", engine = 'interactive') # 색 진하기 conf, 동그라미 크기 supp


rule <- subset(basket_rules, lhs %in% "income=small" & lift > 1.1)
inspect(rule)
p <- inspectDT(rule)
print(p)
plot(rule, method = "graph", engine = 'interactive') # 색 진하기 conf, 동그라미 크기 supp
plot(rule, method = "graph", engine = 'visNetwork')
plot(rule, method = "graph", engine = 'htmlwidget')

# 주당 일하는 시간과 소득과의 관계 확인
rule <- subset(basket_rules, lhs %pin% "hours-per-week" & rhs %pin% "income")
inspect(rule)
attributes(rule)
plot(rule, method = "graph", engine = 'interactive') # 색 진하기 conf, 동그라미 크기 supp

inspect(sort(rule, by = "lift")[1:50])

# 기타 위의 연관분석 결과로 출력된 rule로부터 자기가 주장하고자 하는 내용의 가설 세우고 증거검증


################################################################################ 실습 4

# 연관어 분석 / 텍스트 마이닝 / 네트워크 분석을 결합
install.packages("KoNLP")
library(KoNLP)
library(arules)
library(igraph)
library(combinat)

f <- file("tax.txt", encoding = "UTF-8")
f
fl <- readLines(f)
fl
close(f)
head(fl)

tran <- Map(extractNoun, fl)

Sys.setenv("JAVA_HOME" = "C://Program Files//Java//jdk-11.0.11")

install.packages("multilinguer")

library(multilinguer)
multilinguer::install_jdk()

install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools","bit","rex","lazyeval",
                   "htmlwidgets","crosstalk","promises","later","sessioninfo","xopen",
                   "bit64","blob","DBI","memoise","plogr","covr","DT","rcmdcheck","rversions"), type = "binary")

install.packages("remotes")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))

library(KoNLP)

############################################################################### 실습5
# 주성분 분석 BIAS(줄이기 위해 변수를 늘리고) / variance(변수를 줄여햐 함) tradeoff관계
# 시각화 : 2~3개의 변수로 데이터를 표현
# 수학적 : 내적이 0이면 직교
str(mtcars)
head(mtcars, 10)
# cor:T 상관계수행렬을 만들고, 공분산 행렬
# 11개의 축이 새로 생성
fit <- princomp(mtcars, cor = TRUE) # principal components analysis
summary(fit)
loadings(fit)
plot(fit, type = "lines")
# 새로운 축 4개가 선정이 되면 모든 데이터는 축에 투영이 되어 재표현
# 새로운 축에 재명명을 해주어야 함
fit$scores
fit$scores[,1:3]
fit$scores[,1:4]
biplot(fit)

















