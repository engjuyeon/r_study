library("dplyr")
?ToothGrowth
str(ToothGrowth)
my_data <- ToothGrowth
sample_n(my_data, 10)
# 등분산성 테스트: 귀무가설이 등분산성이다. 대립가설 등분산성이 아니다.
# F-value = 분산비 = 그룹간분산비/ 그룹내분산비
res.ftest <- var.test(len ~ supp, data = my_data)
res.ftest
# p-value = 0.2331 유의 수준 : 0.05
# 귀무가설을 기각하지 못한다.
res.ftest$estimate
res.ftest$p.value

# var.test 2개 변수. bartlett.test() 그룹 3개 이상
a = c(175, 168,168,190,156,181,182,175,174,179)
b = c(185,169,173,173,188,186,175,174,179,180)
# 등분산성 테스트
abdata <- data.frame(a,b)
str(abdata)
ftest <- var.test(a, b, data = abdata)
ftest <- var.test(a, b)
ftest

# 다음 모집단은 평균이 75 단일표본 평균분석하시오
x = c(65,78,88,55,48,95,66,57,79,81)
t.test(x, mu=75) # 모집단의 평균균
# 평균 71.2
# t값 = (두평균의차이)/(두평균의표준오차를더한값)
# 표준오차 = 표준편차 / sqrt(n)
# 검정통계량 : p-value = 0.4537
# 평균분석의 귀무가설은 평균이 동일하다. 대립가설 평균이 동일하지 않다.
# 귀무가설 기각하지 못한다. 두 집단의 평균은 일치한다.


# sleep데이터로 평균검정 (수면제 투약 집단, 수면제 비투약 집단)
str(sleep)
?sleep
plot(extra ~ group, data = sleep)
with(sleep, t.test(extra[group == 1], extra[group == 2])) # 집단끼리의 평균검정
# 귀무가설 : 두 집단의 평균이 동일하다.
# 대립가설 : 두 집단의 평균이 동일하지 않다.
# formula를 이용한 경우
t.test(extra ~ group, data = sleep)
# 검정통계량 # p-vqlue = 0.07939 -> 0.05 넘으면 기각X
# 귀무가설을 기각하지 못한다. 수면제는 도움이 안된다.

# 남학생, 여학생 영어 성적에 대해 평균에 차이 존재하니?
boy <- c(46,47,58,47,27,58,56,26,47,25)
girl <- c(78,57,31,28,67,77,36,57,36,57)
x <- var.test(boy,girl)
x$p.value
# p-value = 0.306 귀무가설 기각하지 못한다. 분산이 동일하다.
t.test(boy, girl, var.equal = T)
# 검정통계량 : p-value = 0.5442
# 귀무가설 : 평균동일
# 대립가설 : 평균동일하지않다.
# 귀무가설 기각하지 못한다.
# 평균동일

shapiro.test(rnorm(1000))
set.seed(450)
x <- runif(50, min = 2,max = 4) # 균등분포
shapiro.test(x)
# 정규분포검정
# 귀무가설 : 정규분포이다
# 대립가설 : 정규분포아니다
# 검정통계량 : p-value = 0.5606
# 귀무가설 기각 못하여 정규분포
# p-value = 0.003259 <- 귀무가설 기각하고 대립가설 채택, 정규분포 아니다.

x <- rnorm(1000, 5.0, 0.5)
t.test(x, mu=5.2, alter="two.side", conf.level = 0.95) # 양쪽 (0.025 0.025)  p-value < 2.2e-16
(result <- t.test(x, mu=5.2, alter="greater", conf.level = 0.95) ) # 오른쪽만 (0.05) p-value 1
(result <- t.test(x, mu=5.2, alter="less", conf.level = 0.95) ) # 왼쪽만 (0.05) p-value < 2.2e-16

# 가설 : 폐암발병률이 20%이다.(귀무가설) 이를 검정
set.seed(200)
(lung <- runif(1000, min=10, max=25))
t.test(lung, mu=20, conf.level=.95)
# p-value < 2.2e-16
# 귀무가설 기각

# 동일한 사람에 대하여 수면제를 투약 (연구자만 알 수 있는 정보) : 대응표본
str(sleep)
?sleep
plot(extra ~ group, data= sleep)
with(sleep, t.test(extra[group == 1], extra[group == 2])) # p-value = 0.07939
with(sleep, t.test(extra[group == 1], extra[group == 2], paired=T)) # p-value = 0.002833

sleep2 <- sleep[, -3] # 전체에 -3번째 라인을 뺴고
tapply(sleep2$extra, sleep2$group, mean)
var.test(extra~group,sleep2) # p-value = 0.7427
t.test(extra~group, data = sleep2, paired=T, var.equal=T) # 0.002833

# 문제
# 새로운 당뇨병 치료제 개발하는 제약사
# 치료에 영향을 주는 외부요인 통제하기 위해 10명의 당뇨병환자 선별하여 1달 동안
# 위약 placebo를 투여한 기간의 혈당(Xi) 신약 투여한 1달 기간의 혈당(Yi)
# 측정하여 짝을 이루어 혈당 차이를 유의 수준 5%에서 비교
x1 <- c(51.4, 58.0, 45.5, 55.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)

shapiro.test(x1) # 정규분포임
shapiro.test(x2) # 정규분포가 아님 # wilcox.test() 해줘야함(비모수분석)
# t.test() 모수분석
var.test(x1, x2) # 동분산성

wilcox.test(x1, x2,
            alternative = c("greater"),
            paired=T,
            var.equal = T,
            conf.int = F,
            exact = F, # Warning message cannot compute exact p-value with ties일 때
            conf.level = 0.95)# paired => 문제에서 짝을 이루어 # CONF.LEVEL => 유의수준
# p-value = 0.0062
# 귀무가설 : 신약의 효과가 없다. # 증명하고자 하는 가설의 반대
# 대립가설 : 신약의 효과가 있다.
# 귀무가설 기각 .!!!!! 신약의 효과가 있다.


# 문제
# MASS 패키지에 내장된 Cars93 데이터 프레임의 Price와 Origin
# 데이터에서 생산국이 USA vs, non-USA 2개의 group에 대해서 차 가격의 평균 차이가 있는지 검정해보시오
# 원산지별 가격차이가 있는가

# 1) 가설설정
#             귀무가설(H0) : 원산지 별 평균 가격 차이가 없다.
#             대립가설(H1) : 원산지 별 평균 가격 차이가 있다.
# 2) 연구환경
#             원산지 별 차 가격 평균 차이가 있는지 검정하기 위해
#             총 93개의 USA에서 생산된 차 가격과 USA에서 생산되지 않은 차 데이터를 이용해 검정을 실시한다.
#
# 3) 유의수준
#             0.05
# 4) 분석방법
#             비모수 평균 검정
# 5) 검정통계량
#             W = 1024.5
# 6) 유의확률
#             p-value = 0.6724
# 7) 결과해석
#             p-value 값 0.6724이 유의수준인 0.05보다 크므로 귀무가설을 기각할 수 없다. 따라서
#             원산지에 따라 차의 평균 가격은 차이가 없다는 것을 알 수 있다. 외국생산이나 국내생산이나
#             소비자를 동등하게 대하고 있는 것으로 분석된다.

library(MASS)
str(Cars93)
Cars93

class(Cars93$Price)
class(Cars93$Origin)
levels(Cars93$Origin) # factor 값 확인
table(Cars93$Origin) # 데이터가 한 값에 치우쳐져 있지 않다.
with(Cars93, tapply(Price, Origin, summary))
boxplot(Price ~ Origin, data = Cars93, main = " 원산지별 차 가격", xlab = "원산지", ylab = "가격") # 이상치 확인



data <- data.frame(Origin, Price)
dataUSA <- data %>%
  filter(Origin == "USA")
dataNonUSA <- data %>%
  filter(Origin == "non-USA")

# data$Price[Origin == "USA"], data$Price[Origin == "non-USA"]

# 정규분포 테스트
# 귀무가설 : 정규분포이다.
# 대립가설 : 정규분포가 아니다.
shapiro.test(dataUSA$Price) # p-value = 0.0002006 귀무가설 기각, 정규분포를 따르지 않는다.
shapiro.test(dataNonUSA$Price) # p-value = 0.0002036 귀무가설 기각, 정규분포를 따르지 않는다.
# 다른 방법 : with(Cars93, tapply(Price, Origin, shapiro.test))

# 등분산성 테스트
# 귀무가설 : 분산이 같다.
# 대립가설 : 분산이 다르다.
var.test(dataUSA$Price, dataNonUSA$Price) # p-value 0.01387 귀무가설 기각.분산 동일하지 않다.

# var.test(Price ~ Origin, data = Cars93)

# 비모수 independent 2 samples : Wilcoxon rank sum test
wilcox.test(dataUSA$Price, dataNonUSA$Price, var.equal = FALSE, exact = F, conf.level = 0.95)
# wilcox.test(Price ~ Origin, data = Cars93, alternative = c("two.sided"),
#              var.equal = F, exact = F, conf.level = 0.95)
# p-value 0.6724 귀무가설 기각할 수 없다.
# 원산지 별 가격 평균 차이 없다.

n <- 1000
x <- rnorm(n, mean = 100, sd = 10)
length(x)
hist(x)
shapiro.test(x)
t.test(x,mu=95) # 단일 표본 평균 검정
t.test(x, mu=99.8, conf.level = 0.95)
t.test(x, mu=99.8, conf.level = 0.99)

data <- read.csv("one_sample.csv", header = TRUE)
head(data)
x <- data$survey
summary(x)
length(x)
table(x) # 불만족 : 0, 만족 : 1
install.packages("prettyR")
library(prettyR)
# 비율검정 binom.test
binom.test(c(14,136), p = 0.2) # 예상 : 불만족이 20%
# binom의 귀무가설 : 비율 차이가 나지 않는다.
# 대립가설 : 비율 차이가 난다.


x <- data$time
head(x)
summary(x) # na처리 없이도 결과 보여줌

mean(x, na.rm = T)
x1 <- na.omit(x)
x1

mean(x1)
shapiro.test(x1)
t.test(x1, mu=5.2) # 일주일에 노동시간(40)
t.test(x1, mu=5.2, alternative ="two.sided", conf.level = 0.95) # 기본값이 양측검정(two.sided), 신괴구간 95%

# 유의 수준 0.05 결정
# 방향성이 있는 연구 가설
t.test(x1, mu=5.2, alternative = "greater", conf.level = 0.95)

# two_sample
data <- read.csv("two_sample.csv", header = TRUE)
data
head(data)
dataset <- data[c('method', 'score')]
table(dataset$method)

method1 <- subset(dataset, method==1)
method2 <- subset(dataset, method==2)
(method1_score<-method1$score)
(method2_score<-method2$score)

var.test(method1_score, method2_score) # 등분산성

t.test(method1_score, method2_score, var.equal=T)# 두 집단에는 평균차이가 있다.

# method1_score, method2_score 두 방법 중에서 방법1이 방법2보다 큰 경우만 고려하면 된다면
# 방향성 있는 연구가설이 된다.
t.test(method1_score, method2_score,alter = "greater", var.equal=T)


data <- read.csv("paired_sample.csv", header=TRUE)
str(data)
head(data)
# -- 1) 데이터 정제
dataset = data[ c('before',  'after')]
dataset

# -- 2) 적용전과 적용후 분리
before = dataset$before                                                # 교수법 적용전 점수
after = dataset$after                                                  # 교수법 적용후 점수


# -- 3) 기술통계량 
length(before)                                                            # 100
length(after)                                                             # 100
mean(before)                                                              # 5.145
mean(after, na.rm = T)                                                    # 6.220833 -> 1.052  정도 증가

# ------------------------------
# -- 3. 분포모양 검정
# ------------------------------
var.test(before, after, paired=TRUE) 
# 동질성 분포 : t.test()
# 비동질성 분포 : wilcox.test()

# ------------------------------
# -- 4. 가설검정
# ------------------------------
t.test(before, after, paired=TRUE, var.equal =T)                                        # p-value < 2.2e-16 
# 유의미한 변화가 있었다.


# 2집단 이상인 경우 F-검정 : ANOVA 분석
# bartlett.test(), 
# 모수인 경우 aov(), 비모수인 경우 kruskal.test()

# 6개의 SPRAY회사
data("InsectSprays")
attach(InsectSprays)
str(InsectSprays)
xtabs(InsectSprays)
with(InsectSprays, mean(count[spray == 'A']))
with(InsectSprays, tapply(count, spray, mean))
with(InsectSprays, tapply(count, spray, var)) # 안정
with(InsectSprays, tapply(count, spray, length))
with(InsectSprays, boxplot(count~spray))

# 연속형, 범주형 간의 관계
# 연속형 변수, 범주형 변수
aov.test = aov(count ~ spray, data = InsectSprays)
aov.test
summary(aov.test) # aov는 summary 꼭
# * signif. codes <- p값 0 근처 *** ***
# 귀무가설 : 분산비가 동일하다.
# 대립가설 : 분산비가 동일하지 않다.
# 0.05보다 작으므로 귀무가설기각하고 대립가설이 채택된다 : 유의미한 차이가 있다.

TukeyHSD(aov.test)

# 위 anova test는 선형회귀와 비슷하다



bartlett.test(InsectSprays$count, InsectSprays$spray)
# 귀무가설 : 등분산
# 대립가설 : 등분산이 아니다.
# p-value = 9.085e-05
# 귀무가설을 기각하고 대립가설을 채택

kr <- kruskal.test(count ~spray, data =InsectSprays)
kr
# p-value = 1.511e-10
# 귀무가설을 기각하고 대립가설 채택한다.
1661018

#PlantGrowth 데이터의 group별로 weight 의 평균차이가 있는지
data("PlantGrowth")
str(PlantGrowth)
levels(PlantGrowth$group)
# 귀무가설 : 평균 차이가 없다.
# 대립가설 : 평균 차이가 있다.
summary(PlantGrowth)
# 정규분포 테스트
# 귀무가설 : 정규분포이다.
# 대립가설 : 정규분포가 아니다.
with(PlantGrowth, tapply(weight, group, shapiro.test)) # 정규분포이다.

with(PlantGrowth, tapply(weight, group, mean))
with(PlantGrowth, tapply(weight, group, var))
with(PlantGrowth, bartlett.test(weight ~ group)) # p-value = 0.2371
aov.Ptest = aov(weight ~ group, data = PlantGrowth)
aov.Ptest
summary(aov.Ptest) # aov는 summary 꼭
# p-value 0.0159  귀무가설 기각,  평균차이가 있다.
TukeyHSD(aov.Ptest) # 사후 평가 그룹 간 관계(p adj) 0.012(차이가 있다.)
summary.lm(aov.Ptest) # 회귀분석  p-value 
plot(aov.Ptest)

install.packages("DBI")
install.packages("RMySQL")

library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "rstudiodb",
                 user = "root", password = "0431", port = 3308)

data <- read.csv("three_sample.csv")
data
str(data)
summary(data)
threeData <- subset(data, !is.na(score), c(method, score))
str(threeData)

write.csv(threeData, file = "threedata.csv", row.names = F)

threedb <- read.csv("threedata.csv", header = T)
dbWriteTable(con, "threeData", threedb, row.names = F)

summary(threeData)
View(threeData)
boxplot(threeData$score)$stats;
which(threeData$score>summary(threeData$score)[5] + 1.5*IQR(threeData$score))
threeData2 <- threeData[-which(threeData$score>summary(threeData$score)[5] + 1.5*IQR(threeData$score)),]
length(threeData2$score)
boxplot(threeData2$score)

threeData2$method[threeData2$method==1] = "방법1" 
threeData2$method[threeData2$method==2] = "방법2"
threeData2$method[threeData2$method==3] = "방법3"

table(threeData2$method)                         
# 동질성 테스트
bartlett.test(score ~ method, data=threeData2)       
# p-value : 0.1905 귀무가설 기각x 동질적이다.

nrow(threeData2)
sum(threeData2$score)

threeData2

aov.Ptest = aov(score ~ method, data = threeData2)
aov.Ptest
# 귀무가설 : 평균에 차이가 없다.
# 대립가설 : 평균에 차이가 있다.
summary(aov.Ptest) # aov는 summary 꼭
# p-value 9.39e-14 0.05 보다 작은 수이므로 귀무가설을 기각하고 대립가설을 채택한다.
# 평균에 차이가 있다.
# 사후검정
TukeyHSD(aov.Ptest) 
summary.lm(aov.Ptest) 
plot(aov.Ptest)



