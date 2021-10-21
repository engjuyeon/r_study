y <- c(1,2,3,8,5,3,10,7)
x1 <- c(2,4,5,6,8,10,11,13)
x2 <- c(1,2,2,4,4,4,6,6)
opar = par(mfrow = c(1,3))
plot(x1, y)
plot(x2, y)
plot(x1, x2)

summary(lm(y~x1))
summary(lm(y~x2))
summary(lm(y~x1+x2)) # 과적합 (변수를 줄여줘야함) => 설명력(Adjusted R-squared)

yhat <- -0.1041  -1.1566*x1 + 3.7267*x2
yhat
y - yhat
(PRESS <- sum((y-yhat)^2)) #PRESS잔차
(PRESS <- sum((y-yhat)^2) / sd(y)) # standadize resudual


par(mfrow=c(1,1))
str(women)
fit = lm(weight~height, data = women)
summary(fit)
( women.fitted_weight = -87.52 + 3.45 * women$height)
plot(weight ~ height, data = women)
abline(fit, col = "blue")
cor(women$height, women$weight)

# 정확도 : 연속된 수치의 비교는 상관분석을 통해 가능
cor(women$weight, women.fitted_weight) # accuracy : 0.9954948
#      실제값           예측값

x <- c(3,4,5,3,2,1,7,5)
rank(sort(x))
(m<-matrix(c(1:10,(1:10)^2), ncol=2)) # 변수가 2갱미
print(m)

# 상관계수 행렬
cor(m, method="spearman") # 순위 상관 계수
cor(m, method="pearson") # 적률 상관 계수 # 연속된 상관

# 공분산
a <- c(1:10)
b <- c((1:10)^2)
a
b

sum((a - mean(a)) * (b - mean(b))) / (length(a) - 1) # 공분산 100.8333
sum((a - mean(a)) * (b - mean(b))) / (length(a) - 1) / (sd(a) * sd(b)) # 상관계수 cor(m, method = "pearson")

# 상관 계수 행렬은 정방행렬이면서 대칭행렬
# 공분산 행렬(변수개수 x 변수개수)
cov(m)

cov(1:5, 2:6)
cov(1:5, c(3,3,3,3,3))
cov(1:5, 5:1)
str(mtcars)
cov(mtcars)
cor(mtcars)


# 상관분석
# 귀무가설 : 상호독립적이다.
# 대립가설 : 상관이 있다.
cor.test(c(1,2,3,4,5), c(1,2,3,4,5), method = "pearson")
# p-value : 2.2e-16 =>  귀무가설 기각!
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method = "pearson")
# install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(mtcars), type="pearson")

# iris
library(dplyr)
data("iris")
head(iris)
cor(iris$Sepal.Width, iris$Petal.Length)
ir <- iris %>%  select(-(Species))
cor(ir)
iris %>%  select(-(Species)) %>%cor()
symnum(cor(iris[,1:4])) # 기호로 알려줌


# install.packages("corrplot")
library(corrplot)
corrplot(cor(mtcars), method="circle") # “circle”, “square”, “ellipse”, “number”, “shade”, “color”, “pie”
corrplot(cor(mtcars), method="square")
corrplot(cor(mtcars), method="ellipse")
corrplot(cor(mtcars), method="number")
corrplot(cor(mtcars), method="shade")
corrplot(cor(mtcars), method="color")
corrplot(cor(mtcars), method="pie")


product <- read.csv("product.csv")
product
colnames(product)<-c("제품친밀도", "제품적절성", "제품만족도")
head(product)
summary(product)
sd(product$제품친밀도); sd(product$제품적절성); sd(product$제품만족도)

cor(product$제품친밀도, product$제품만족도)
cor(product, method="pearson")
# install.packages("corrgram")
library(corrgram)
corrgram(product)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(product, histogram = T, pch="+")


# 문제
# 1) airquality에 대한 상관계수를 구해보시오
data("airquality")
airquality <- na.omit(airquality)
head(airquality)
cor(airquality) # 적률 상관 계수 # 연속된 상관
chart.Correlation(airquality, histogram = T, pch="+")

# 2) 분석 대상 변수만 선택합니다 : 숫자여만 가능, 의미 없는 데이터 변수(month, day) 제거
air <- airquality %>% 
  select( -c(Month, Day))
air <- na.omit(air)

# 3) 데이터의 상관정도를 확인합니다
air_cor<- cor(air)

# 4) 상관계수 행렬을 시각화 해보시오
corrplot(cor(air), method="pie")

col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20)
heatmap(x = air_cor, col= col, symm = TRUE)
chart.Correlation(air_cor, histogram = T, pch = 19)


# 상관행렬 => 정방행렬, 대칭행렬 => 고유값 분해 => 고유치와 고유벡터 =>
# 고유벡터는 서로 내적이  0이며 직교한다. 사이즈는 1 => 축을 의미
# 고유치(영향을 미치는 정도 = 분산의 크기) 는 고유벡터의 방향으로 크기를 의미한다. 주성분 분석
# 주성분 분석을 해서 축, 차원축소를 해도 원래의 값을 띈다.


a <- c(4.0, 4.2, 3.9, 4.3, 4.1)
b <- c(2.0, 2.1, 2.0, 2.1, 2.2)
c <- c(0.60, 0.59, 0.58, 0.62, 0.63)

mat <- matrix( c(a,b,c), nrow = 5, byrow = F)
mat
cor(mat)

# 고유값 분해 eigen
cor(mat)
(eigenvector <-  eigen(cor(mat))) # 열에 대해서 작업

# 내적
eigenvector$vectors[,1] %*% eigenvector$vectors[,2] # 내적기호 %*% 0 직교 => 축을 의미
eigenvector$vectors[,1] %*% eigenvector$vectors[,3] # 0 직교 => 축을 의미
eigenvector$vectors[,2] %*% eigenvector$vectors[,3] # 0 직교 => 축을 의미


eigenvector$vectors[1,] %*% eigenvector$vectors[2,] # 0 직교
eigenvector$values # 85%를 데이터를 설명할 수 있는 주성분 선택 후 새로운 축 생성
# 새롭게 생선된 축으로 원래의 데이터 매핑 ( 정직교하는 축으로 데이터가 재표현 )

head(cars)
summary(cars)
summary(cars$speed)
plot(cars$speed, cars$dist) 
res.lm <- lm(dist~speed, data=cars) 
summary(res.lm) 
cor(cars$speed, cars$dist)  # 상관계수
cor(cars$dist, -17.5791 +3.9324 * cars$speed )  #  상관계수 == 연속형 변수


names(res.lm) 
attributes(res.lm)
(cof <-coef(res.lm)) 
cof["speed"] 
cof["(Intercept)"] 
( dist <- cof["(intercept)"] + cof["speed"] * cars$speed ) 
plot(cars$speed, cars$dist, xlab="속도", ylab="정지거리", xlim=c())
abline(coef(res.lm), col = 2)

fitted(res.lm)[1:4]
residuals(res.lm)[1:4]
deviance(res.lm)
plot(res.lm)

library(lmtest)
bptest(cars$speed ~ cars$dist) # 이분산성 테스트트
# 귀무가설은 등분산성이다. 대립가설은 이분산성이다.
# 귀무가설 기각할 수 없다. 등분산성이다.

dwtest(res.lm) # 2이면 자기 상관이 없다.
# DW = 1.6762 자기 상관이 없는 편에 속한다.
# 2~4 음의 자기상관, 0~2 사이는 양의 자기상관

# 예측
predict(res.lm, newdata = data.frame(speed=10)) # 점추정
predict(res.lm, newdata = data.frame(speed=10), interval = "confidence") # 95% 신뢰구간, 구간추정
predict(res.lm, newdata = data.frame(speed=10), interval = "prediction") # 변동성을 고려, 실시간 추정
# 역수인 경우 -
predict(res.lm, newdata = data.frame(speed=seq(40.0, 25.0, -.21)), interval="confidence")
par(mfrow=c(2,2))
plot(res.lm)

# age, sex, bmi, children, smoker, region, charges
# 종속변수 charges : 의료비
#회귀분석실시

# 1 데이터세트 가져오기
insu <- read.csv("insurance.csv", header = T, stringsAsFactors = F)
str(insu)
insu$sex <- as.factor(insu$sex)
insu$smoker <- as.factor(insu$smoker) # 범주형
insu$region <- as.factor(insu$region) # 범주형
levels(insu$region)
summary(insu)
insu2 <- insu[, c(1,3:7)] # domain knowledge가 없어서 sex 제외하고 다 넣었음

# 2 회귀모델 생성
insu.lm <- lm(charges ~ ., data = insu2)
insu.lm
# 흡연자인 경우 : 23836.3, 지역에 따라서는 -, 감소하고 있다.
# 나오지 않은 값은 (Intercept)에 영향을 미친다. but 불명확함.

a <- c('yes', 'no')
summary(insu.lm)
insu2$smoker <- as.factor(insu2$smoker, levels = a)

head(insu2)
str(insu2$smoker)
isu.lm <- lm(charges ~., data = insu2)
isu.lm
summary(isu.lm)
# 개수, 스모크 영향ㅇ이 있따.
region1 <- numeric()
region2 <- numeric()
region3 <- numeric()

# 3) 더미변수화
idx <- 1
for(re in insu$region){
  if(re == "northeast"){
    region1[idx] <- 0
    region2[idx] <- 0
    region3[idx] <- 0
  }else if(re == "northwest"){
    region1[idx] <- 1
    region2[idx] <- 0
    region3[idx] <- 0
  }else if(re == "southeast"){
    region1[idx] <- 0
    region2[idx] <- 1
    region3[idx] <- 0
  }else if(re == "southwest"){
    region1[idx] <- 0
    region2[idx] <- 0
    region3[idx] <- 1
  }
idx = idx + 1
}


table(region1) # 325 northwest
table(region2) # 236
table(region3)

tot <- nrow(insu2)
tot <- (table(region1)[2] + table(region2)[2] + table(region3)[2])
# 파생변수
insu$region1 <- region1
insu$region2 <- region2
insu$region3 <- region3

insutr <- insu[, -c(2,6)]
head(insutr)
model_ins <- lm(charges ~ ., data = insutr)
attach(model_ins)
# 범주형 변수 0, 1
# 있고 없고는 기울기에 영향을 주지않고 상수역할
# 거주 지역에 따라서 의료 감소 경향이 있음 southeast 지역이 의료비가 가장 많이 절감
y = -11990.3 + (257*age) + (338.7*bmi) + (474.6*children) + (23836.3*smokeryes)  + (-352.2*region1) + (-1034.4*region2) + (-959.4*region3)




# Boston : 보스턴 시 주택 가격 데이터
# 주택 가격에 영향을 미치는 요소를 분석하고자 하는 목적으로 사용 
# 회귀분석에 활용 
# 범죄율, 학생vs교사 비율 등 보스턴 주택 가격의 중앙값(median value)
# 전제 : 주택가격에 영향을 미치는 요소에 대하여 연구
# 변수를 다음의 것으로 결정하였다. => 주택가격에 어떤영향을 미치는가. ?
# 교통, 쾌적한 환경, 경관, 주택의 구조, 교육환경, 부랑아 비율, 
#crim : 도시 1인당 범죄율 V
#zn :   25,000 평방피트를 초과하는 거주지역 비율 
#indus :비상업지역이 점유하고 있는 토지 비율  -
#chas : 찰스강에 대한 더미변수(1:강의 경계 위치, 0:아닌 경우) #애는 제
# nox : 10ppm 당 농축 일산화질소 V
#rm : 주택 1가구당 평균 방의 개수 V
#age : 1940년 이전에 건축된 소유주택 비율 -
#dis : 5개 보스턴 직업센터까지의 접근성 지수  
#rad : 고속도로 접근성 지수 V
#tax : 10,000 달러 당 재산세율 
#ptratio : 도시별 학생/교사 비율 V
#black : 자치 도시별 흑인 비율 
#lstat : 하위계층 비율 
#medv : 소유 주택가격 중앙값 (단위 : $1,000)


library(MASS)
data("Boston")
help("Boston")
str(Boston)
head(Boston, 3)
class(Boston)
# 회귀분석, 신경망
# 데이터 전처리 : 결측치, 이상치, 범주화, 정규화(모든 변수들의 기여도, 영향력을 동일하게) 처리
# 정규화 : min-max, z-점수 정규화(scale)
scale(Boston) # matrix, array로 변경됨
boston_df <- as.data.frame(scale(Boston)) # z점수 정규화를 통해 => 기여도가 동일해짐, 동등하게 됨
head(boston_df,3)
# sampling : 과적합을 방지 train / test 나눔
set.seed(123) # 같은 결과가 나오도록 학습하기 위해 값을 줌
idx <- sample(1:nrow(boston_df), 300)
trainDF <- boston_df[idx,]
testDF <- boston_df[-idx,]
dim(trainDF)
dim(testDF)

form <- medv ~ crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+black+lstat # 범주형 변수 제거
lm_model <- lm(formula = form, data = trainDF)
lm_model
# y <- 0.037345 + (0.

names(lm_model)
# 예측값
lm_model$fitted.values[1:5]
# 잔차확인(예측값과 실제값과 차이)
lm_model$residuals[1:5]

trainDF$medv[1:5]
trainDF$medv[1:5] - lm_model$fitted.values[1:5]
summary(lm_model)

pred <- predict(lm_model,testDF)
length(pred)
summary(pred)

cor(pred, testDF$medv)
#자기상관 
dwtest(lm_model)


install.packages("car")
library(car)
sqrt(vif(lm_model)) > 2 # 다중공선성존재(변수간에 서로 영향을 미치는, 상관이 있는 변수가 있다.) ->TRUE 5개가 나옴
# 상관관계
cor(trainDF[c('nox','dis', 'rad', 'tax')]) # rad와 tax domain knowlegde를 모름 -> 임의적으로 가장 높은 값의 변수를 선택함

form <- medv ~ crim+zn+indus+rm+age+dis+rad+ptratio+black+lstat # rad 와 nox로 선택하여 제거
lm_model <- lm(formula = form, data = trainDF)
pred <- predict(lm_model, testDF)
cor(pred, testDF$medv)
# scale을 해서 예측한 결과는 scale 되어 있는 상태다 이를 원래 주택가격으로 변화
# pred[1] => 원래의 가격으로 변환
# z점수 : (관측치-평균)/표준편차
# 관측치 = (z점수 * 표준편차) + 평균균

pred[1] * sd(Boston$medv) + mean(Boston$medv)
testDF$medv[1]* sd(Boston$medv) + mean(Boston$medv)
