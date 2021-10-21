# Logistic


Dine <- matrix(c(0,17,0,19,0,20.5,0,21.5,0,22,1,24,1,25,0,25,0,27,1,28,1,30,0,31.5,1,32,1,34,1,35,1,36.5,1,39,1,40,1,44),nc=2,byrow=T)

Dine

#소득에따른외식여부의결정
colnames(Dine) <- c("Class", "Income")
(Dine<-as.data.frame(Dine))
#로지스틱 회귀 명령
Logit.model <- glm(Class~Income, family="binomial", data=Dine)
summary(Logit.model)
Logit.model$coeff[1]
Logit.model$coeff[2] # log를 취한 결과값
# 결과를 지수화하는 이유
OR <- exp(Logit.model$coeff[2]) # 소득의 1.4배가 되어야 외식을 하겠다.
OR


# 내일 비가 올 것인가?
############################
weather <- read.csv("weather.csv", stringsAsFactors = F)
dim(weather)
head(weather)
str(weather) #
# 필요 없는 chr 제거
weather_df <- weather[,c(-1,-6,-8,-14)]
sum(is.na(weather_df))
weather_df <- na.omit(weather_df) # 결측치 처리
str(weather_df)

# 범주형으로 데이터 변환
weather_df$RainTomorrow[weather_df$RainTomorrow=="Yes"] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow=="No"] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)
str(weather_df)

# 데이터 샘플링 
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train<- weather_df[idx,]
test<- weather_df[-idx,]

# 로지스틱 회귀 모델 생성 : 학습데이터
weather_model <- glm(RainTomorrow ~ ., data = train, family = "binomial")
weather_model
summary(weather_model)

# 예측치 생성 # type = "response" => 확률값으로 리턴됨
pred <- predict(weather_model, newdata = test, type ="response")
pred
summary(pred)
str(pred)

# 예측치 변환
cpred <- ifelse(pred >= 0.5, 1, 0) # 예측치 확률값이 0.5 보다 크면, 작으면
table(cpred)

t <- table(cpred, test$RainTomorrow)
t # 정분류, 오분류, 정밀도, 민감도, 특이도 확인

# 정분류율 계산
sum(diag(t)) / nrow(test) # diag() 행렬에서 대각선만 뽑아냄  #0.8348624 대각선을 전체개수로 나눔
# 확률 0.83

#민감도 sencitivity
82 / (82+8) # 0.9111111
#특이도 specificity 
9 / (9+10) # 0.4736842



# 민감도와 특이도와의 관계에서 평가 모델 제공 : ROC 커브
# install.packages("ROCR")
library(ROCR)
length(cpred)
length(test$RainTomorrow)
pr <- prediction(pred, test$RainTomorrow)
pfr <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(pfr)

# install.packages("pscl")
library(pscl)
pR2(weather_model) # r2CU : 0.5444990 => 로지스틱 회귀 분석에서 설명력 : 54퍼센트

# 다항형 로지스틱 회귀
library(nnet)
data("iris")
str(iris)
idx <- sample(1:nrow(iris), nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]
model <- multinom(Species ~ ., data = train) # 3개의 데이터에 대해서 확률값으로 표현
fit <- model$fitted.values
head(fit)
# softmax 함수 : 결과를 확률값으로 변환 -> 신경망에서 중요한 분류매핑 함수 activation함수 (활성화함수) 
fit_pred <- ifelse(fit[,1] > 0.5, 'setosa', ifelse(fit[,2]>= 0.5, 'versicolor', 'virginica'))
table(fit_pred)

pred_prob <- predict(model, newdata = test, type="probs")
fit_pred <- ifelse(pred_prob[,1] >= 0.5, 'setosa', ifelse(pred_prob[,2]>= 0.5, 'versicolor', 'virginica'))
table(fit_pred, test$Species)

# 정분류
sum(diag(table(fit_pred, test$Species))) / nrow(test) # 테스트 정분류율 0.9333333

##########################
# install.packages("ISLR")
library(ISLR)
Smarket
head(Smarket)
str(Smarket)

attach(Smarket)
plot(Volume)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, family = binomial)
summary(glm.fit)
# aic 1741.6 모델 복잡도 => 낮을수록 좋음, 표본의 분산값 + 변수의 개수
confint(glm.fit)
exp(coef(glm.fit)) # 회귀분석 승부비
exp(confint(glm.fit))
glm.probs <- predict(glm.fit, type = "response")
residuals(glm.fit, type="deviance")
glm.pred <- rep("Down", dim(Smarket)[1]) # 행과 열
(glm.pred[glm.probs>0.5] <- "Up")
res <- table(glm.pred, Direction)
res

sum(diag(res))/sum(res) # accuracy 0.5216
detach(Smarket)

# 문제
# Class => 암여부 1 폐암, 2 폐암아님
train <- read.csv("breastcan_t.csv.csv")
test <- read.csv("breastcan_te.csv.csv")
str(train)
str(test)
head(train)

# 로지스틱 회귀 모델 생성 : 학습데이터
bmodel <- glm(Class ~ ., data = train, family = "binomial")
bmodel
summary(bmodel)


exp(coef(bmodel)) # 회귀분석

# 예측치 생성 # type = "response" => 확률값으로 리턴됨
pred <- predict(bmodel, newdata = test, type ="response")
pred
summary(pred)
str(pred)

# 예측치 변환
cpred <- ifelse(pred >= 0.5, 1, 0) # 예측치 확률값이 0.5 보다 크면, 작으면
table(cpred)


t <- table(cpred, test$Class)
t

sum(diag(t))/sum(t) # accuracy 0.9607843

# 신생아 체중에 영향을 미치는 요인에 대하여 선형회귀분석및 상관분석을 실시
# 종속변수 : 신생아 어린이의 체중(bwt)
# 독립변수 : 나이(age), 어머니의 체중(lwt), 인종(race : white 1 , black 2, other 3), 흡연여부(smoke), 낙태여부(ptl), 고혈압병여부(ht), 신체민감도(ui)

library(MASS)
str(birthwt)
?birthwt
attach(birthwt)
birthdata <- birthwt[,c(-1, -9)]
sum(is.na(birthdata))

cor(birthdata)

corrplot::corrplot(cor(birthdata), method = "pie")
chart.Correlation(cor(birthdata), histogram = T, pch = 19)

birthmodel <- lm(bwt ~ ., data = birthdata)
summary(birthmodel) 
# p-value 0.05보다 작으면 귀무가설 기각 => 유의미한 변수

birthmodel <- lm(bwt ~ lwt + race + smoke + ht + ui, data = birthdata)
summary(birthmodel)


library(lmtest)
dwtest(birthmodel)


# 데이터 샘플링 
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train<- weather_df[idx,]
test<- weather_df[-idx,]

# 로지스틱 회귀 모델 생성 : 학습데이터
weather_model <- glm(RainTomorrow ~ ., data = train, family = "binomial")
weather_model
summary(weather_model)

# 예측치 생성 # type = "response" => 확률값으로 리턴됨
pred <- predict(weather_model, newdata = test, type ="response")
pred
summary(pred)
str(pred)

# 예측치 변환
cpred <- ifelse(pred >= 0.5, 1, 0) # 예측치 확률값이 0.5 보다 크면, 작으면
table(cpred)
predict(birthmodel, newdata = test)
