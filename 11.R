# Entropy : 불순도 척도, jini계수, 카이제곱값 ((관측값 - 기대값)^2)/ 기대값
# 재귀 : 트리를 구현할 때 재귀함수를 사용
baseEntropy = 1
# 동전 앞면이 나올 확률 : 0.4, 뒷면이 나올 확률이 0.6
x1 <- 0.4
x2 <- 0.6
e <- -x1 * log2(x1) - x2 * log2(x2) # 엔트로피 : 0.9709506 => 불확실성이 크다
e

# 동전 앞면 : 0.2 뒷면 : 0.8
x1 <- 0.2
x2 <- 0.8
e <- -x1 * log2(x1) - x2 * log2(x2) # 0.7219281 => 불확실성이 작다
e

# 정보 이득
baseEntropy - 0.9709506 # 불확실성이 커지면 정보이득이 작아지고
baseEntropy - 0.7219281 # 불확실성이 작아지면 정보이득이 커진다.

library(rpart)
result <- sample(1:nrow(iris), nrow(iris) * 0.7)
train <- iris[result,]
test <- iris[-result,]
dim(train); dim(test);

formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model <- rpart(formula = formula, data = train)
model
pred <- predict(model, test)
cpred <- ifelse(pred[,1] >= 0.5, 'setosa', ifelse(pred[,2] >= 0.5, 'versicolor', 'virginica'))
tb <- table(cpred, test$Species)
tb

sum(diag(tb) / nrow(test))
plot(model)
text(model, use.n = T, cex = 0.6)
post(model, use.n = T, file="")

x11()
formula <- Species ~.
iris.df <- rpart(formula, data = iris)
iris.df
plot(iris.df)
text(iris.df, use.n = T, cex = 0.6)
post(iris.df, use.n = T, file="")
# install.packages("rpart.plot")
library(rpart.plot)
# install.packages("rattle")
library(rattle)
prp(iris.df)
rpart.plot(iris.df)
fancyRpartPlot(iris.df)


# 문제
wdbc <- read.csv("wdbc_data.csv", stringsAsFactors = F)
str(wdbc)
# diagnosis 진단 : B -> 양성 M -> 악성
# 분류 모델 생성
# FACTOR로 diagnosis 변환, 결측치처리, 정규화(mix-max확인), 범주화, 이상치 처리 확인
dim(wdbc)
wdbc <- wdbc[-1]
head(wdbc)


# 결측치 제거
sum(is.na(wdbc))
wdbc <- na.omit(wdbc)

# factor로 변환
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
table(wdbc$diagnosis)
levels(wdbc$diagnosis) # "B:1" "M:2"

# 정규화 (mix-max)
nor_minmax = function(x){
  return (x - min(x)) / (max(x) - min(x))
}

# 데이터타입 확인(범주형은 정규화 안됨 -> 제외해야함)
wdbc_x <- as.data.frame(lapply(wdbc[2:31], nor_minmax))
head(wdbc_x)
summary(wdbc_x)
wdbc_df <- data.frame(wdbc$diagnosis, wdbc_x)
summary(wdbc_df)

# 이상치 확인 # 이상치가 있다고 다 지우면 X
boxplot(wdbc_df)

# sampling : 과적합을 방지 train / test 나눔

idx <- sample(nrow(wdbc_df), 0.7 * nrow(wdbc_df))
wdbc_train <- wdbc_df[idx,]
wdbc_test <- wdbc_df[-idx,]
dim(wdbc_train); dim(wdbc_test)
str(wdbc_train)
# 분류모델 생성
model2 <- rpart(wdbc.diagnosis ~ ., data = wdbc_train)
pred2 <- predict(model2, wdbc_test, type='class')

# 모델 시각화
prp(model2)
rpart.plot(model2)
fancyRpartPlot(model2)

# 평가
tr <- table(pred2, wdbc_test$wdbc.diagnosis)
paste("test 데이터에 대한 정분류율", round( sum(diag(tr)) / nrow(wdbc_test) * 100), '%')
# * 100한 효과 => 소수점 1째자리까지 표현 가능
paste("test 데이터에 대한 오분류율", round( sum(tr[1,2], tr[2,1]) / nrow(wdbc_test) * 1000)/10, '%') 

# 정밀도, 민감도, 특이도 구해보시오
paste("test 데이터에 대한 정밀도", round( tr[1,1] / sum(tr[1,1] ,tr[1,2]) * 1000)/10, '%') 
99 / (99+9) # 0.9166667
paste("test 데이터에 대한 민감도", round( tr[1,1] / sum(tr[1,1] ,tr[2,1]) * 1000)/10, '%') 
99 / (99+5) # 0.9519231
paste("test 데이터에 대한 특이도", round( tr[2,2] / sum(tr[2,2] ,tr[1,2]) * 1000)/10, '%') 
58 / (58+9) # 0.8656716

# airquality : Temp에 영향을 미치는 Ozone, Sorar.R, wind
# install.packages("party")
library(party)
str(airquality)
formula <- Temp ~ Ozone + Solar.R + Wind
air_tree <- ctree(formula, data = airquality)
air_tree
plot(air_tree)


# DT(diction tree를 이용해서 AdultUCI 데이터에 대하여 분류분석 실시)
# 자본이득에 영향을 미치는 요인에 대하여 분석해 보시오 ( capital-gain)
# install.packages("arules")
library(arules)
data(AdultUCI)
str(AdultUCI)
?AdultUCI
# workclass 집업군 occupation 직업 relationship 가족ㄱ관계 race = 인종, native country = 국가
summary(AdultUCI)
dim(AdultUCI)

# 결측치 제거
# NA가 많아서 10000개만 샘플ㄹ이
choice <- sample(1:nrow(AdultUCI), 10000)
adult.df <- AdultUCI[choice,]
str(adult.df)


capital <- adult.df$`capital-gain`
hours<- adult.df$`hours-per-week`
education <- adult.df$`education-num`
race <- adult.df$race
age <- adult.df$age
income <- adult.df$income

adult_df <- data.frame(capital = capital, age = age, race = race, hours = hours, education = education, income = income)

formula <- capital ~ income + education + hours + race + age
adult_ctree <- ctree(formula, data = adult_df)
adult_ctree
plot(adult_ctree) # income과 education이 중요

# caret 패키지 이용학습

#install.packages("caret")
library(caret)
library(rpart.plot)
car_df <- read.csv("car.data", sep=',', header = F)  # 전부 범주형 데이터
str(car_df)
title<-c("buying", "maint", "doors", "persons", "lug_boot", "safety", "Car_Evaluation")
names(car_df) <- title
head(car_df)
set.seed(3033) # 항상 같은 결과 : 의사난수
intrain <- createDataPartition(y = car_df$buying, p = 0.7, list = F)  # 층화 추출 - 종속변수를 고려
anyNA(car_df) # NA가 하나라도 있니
training <- car_df[intrain,]
testing <- car_df[-intrain,]
summary(car_df)
# 학습 방법을 제어하는 콘트롤 요소 구성
# cross validation 교차 검증 - 모든 데이터를 test에 참여해서 테스팅
# 데이터를 (number) 10개로 균일하게 나눔 => 3번 반복
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # 30번에 걸쳐 test
set.seed(3333)
# information gain을  파라미터로 해서
# training 데이터만 입력 => train/validation(검증용)
install.packages("e1071")
library(e1071)
dtree_fit <- train(Car_Evaluation ~., data = training, method = "rpart", # 신경망
                   parms = list(split = "information"), # 불순도
                   trControl = trctrl, tuneLength = 10) # parameter tuning
dtree_fit # => cp 0.002747253일때 Accuracy가 제일 높았따.

# 가지치기의 기준값 cp : 0.01이 기본값(pruning 가지치기) : 과적합 방지

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
# 재귀적으로 데이터 나눔 => 마지막 노드인 종단노드가 모두 1개가 되면 완전 과적합 : 모델일치도
 predict(dtree_fit, newdata = testing[1,])
test_pred <- predict(dtree_fit, newdata = testing) 
confusionMatrix(test_pred, testing$Car_Evaluation)
length(test_pred)
length(testing$Car_Evaluation)
confusionMatrix(as.factor(test_pred), as.factor(testing$Car_Evaluation))
# 데이터만 보면 실시한다. ( 범주형, 연속형에 강건하고 데이터에 강건하다. -> 정규화를 하지않아도
# 분석이 가능하고, 연속형 범주형으로 만들어 속도가 빠르고, if문 만을 사용하기 때문에 빠름)
# model shopping을 하더라도 기본적으로 데이터를 이해하기 위해서 합니다. -> 항상한다라고 이해
# 변수 중요도도 출력 => 변수를 선택할 수 있도록 해줌
# 시각화를 통해 데이터 구성을 잉해할 수 있도록 해줌 (어떻게 구성, 어떤 변수 중요변수인지)
# 데이터를 어떻게 다뤄야되는지에 대해 이해할 수 있음.

# stacking => DT, RF, Ada, G => 투표, 평균 + 변수를 추가
# (PCA를 이용해 noise가 제거 된 변수를 추가하면 accuracy가 올라감) => 무조건 accuracy만 따지는 경연대회

# RF(rangom forest)
# ensemble학습, bagging : bootstrapping(변수조합), aggregation(통합)
# install.packages("randomForest")
library(randomForest)
str(iris)
# DT : 과적합, 불안전성
# 연속형 : 평균, 범주형 : 다수결의 원리인 투표
model = randomForest(Species~., data=iris) # 500개의 모델(DT)를 생성, 각트리는 2개의 변수
?randomForest
model
model = randomForest(Species~., data=iris, ntree = 300, mtry=4, na.action = na.omit) 
model
# ntree = 300, mtry = 4, 가장 중요한 hyper parameter : 사람이 결정해줘야 하는 변수
ntree <- c(400, 500, 600)
mtry <- c(2:4) # 최소 2개
param <- data.frame(n = ntree, m = mtry)
param
for(i in param$n){
  cat('ntree = ', i, '\n')
  for(j in param$m){
    cat('mtry = ', j, '\n')
    model = randomForest(Species~., data=iris, ntree = i, mtry=j, na.action = na.omit) 
    print(model)
  }
  
}

model3 = randomForest(Species~., data=iris, ntree = 400, mtry=2, importance =T, na.action = na.omit) 
model3
importance(model3)
# MeanDecreaseAccuracy 분류정확도 개선에 기여하는 변수
# MeanDecreaseGini 노드 불순도(불확실성) 개선에 기여하는 변수

varImpPlot(model3)

# for문의 효율성이 중요
# foreach
library(foreach)
m <- matrix(1:9,3,3)
m
result <- numeric()
for(i in 1:ncol(m)){ # 단일 코어 사용
  result[i] <- mean(m[,i])
}
result  

foreach(i = 1:ncol(m), .combine = c) %do% mean(m[,i]) # 멀티 코어 사용
foreach(i = 1:ncol(m), .combine = rbind) %do% mean(m[,i]) 

model_iris <- foreach(i = rep(250,4), .combine = combine) %do% 
  randomForest(Species~., data=iris, ntree = i, mtry=2, na.action=na.omit)

model_iris$predicted

# install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
getDoParWorkers()
system.time(
  rf_iris <- foreach(ntree = rep(250,8), .combine = combine,
                     .packages = 'randomForest', .multicombine = T) %dopar% 
    randomForest(Species~., data=iris, ntree = ntree, mtry=2, na.action=na.omit)
  
)

pred <- rf_iris$predicted
table(pred, iris$Species)

# 문제 : weatherAUS.csv를 
weatherAUS <- read.csv("weatherAUS.csv")
str(weatherAUS)
weather2 <- select_if(weatherAUS, is.numeric)


# factor로 변환
weatherAUS$RainTomorrow <- as.factor(weatherAUS$RainTomorrow)
table(weather2$RainTomorrow)
levels(weather2$RainTomorrow)


model3 = randomForest(RainTomorrow~., data=weather2, ntree = 100, mtry=2, importance =T, na.action = na.omit) 
model3
importance(model3)

model3 = randomForest(RainTomorrow~., data=weather2, ntree = 200, mtry=2, importance =T, na.action = na.omit) 
model3
importance(model3)
# MeanDecreaseAccuracy 분류정확도 개선에 기여하는 변수
# MeanDecreaseGini 노드 불순도(불확실성) 개선에 기여하는 변수


#
