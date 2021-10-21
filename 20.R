library(e1071)
?svm
df = data.frame(
  x1 = c(1,2,1,2,4,5,6),
  x2 = c(8,7,5,6,1,3,2),
  y=factor(c(1,1,1,1,0,0,0))
)
df
model_svm = svm(y ~ ., data = df) # 종속변수 y

model_svm # type =  C-classification 0 ~ 무한대, kernel : radial 방사형커널
summary(model_svm)

par(mfrow=c(1,1))
plot(df$x1, df$x2, col=df$y)
x11()
plot(model_svm, df)

####################################
data("iris")
set.seed(415)
idx = sample(1:nrow(iris), 0.7*nrow(iris))
training = iris[idx,]
testing = iris[-idx,]

# 디폴트 parameter로 학습
model_svm = svm(Species ~ ., data = training, na.action = na.omit)
summary(model_svm)
pred <- predict(model_svm, testing)
res <- table(pred, testing$Species)
res # 13 17 12
sum(diag(res))/sum(res) # 0.9333333
tuning <- tune.svm(Species ~ ., data = training,
                   gamma = 10^(-5:1), cost=10^(-2:3))
                                                 
tuning # gamma 0.01 , cost 1000 추천

#tuning 후 결정된 parameter로 학습
model_svm2 = svm(Species ~ ., data = training,  gamma = 0.01, cost=1000, na.action = na.omit)
pred2 <- predict(model_svm2, testing)
tb <- table(pred2, testing$Species)
tb # 13 16 15
# 정분류율 더 높게 나옴
(13+16+15) / nrow(testing) # 0.9777778
sum(diag(tb))/nrow(testing) # 0.9777778

############################################################################ 문제
weather <- read.csv("weatherAUS.csv")

str(weather)
weather <- weather[c(-1,-2,-8,-10,-11,-22)]
weather$RainTomorrow <- as.factor(weather$RainTomorrow)
weather <- na.omit(weather)
set.seed(415)
idx = sample(1:nrow(weather), 0.7*nrow(weather))

training = weather[idx,]
testing = weather[-idx,]

# 디폴트 parameter로 학습
model_svm = svm(RainTomorrow ~ ., data = training)
summary(model_svm)
pred <- predict(model_svm, testing)
res <- table(pred, testing$RainTomorrow)
res # 13 17 12
sum(diag(res))/sum(res) # 0.9648176
tuning <- tune.svm(RainTomorrow ~ ., data = training,
                   gamma = 10^(-1:1), cost=10^(0:3))

tuning # gamma 0.01 , cost 1000 추천

#tuning 후 결정된 parameter로 학습
model_svm2 = svm(RainTomorrow ~ ., data = training,  gamma = 0.01, cost=1000, na.action = na.omit)
pred2 <- predict(model_svm2, testing)
tb <- table(pred2, testing$RainTomorrow)
tb # 13 16 15
# 정분류율 더 높게 나옴
(13+16+15) / nrow(testing) # 0.9777778
sum(diag(tb))/nrow(testing) # 0.9777778


# caret을 이용한 svm분석

library(mlbench)
data(Sonar)
str(Sonar)
library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[inTraining,]
testing <- Sonar[-inTraining,]
svmControl <- trainControl(method = "repeatedcv",
                           number = 10, repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random")
set.seed(825)
# svmRadial, svmLinear, svmPloy에 대한 파라미터 최적화 결고가 다름
svmFit <- train(Class ~ ., data = training,
                method = "svmRadial", # svmLinear, svmPloy
                trControl = svmControl,
                preProc = c("center", "scale"),
                metric = "ROC",
                tuneLength = 15)
svmFit # sigma = and c = -> cost : 오분류 규제값
svmFit$bestTune
sonar_p <- predict(svmFit, newdata = testing)
head(sonar_p)
table(sonar_p, testing$Class)
confusionMatrix(sonar_p, testing$Class)
# svmRadial :
# svmLinear :
# svmPloy :

# svm을 이용한 회귀
# 종속변수를 통해서 확인 : 종속변수가 연속형인 경우에 회귀로 작업 eps-regression
library(rpart)
library(mlbench)
data(Ozone)
str(Ozone)
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,c(-1,-2,-3)])
trainset <- na.omit(Ozone[-testindex, c(-1,-2,-3)])
str(testset)
# svmtype = eps-regression
# svmkernel = radial 방사형 커널
tuning <- tune.svm(V4 ~ ., data = trainset, gamma = 10^(-5:1), cost = 10^(-10:3))
tuning$best.parameters
tuning$best.parameters[,"gamma"]
tuning$best.parameters[,"cost"]

svm.model <- svm(V4 ~ ., data = trainset, gamma = tuning$best.parameters[,"gamma"], cost = tuning$best.parameters[,"cost"])
svm.pred <- predict(svm.model, testset)
crossprod(svm.pred - testset[,1]) / length(testindex) # 공분산
cor(svm.pred, testset[,1]) # 상관계쑤 0.89 : 1이 완전 상관

a <- c(2,3,4)
b <- c(5,6,7)

a%*% b
crossprod(a,b)
crossprod(svm.pred - test)
summary(svm.model)