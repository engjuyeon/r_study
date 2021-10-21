# install.packages("adabag")
library("adabag")
data("iris")
str(iris)
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25)) # 층화추출
iris.adaboost <- boosting(Species ~ ., data = iris[train, ],
                          mfinal = 20, control = rpart.control(maxdepth = 2))
names(iris.adaboost)
iris.adaboost$trees
iris.adaboost$importance
barplot(iris.adaboost$imp[order(iris.adaboost$imp, decreasing = T)],
        ylim = c(0,100), main = "변수중요도",
        col = "lightblue")
tb <- table(iris.adaboost$class, iris$Species[train])
round(sum(diag(tb))/sum(tb)*100) # 100 완전적합 : 잘못 판정된 데이터에 가중치를 부여해서 모델을 개선
iris.predboosting <- predict.boosting(iris.adaboost, newdata = iris[-train,])
iris.predboosting

tb2 <- table(iris.predboosting$class, iris$Species[-train])
round(sum(diag(tb2))/sum(tb2)*100) # 92 과적합
iris.boostcv <- boosting.cv(Species ~ ., v = 10, data = iris, mfinal=10)
iris.boostcv

names(iris.boostcv)
iris.boostcv$confusion
#
adadata <- read.csv("bafull.csv", header = T, sep=";") # sep 분리자. text는 분리자가 있어야함(공백, comma, semi colon, colon)
library(caret)
str(adadata)
# y : 정기적금을 확정 (yes/no)
# default : 채무불이행
# contact : 상담 통신유형
# day : 마지막 상담일자
# campaign : 캠페인 기간동안 상담횟수
# pdays : 이전 캠페인에서 고객에 마지막으로 연락한 후 경과일 수
# previous : 캠페인 이전에 고객과 상담한 횟수
 # poutcome : 이전 마케팅 캠페인 결과
# 모든 변수에 대해 categorical(범주형) 확인
# y 값인 고객이 정기 예금 가입여부를 결정하는 모델을 구성

str(adadata)
adadata$y <- as.factor(adadata$y)
levels(adadata$y)


adadata <- na.omit(adadata)

# 범주화
library(dplyr)
d = select_if(adadata, is.character) # 문자열
vec <- c(names(d)) # 문자열 이름만 추출
for(i in vec){
  adadata[,i] <- as.factor(adadata[,i])
}
str(adadata)

# 수치 데이터에 대하여 z점수 정규화 (1% 떨어짐 => 일단 생략)
trainX = select_if(adadata, is.numeric) # 숫자열
vec <- c(names(trainX)) # 숫자열 이름만 추출
for( i in vec){
   adadata[,i] <- scale(adadata[,i])
}
str(adadata)


intrain <- createDataPartition(y = adadata$y, p = 0.7, list = F)  # 층화 추출 - 종속변수를 고려
anyNA(adadata) # NA가 하나라도 있니
summary(adadata)

prop.table(table(adadata[intrain,]$y)) * 100
set.seed(400)

# coeflearn hyper parameter : Breiman, Freund : 그래픽지식
data.adaboost <- boosting(y ~ ., data = adadata[intrain, ], 
                          mfinal = 20, coeflearn = 'Breiman') # 가중치를 얼마 적용하는
summary(data.adaboost)

names(data.adaboost)
data.adaboost$trees
data.adaboost$formula
data.adaboost$weights
data.adaboost$trees
data.adaboost$importance
barplot(data.adaboost$imp[order(data.adaboost$imp, decreasing = T)],
        ylim = c(0,100), main = "변수중요도",
        col = "lightblue")
tb <- table(data.adaboost$class, adadata$y[intrain])
round(sum(diag(tb))/sum(tb)*100) # 91
data.predboosting <- predict.boosting(data.adaboost, newdata = adadata[-intrain, ])
data.predboosting

tb2 <- table(data.predboosting$class, adadata$y[-intrain])
round(sum(diag(tb2))/sum(tb2)*100) # 90
data.boostcv <- boosting.cv(y ~ ., v = 10, data = adadata, mfinal=20)
data.boostcv

names(data.boostcv)
data.boostcv$confusion



################# bgm(gradient boosting model)
install.packages("gbm")
library(gbm)
library(ggplot2)
library(reshape2)
library(randomForest)
library(MASS)
letter <- read.csv("letter_rec.csv", header = T) # 문자인식
str(letter)

attach(letter)
letterBCDGOQ <- letter[Class == "B" | Class == "C" | Class == "D" | Class == "G" | Class == "O" | Class == "Q" ,]

write.csv(letterBCDGOQ, "letterBCDGOQ.csv")
letter6 <- read.csv("letterBCDGOQ.csv", header = T)[, -1] # 종속 변수
letter.train <- letter6[1:3692,]
letter.test <- letter6[-(1:3692),]
set.seed(1234)
t <- proc.time()
# 경사하강법 - 근접해를 구하는 방법 : 비선형인 경우 ( log, 삼각함수, exp : 초월함수)
# 삭습방향 -> 이동 (learning rate가 적으면 속도가 더뎌지고, 많아지면 해에 근접하는 것이 불가능)
# shrinkage == learning rate
# hyper parameter tuning 
# interaction.depth 과적합을 방지 : depth 제약
# bag.fraction : 1일 때는 똑같은 방식으로 2번한다.
gbm.letter <- gbm(Class ~ ., data = letter.train, distribution = "multinomial", 
                  n.trees = 10000, shrinkage = 0.01, interaction.depth = 3, bag.fraction = 0.5, cv.folds = 10)

t1<-proc.time() - t # 경과시간
t1

# gbm.perf : boosting iteratopms 반복횟수
best.iter <- gbm.perf(gbm.letter, plot.it = F, method ="cv")
best.iter

pre.gbm <- predict(gbm.letter, newdata = letter.test, n.trees = best.iter, type = "response")
pre <- apply(pre.gbm, 1, which.max) # softmax => 확률 . which.max 최고값
pre[pre == 1] <- "B"
pre[pre == 2] <- "C"
pre[pre == 3] <- "D"
pre[pre == 4] <- "G"
pre[pre == 5] <- "O"
pre[pre == 6] <- "Q"
pre <- as.factor(pre) # 예측값
true <- letter.test$Class # 원래값
gbm.error <- 1 - sum(true == pre) / length(pre) # 오분류율
gbm.error
sum(true == pre) / legnth(pre) # 정분류율

# install.packages("mlbench")
library(mlbench)
? Sonar # Mines vs Rocks 분류 문제 데이터
# glm 으로 분류모델 구축, interaction.depth에 대하여 최적의 깊이를 결정하고 n.trees 모델 개수도 결정해보시오
# parameter tuning
data(Sonar)
str(Sonar)
summary(Sonar)
library(caret)

sum(is.na(Sonar))
# 과적합을 방지하기 위한 데이터 분할
set.seed(998)
inTraining <- createDataPartition(y = Sonar$Class, p = 0.70, list = F)  
training <- Sonar[inTraining,]
testing <- Sonar[-inTraining,]

# caret패키지에서 실행
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
)

# 첫번째 모델

set.seed(825)
gbmFit1 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = F)
gbmFit1 # 3                  150      0.8317802  0.6595887
# The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.
plot(gbmFit1)


# 예측
pred <- predict(gbmFit1, newdata = testing)
tb <- table(pred, testing$Class)
round(sum(diag(tb))/ sum(tb)*100) # 79% 정분류율


# 파라미터 튜닝
gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9), # 광범위하게 테스트 하기 위해서는 처음에서는 듬성듬성 ; ex) 1, 10, 20 => 10이 나오면 
                       #                                                                                  두번째는 5, 10, 15 => 또 10이 나오면
                       #                                                                                  세번째는 8, 10, 12 이런식으로 점점 촘촘히
                       n.trees = (1:30)*50,
                       shrinkage = 0.1,
                       n.minobsinnode = 20)

nrow(gbmGrid)
set.seed(825)

# 두번째 모델
gbmFit2 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = F,
                 tuneGrid = gbmGrid)
gbmFit2
# The final values used for the model were n.trees = 1300, interaction.depth = 9, shrinkage = 0.1 and n.minobsinnode = 20.

trellis.par.set(caretTheme())
plot(gbmFit2)
ggplot(gbmFit2)

set.seed(825)

# 세번째 모델
gbmFit3 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = F,
                 tuneGrid = data.frame(interaction.depth = 9,
                                       n.trees = 1300,
                                       shrinkage = 0.1,
                                       n.minobsinnode = 20))#,classProbs = TRUE),
                
                 # metric = "ROC")
gbmFit3
# Accuracy  # regulized 규제를 사용하여 정분류율은 높이고 얘는 낮춰야한다.
# 0.82863  

plot(gbmFit3)
ggplot(gbmFit3)
class(testing)
pred <- predict(gbmFit3, newdata = testing)
tb <- table(pred, testing$Class)
round(sum(diag(tb))/ sum(tb)*100) # 81%
?gbm








# XGboost
# matrix : sparce matrix(희소행렬), dense matrix(밀집행렬), xgb.Matrix(xgb고유행렬포맷) 종류 3가지
install.packages("xgboost", dependencies = T) # dependencies TRUE로 적용하는 게 좋음 종속관계에 있는 패키지 전부 설치
require(xgboost)
# 독버섯 관측 데이터 ( 독버섯인가 아닌가. )
data(agaricus.train, package="xgboost")
data(agaricus.test, package="xgboost")
train <- agaricus.train
test <- agaricus.test
str(train) # r의 4세개 클래스
# xgboost에 중요 parameter : max_depth, nthread, nrounds : iterator
# 'dgcMatrix' sparse Matrix  data, label라벨
# 희소행렬 ='> 특징추출해야 행렬연산이 됨.
bst <- xgboost(data = train$data, label = train$label, max.depth = 2,   eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic") # 로지스틱 회귀 분석(2진 분류)

# booster 생성
pred <- predict(bst, test$data)

# 고유포맷으로 변환
dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
watchlist <- list(eval = dtest, train = dtrain)
# eta : learning rate
param <- list(max_depth = 2, eta = 1, silent =1 , nthread =2,
              objective = "binary:logistic", eval_metric = "auc") # auc 커브 : 민감도, 1 - 특이도와의 관계 ( 꽉차게 나오는게 좋다 )

bst <- xgb.train(param, dtrain, nrounds = 2, watchlist)
bst

xgb.save(bst, 'xgb.model')
bst <- xgb.load('xgb.model')
pred <- predict(bst, test$data)
pred
err <- mean(as.numeric(pred > 0.5)!= test$label)
print(paste("test-error=", err)) #정분류율은 98%, 오분류율 : 2.1%
# 결측치 처리, 정규화에 대해서는 자동화되어있음


library(data.table)
# install.packages("mlr")
library(mlr)

setcol <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "
            occupation", " relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "target")
train <- read.table("adult.data", header = F, sep = ",", col.names = setcol, na.strings = c(" ?"), stringsAsFactors = F)
str(train)
head(train)
train <- train[, - 14]
head(train)
test <- read.table("adult.test", header = F , sep = ",", col.names = setcol, skip = 1, na.strings = c(" ?"), stringsAsFactors = F)
test <- test[, -14]
head(test)
# na check
table(is.na(train))
sapply(train, function(x) sum(is.na(x))/length(x))* 100
table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))* 100

# 데이터 전처리하여 xgboost로 분류 모델을 구축해보시오 
# 정리
  # factor형은 dummy변수화(더미변수화)하는 것이 좋다.
  # 범주화할 때 train과 test 데이터의 범주값이 같아야 한다. 아니면 변수 개수가 달라지는 현상이 있다.
  # NA는 Missing이라고 입력하면 자동으로 처리해준다.
  # 분류인 경우 factor변수이면 수치로 변경하는 것이 유리하다.
  # 데이터는 DMatrix로 변경 처리하는 것을 권장한다.
  # 전처리 시 다양한 일이 벌어진다. (데이터에 공백이 들어가거나 _가 들어가거나, 원하지 않는 포맷으로 입력될 때가 있다.) => 다 고려해야한다.
  # gbtree(분류) : 비선형 , gblinear(회귀) : 선형

colSums(is.na(train))
colSums(is.na(test)) # workclass, occupation

# 데이터 테이블로 변경
setDT(train)
setDT(test)

library(stringr)
head(test$target)
# 글자 마지막에 .이 있어서 삭제
# train [,target := substr(target,start = 1, stop = nchar(target)-1)]
test [,target := substr(target,start = 1, stop = nchar(target)-1)] # := => 데이터 테이블에서 즉시 데이터 수정*변환할 때 사용

# 글자에 공백이 있는 경우 => 비교가 안됨 => 공백 삭제
char_col <- colnames(train)[sapply(test, is.character)]
char_col

# 데이터 프레임 명령을 이용해서 문자열인 데이터의 공백을 제거
for(i in char_col) set(train, j=i, value = str_trim(train[[i]], side = "left"))
for(i in char_col) set(test, j=i, value = str_trim(test[[i]], side = "left"))
str(train)
str(test)

# NA 처리
train[is.na(train)] <- "Missing"
test[is.na(test)] <- "Missing"

# 범주형 데이터 수정
labels <- ifelse(train$target == "<=50K", 0, 1)
ts_label <- ifelse(test$target == "<=50K", 0, 1)

# 범주형변수 더미 변수화, 변수가 60개로 변환됨
new_tr <- model.matrix(~.+0, data = train[ , -c("target"), with = F]) # 더미변수화 해줌 false값도 고려
new_ts <- model.matrix(~.+0, data = test[ , -c("target"), with = F])

# factor형은 숫자로
labels <- as.numeric(labels)
ts_label <- as.numeric(ts_label)
# xgboost에서 추천하는 고유 포맷의 matrix로 변환
library(xgboost)
dtrain <- xgb.DMatrix(data = new_tr, label = labels)
dtest <- xgb.DMatrix(data = new_ts, label = ts_label)

# 파라미터를 결정
params <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.3,
               gamma = 0, max_depth = 6, min_child_weight = 1, subsample = 1,
               colsample_bytree = 1)

# 파라미터 튜닝 명령
xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 100, nfold = 5, 
                showsd = T, startified = T, print_every.n = 10,
                early_stop.rounds = 20, maximize = F)
xgbcv$params
xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 79,
                  watchlist = list(val = dtest, train = dtrain),
                  print_every_n = 10, early_stopping_rounds = 10,
                  maximize = F, eval_metric = "error")
xgb1$params

# 테스트 예측
xgbpred <- predict(xgb1, dtest)
xgbpred <- ifelse (xgbpred > 0.5, 1,0)

confusionMatrix(as.factor(xgbpred), as.factor(ts_label))


