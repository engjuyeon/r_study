# Naive Bayes Model
  # Naive 변수간에 독립성을 전제로
  # Bayes 정리에 기반을 둠
# install.packages("naivebayes")
library(naivebayes)
library(e1071)

data(iris)
attach(iris)
set.seed(415)
idx <- sample(1:nrow(iris), 0.7 * nrow(iris))

train <- iris[idx,]
test <- iris[-idx,]
train; test
nrow(train)
# ?nativeBayes # 사후확률계산, 변수간의 관계를 독립적으로 전제


model <- naiveBayes(Species ~ ., data = train) # => 이부분만 바뀐다.
model

p <- predict(model, test, type='class')

res <- table(p, test$Species)
success <- sum(res[1,1], res[2,2], res[3,3])/nrow(test)
print(success) # 0.9111111
caret::confusionMatrix(res) # Accuracy : 0.9111

# install.packages("klaR")
library(klaR)

nb_mod <- NaiveBayes(Species ~., data= train)
pred <- predict(nb_mod, test)
tab <- table(pred$class, test$Species)
caret::confusionMatrix(tab)
opar = par(mfrow=c(2,2), mar=c(4,0,0,0))
plot(nb_mod, main = "")
par(opar)

# weatherAUS.csv 파일 random하게 30000개 표본 샘플링하여 k겹교차검정수행, naive bayes 모델 구성

str(weatherAUS)
weatherAUS <-read.csv("weatherAUS.csv")
weatherAUS <- weatherAUS[,c(-1,-2,-22,-23)]
idx <- sample(1:nrow(weatherAUS), 30000, replace = TRUE)
library(caret)
library(rpart)

train <- weatherAUS[idx,]
test <- weatherAUS[-idx,]
train; test

nrow(train)
# ?nativeBayes # 사후확률계산, 변수간의 관계를 독립적으로 전제

model <- naiveBayes(RainTomorrow ~ ., data = train) # => 이부분만 바뀐다.
model

p <- predict(model, test)
res <- table(p, test$RainTomorrow)

success <- sum(res[1,1], res[2,2])/nrow(test)
print(success) # 0.9208818 정분류율
fail <- sum(res[1,2], res[2,1])/nrow(test)
print(fail) # 0.19504224 오분류율
sum(res[1,1], res[1,])/nrow(test) # 정밀도
sum(res[1,1], res[,1])/nrow(test) # 민감도도
sum(res[2,2], res[,2])/nrow(test) # 재현율
####################################################################
# install.packages("cvTools")
library(cvTools)
# cvFold를 이용한 교차검증
idx <- sample(1:nrow(weatherAUS), 30000)
weatherAUS_df <- weatherAUS[idx,]
dim(weatherAUS_df)
cross <- cvFolds(n=nrow(weatherAUS_df), K = 3, R = 1, type="random")
head(cross)
k = c(1,2,3)
r = 1
cnt <- 1
acc <- numeric()
for(i in k){ # 테스트 번호
  cat('test = ', i, '\n')
  idx <- cross$subsets[cross$which == i, r] # 테스트 데이터 추출
  test <- weatherAUS[idx,]
  for(j in k[-i]){ # train 데이터
    cat('train = ', j , '\n')
    idx <- cross$subsets[cross$which == j, r]
    train <- weatherAUS[idx,]
    model = naiveBayes(RainTomorrow ~ ., data = train)
    pred <- predict(model, test)
    t <- table(pred, test$RainTomorrow)
    acc[cnt] <- (t[1,1] + t[2,2]) / nrow(test) # 정분류율 계산
    cat('cnt = ', cnt, '\n')
    cnt <- cnt + 1
  }
}
length(acc)
acc
acc_avg <- mean(acc)
###############################################
#CARET
str(weatherAUS)
weatherAUS <-read.csv("weatherAUS.csv")
weatherAUS <- weatherAUS[,c(-1,-2,-22,-23)]
idx <- createDataPartition(weatherAUS$RainTomorrow, p = 0.8, list = F)

train <- weatherAUS[idx,]
test <- weatherAUS[-idx,]

data_x <- train %>% 
  select(-RainTomorrow)
data_y <- train %>% 
  select(RainTomorrow) %>%  # 결과가 data.frame
  pull() # 결과가 한 열인데 => [[]] 을 씌워서 순수데이터만 리턴


data_y <- as.factor(data_y) # vector에 대한 명령

ctrl <- trainControl(
  method = "cv", # resample
  number = 5,
  summaryFunction = twoClassSummary, # performance요약
  classProbs = T, # 분류된 클래스 확률을 계산할 것인가
  verboseIter = T # 반복할 때 log 여부
  )



# y = ax + b
model <- caret::train(
  x = data_x, # 독립변수
  y = data_y, # 종속변수
  method = "naive_bayes",
  trControl = ctrl,
  preProcess= c("center", "scale") # 전처리 : 수치 데이터에 대하여
)

# warnings()
pred <- predict(model, newdata = select(test, -RainTomorrow))
confusionMatrix(as.factor(pred), as.factor(test$RainTomorrow))


# sms_spam.csv 로딩하고 ham/ spam메세지(종속변수 : Target)를 분류하는 NB모델 구성하고 정분류율, 재현율, 정밀도 출력
library(ggplot2) 

library(foreign) 

library(MASS) 

library(dplyr)

sms1 <- read.csv("sms_spam_tm.csv")
str(sms1)
d = select_if(sms1, is.character) # 문자열
vec <- c(names(d)) # 문자열 이름만 추출
for(i in vec){
  sms1[,i] <- as.factor(sms1[,i])
}
str(sms1) # text mining에서 documentTermMatrix 데이터 구성 # 기본 => 문서를행, 단어를 열로 하는(mart)
levels(sms1$sms_type)
table(sms1$sms_type)
dim(sms1)
colnames(sms1) 
sms1 <- sms1[,c(-1)]

idx <- sample(nrow(sms1),0.7 * nrow(sms1))
library(caret)
library(rpart)

train <- sms1[idx,]
test <- sms1[-idx,]
train; test

nrow(train)
# ?nativeBayes # 사후확률계산, 변수간의 관계를 독립적으로 전제

model <- naiveBayes(sms_type ~ ., data = train) # => 이부분만 바뀐다.
model

p <- predict(model, test)
res <- table(p, test$sms_type)

success <- sum(res[1,1], res[2,2])/nrow(test)
print(success) # 0.9688249 정분류율
fail <- sum(res[1,2], res[2,1])/nrow(test)
print(fail) # 0.03117506 오분류율
sum(res[1,1], res[1,])/nrow(test) # 정밀도 1.724221
sum(res[1,1], res[,1])/nrow(test) # 민감도 1.717026
sum(res[2,2], res[,2])/nrow(test) # 재현율 0.2517986

# caret패키지
table(sms1$sms_type) %>%  prop.table() # 구성비 확인인
idx <- createDataPartition(sms1$sms_type, p = 0.7, list = F)

train <- sms1[idx,]
test <- sms1[-idx,]

(vec)
vec <- vec[-1]
data_x <- sms1[,vec]
data_y <- sms1[, "sms_type"]

ctrl <- trainControl(
  method = "cv", # resample
  number = 10,
  classProbs = T, # 분류된 클래스 확률을 계산할 것인가
  verboseIter = T # 학습 시 log 출력 여부
)



# y = ax + b
model <- caret::train(
  x = data_x, # 독립변수
  y = data_y, # 종속변수
  method = "naive_bayes", # 학습을 이걸로 할것이ㅏㄷ.
  trControl = ctrl, # ㅎ
  tuneGrid = expand.grid(usekernel = c(F), # parameter 값 3개 # 
                         laplace = 1,
                         adjust = 1)
)

# warnings()
pred <- predict(model, newdata = select(test, -sms_type))
confusionMatrix(as.factor(pred), as.factor(test$sms_type))


###################

#CARET

Titanic_train <-read.csv("Titanic_train.csv")
Titanic_test <-read.csv("Titanic_test.csv")
str(Titanic_test)
str(Titanic_train)
library(dplyr)
d = select_if(Titanic_train, is.character) # 문자열
vec <- c(names(d)) # 문자열 이름만 추출
for(i in vec){
  Titanic_train[,i] <- as.factor(Titanic_train[,i])
}


Titanic_train$Survived[Titanic_train$Survived == "1"] <- as.character("YES")
Titanic_train$Survived[Titanic_train$Survived == "0"] <- as.character("NO")

Titanic_train$Survived <- factor(Titanic_train$Survived, levels=c("YES", "NO"))

vec <- c(names(Titanic_train)) 
vec <- vec[-2]
data_x <- Titanic_train[,vec]
data_y <- Titanic_train[, "Survived"]
str(data_y)

data_y <- as.factor(data_y)
str(data_x)
str(data_y)
#CARET


Titanic_train$Survived[Titanic_train$Survived == "1"] <- as.character("YES")
Titanic_train$Survived[Titanic_train$Survived == "0"] <- as.character("NO")

Titanic_train$Survived <- factor(Titanic_train$Survived, levels=c("YES", "NO"))


data_x <- Titanic_train %>% 
  select(-Survived)
data_y <- Titanic_train %>% 
  select(Survived) %>% 
  pull()

data_y <- as.factor(data_y)
str(data_y)
ctrl <- trainControl(
  method = "cv", # resample
  number = 3,
  classProbs = T, # 분류된 클래스 확률을 계산할 것인가
  verboseIter = T # 반복할 때 log 여부
)


# y = ax + b
model <- caret::train(
  Survived ~., data = Titanic_train,
  method = "naive_bayes",
  trControl = ctrl
)

# warnings()
pred <- predict(model, newdata = select(Titanic_test, -Survived))
confusionMatrix(as.factor(pred), as.factor(Titanic_test$Survived))
