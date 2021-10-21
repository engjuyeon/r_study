library(dplyr)
library(caret)
library(ggplot2)
library(RColorBrewer)

# class 3, class 4
data <- read.csv("Titanic_train.csv")
head(data)
str(data)
glimpse(data)
sum(is.na(data))
data_copy <- data
head(data_copy, 3)
colnames(data)

data_copy <- data_copy %>% 
  select(c(Pclass, Sex, Age, SibSp, Parch, Fare, Survived)) # survived 종속변수, sibsp 형제자매, parch : 부모

data_copy$Survived <- as.factor(data_copy$Survived)




#
# classification : box, strip, density, pairs or elipse. for regression, pairs or scatter
featurePlot(x = data_copy[, c(1,3,4,5,6)],
            y = data_copy$Survived,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation = "free")))
levels(data_copy$Survived) <- c("NS","S")


# inex가 리턴됨
in_train <- createDataPartition(data_copy$Survived, p = 0.8, list =F)
data_train <- data_copy[in_train,]
data_test <- data_copy[-in_train,]

#독립변수
data_x <- data_train %>% 
  select(-Survived)
data_y <- data_train %>% 
  select(Survived) %>% 
  pull()

ctrl <- trainControl(
  method = "cv", # cross valiation
  number = 5, # 등분
  summaryFunction = twoClassSummary,
  classProbs = T, # 클래스별 확률값을 계산여부
  verboseIter = T, # log 출력
  preProcOptions = list(thresh = 0.8) # pca(principle component analysis)
)

modelLookup("naive_bayes")
# model parameter                label forReg forClass probModel
# 1 naive_bayes   laplace   Laplace Correction  FALSE     TRUE      TRUE # training 에는 존재/ test에는 존재하지않음
# bayes 정리에 의해 계산되는 확률값이 0 => 곱셈연산
# 2 naive_bayes usekernel    Distribution Type  FALSE     TRUE      TRUE # ueskernel TRUE => kernel density esimation : 보간법(interpolation)을 적용
# 3 naive_bayes    adjust Bandwidth Adjustment  FALSE     TRUE      TRUE # adjust 절대값을 할 것인지 제곱값을 할 것인지 3승값을 할 것인지 

model_nb_default <- caret::train(
  x = data_x,
  y = data_y,
  method = "naive_bayes",
  trControl = ctrl,
  tuneGrid = expand.grid(
    usekernel = c(TRUE, FALSE),
    laplace = 0:3,
    adjust = 1:3
  )
  ,
  preProcess = c("center", "scale") # z점수 정규화
 

)
?predict


plot(model_nb_default, main="모델 정확도")
set.seed(123)
model_nb_default <- caret::train(
  x = data_x,
  y = data_y,
  method = "naive_bayes",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = expand.grid(
    usekernel = c(TRUE),
    laplace = 1,
    adjust = 3
  )
  ,
  preProcess = c("center", "scale") # z점수 정규화
  
  
)

model_nb_default
set.seed(123)
nb_default_pred <- predict(model_nb_default, newdata = select(data_test, -Survived))
nb_cm <- confusionMatrix(nb_default_pred, reference = data_test$Survived)
nb_cm # Accuracy : 0.7627       

test_data <- read.csv("Titanic_test.csv")
head(test_data)
testing_data <- test_data %>% 
  select(Pclass, Sex, Age, SibSp, Parch, Fare)
set.seed(123)
new_pred <- predict(model_nb_default, newdata = testing_data)
testing_with_pred <- test_data %>% 
  mutate(prediction = new_pred) # 예측된 값을 prediction이라는 변수로 추가
sub_data <- testing_with_pred %>% 
  select(c(PassengerId, prediction)) # 예측결과
sub_data

######################################## knn ( k nearest neighbor)
# 방향으로 거리를 잴 때 사용
vec1 <- c(1,1,1,1,1)
vec2 <- c(2,3,4,5,6)
numer <- sum(vec1 * vec2) # 내적 
denom <- sqrt(sum(vec1^2) * sqrt(sum(vec2^2))) # 원점으로부터의 크기 (norm)
numer / denom
# install.packages("lsa")
library(lsa)
vector1 <- c(1,2,3,4,5)
vector2 <- c(6,7,8,9,10)
cosine(vector1, vector2)

# min-max normalize
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x))) # 기준점이 전부 0으로 0~1 사이의 값
}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50)) # 더 중요한 변수 척도가 크다는 이유 => 가중치 붙어 있음

# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# 유클리디안 거리 계산법을 적용하여
# 단맛과 아삭거림으로 과일과 단백질 채소를 구분하는 예제
library(class)
train_df = data.frame(x1 = c(8,2,7,7,3,1),
                      x2 = c(5,3,10,3,8,1),
                      y = factor(c(1,2,3,1,3,2))) # 모두가 계산에 참여
train_df
plot(train_df$x1, train_df$x2, col=train_df$y, xlab='단맛', ylab='아삭거림')

# 새로운 항목 : 토마토(6,4), 땅콩(3,8), 사과(10,9)
test_df = data.frame(x1 = c(6,3,10), x2 = c(4,8,9))
test_df
knn(train_df[, -3], test_df, train_df$y, k = 3)
# 1, 3, 1 로 예측

# 토마토(6,4)하고 fish(2,3) 거리 비교
sqrt(((6-2)^2) + ((4-3)^2)) # 4.123106
# 토마토랑 grape(8,5)
sqrt(((6-8)^2) + ((4-5)^2)) # 2.236068 
# 토마토 carrot(7,10)
sqrt(((6-7)^2) + ((4-10)^2)) # 6.082763
# 토마토 orange(7,3)
sqrt(((6-7)^2) + ((4-3)^2)) # 1.414214
# 토마토 celery(3,8)
sqrt(((6-3)^2) + ((4-8)^2)) # 5
# 토마토 cheese(1,1)
sqrt(((6-1)^2) + ((4-1)^2)) # 5.830952

# iris데이터에 대하여 knn 적용

data(iris)
str(iris)
set.seed(415)
idx = sample(1:nrow(iris), 0.7*nrow(iris))
training = iris[idx,]
testing = iris[-idx,]
training
testing
names(training)
# k값의 결정 및 모델 생성
sqrt(length(training$Sepal.Length)) # 10.24695 올림해서 
model.knn = knn(training[, -5], testing[,-5], training$Species, k = 11, prob=TRUE)  # k = 11
model.knn
summary(model.knn)

# 평가
tb <- table(model.knn, testing$Species)
sum(diag(tb))/ sum(tb) # 정분류율(Accuracy) 0.9777778

# 문제 diagnosis ("B" 양성 , "M" 악성)

wdbc_data <- read.csv("wdbc_data.csv")
str(wdbc_data)
wdbc_data$diagnosis <- as.factor(wdbc_data$diagnosis)
wdbc_data <- wdbc_data[-1]

prop.table(table(wdbc_data$diagnosis)) * 100
summary(wdbc_data[,c(2:31)])
# 정규화

wdbc <- as.data.frame(lapply(wdbc_data[2:31], normalize))

class(wdbc_data)
nrow(wdbc_data)
# 데이터 분할
set.seed(415)
idx = sample(1:nrow(wdbc), 0.7*nrow(wdbc))
training_wdbc = wdbc[idx,] # x값
testing_wdbc = wdbc[-idx,]
names(training_wdbc)

training_wdbc_y <- wdbc_data[idx, 1] # y값
testing_wdbc_y <- wdbc_data[-idx, 1]

# k값 결정
dim(training_wdbc)
k <- sqrt(dim(training_wdbc)[1])
k

model.knn = knn(training_wdbc, testing_wdbc, training_wdbc_y, k= 19, prob = TRUE)
model.knn
summary(model.knn)

tb <- table(model.knn, testing_wdbc_y)
sum(diag(tb))/sum(tb) # 정분류율 0.9532164

# 최적의 k값 찾기
k <- 5:25
length(k)

acc <- NULL
arr <- NULL
# acc_v <- c()
for(i in k){
  cat("k=", i, '\n')
  model.knn <- knn(training_wdbc, testing_wdbc, training_wdbc_y, k= i)
  t <- table(model.knn, testing_wdbc_y)
  arr[i] <- i
  acc[i] <- (t[1,1] + t[2,2]) /sum(t)
  cat('분류정확도 =', acc[i], '\n')
  # bind <- data.frame(acc_result = acc , k_result = i)
  # acc_v <- rbind(acc_v, bind)
}
tab <- cbind(arr, acc)
tab
tab <- data.frame(tab)
# acc_v2 <- acc_v[order(-acc+v$acc_result),]
# head(acc_v2,5)
# acc_v2[1,2]

# iris caret패키지
library(caret)
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]
# kernel density estimation 보간법
featurePlot(TrainData, TrainClasses, plot="density", auto.key=list(columns=3))
# 자동튜닝됨. 
modelLookup("knn")
knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center" , "scale"), # z-점수 scaling 
                 tuneLength = 5,
                 trControl = trainControl(method = "cv")
                 )
plot(knnFit1)

knnPredict <- predict(knnFit1, newdata= TrainData)
confusionMatrix(knnPredict, TrainClasses)
mean(knnPredict == TrainClasses) # True = 1 / False = 0

library(e1071)
x <- iris[,-5]
y <- iris[,5]
obj2 <- tune.knn(x, y, k = 5:25, tune.control = tune.control(sampling = "boot"))
summary(obj2)
plot(obj2)












# knn //   train(method = "knn)   비교
weather <- read.csv("weatherAUS.csv", stringsAsFactors=FALSE)

str(weather)
weather <- weather[,c(-1,-2,-8,-10,-11,-22)]
str(weather)

prop.table(table(weather$RainTomorrow)) * 100
weather <- na.omit(weather)
table(weather$RainTomorrow)
weather$RainTomorrow <- as.factor(weather$RainTomorrow)
# 정규화방법 ( min-max로 정규화한 경우가 z점수 정규화보다 더 높은 정분류율을 나타낸다.)

weather_data <- as.data.frame(lapply(weather[1:17], normalize))

class(weather_data)
nrow(weather_data)
# 데이터 분할 
idx = sample(1:nrow(weather_data), 0.7*nrow(weather_data))
training_wdbc = weather_data[idx,] # x값
testing_wdbc = weather_data[-idx,]
names(training_wdbc)

training_wdbc_y <- weather[idx, 18] # y값
testing_wdbc_y <- weather[-idx, 18]

# k값 결정
weather_pred <- NULL
error.rate <- NULL
for ( i in 1:30){
  error.rate[i]<-0
  if(!i%%2){ # 짝수 제외 홀수일 때만
    weather_pred <- knn(train = training_wdbc, test = testing_wdbc, cl = training_wdbc_y, k = i)
    error.rate[i] = mean(weather_pred != testing_wdbc_y) # 오분류율 -> 낮으면 좋다.
  }
}
k.values <- 1:30
error.df <- data.frame(error.rate, k.values)
error.df
ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty = "dotted", color = "red")
# 오분류율 제일 적은 18로 결정

weather_pred <- knn(train = training_wdbc, test = testing_wdbc, cl = training_wdbc_y, k = 18)
confusionMatrix(testing_wdbc_y, weather_pred) # accuracy 0.8494          


#정규화방법 변경 ( z점수 정규화로 처리 : 결과가 min-max보다 안좋음)

weather_x <- as.data.frame(scale(weather[-18]))
idx <- sample(1:nrow(weather_x), nrow(weather_x)*0.7)
weather_train <- weather_x[idx,]
weather_test <- weather_x[-idx,]
# 종속변수는 위의 것을 사용할 것


# k값 결정
weather_pred <- NULL
error.rate <- NULL
for ( i in 1:30){
  error.rate[i]<-0
  if(!i%%2){ # 짝수 제외 홀수일 때만
    weather_pred <- knn(train = weather_train, test = weather_test, cl = training_wdbc_y, k = i)
    error.rate[i] = mean(weather_pred != testing_wdbc_y) # 오분류율 -> 낮으면 좋다.
  }
}
k.values <- 1:30
error.df <- data.frame(error.rate, k.values)
error.df
ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty = "dotted", color = "red")
# 오분류율 제일 적은 26로 결정

weather_pred <- knn(train = weather_train, test = weather_test, cl = training_wdbc_y, k = 26)
confusionMatrix(testing_wdbc_y, weather_pred) # accuracy 0.7772                    

###### caret패키지
library(caret)
KGrid <- expand.grid(k=1:50)
knnFit2 <- train(training_wdbc, training_wdbc_y,
                 method ="knn",
                 tuneGrid=KGrid,
                 trControl = trainControl(method ="cv", number=10))

knnPredict <- predict(knnFit2, newdata =  testing_wdbc)
confusionMatrix(as.factor(knnPredict), as.factor(testing_wdbc_y))
