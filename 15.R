# Perceptron의 구현 (구성을 알아보기 위해)
x1 <- runif(30, -1, 1) # 균등분포
x2 <- runif(30, -1, 1)
x <- cbind(x1, x2)
Y <- ifelse(x2 > 0.5 + x1, +1, -1) # y값을 결정

plot(x, pch = ifelse(Y > 0, "+", "-"), xlim = c(-1, 1), ylim = c(-1, 1), cex = 2)
abline(0.5, 1)

########################### 위의 결과와 아래의 결과가 같다.

calculate_distance = function(x, w, b){ # forward propagation 순전파
  sum(x * w) + b
}

linear_classifier = function(x, w, b){ # 
  distances = apply(x, 1, calculate_distance, w, b) # x의 값을 함수 calculate_distance를 적용함, 현재의 가중치와 바이어스 
  return(ifelse(distances < 0, -1, +1)) # 분류
}

second_norm = function(x) {sqrt(sum(x * x))} # 정규화

# 학습률은 오차에 적용될 학습 정도 => learning_rate : 큰 경우 학습속도가 빨라지고 작은 경우는 학습속도가 느려져서 데이터와 시간이 많이 필요하다.
perceptron = function(x, Y, learning_rate = 1){ # 가중치와 바이어스를 학습
  w = vector(length = ncol(x)) # 가중치
  b = 0 # 바이어스
  k = 0 # log 출력 횟수 조절
  R = max(apply(x, 1, second_norm)) # 방향값 중 가장 큰 값을 R값으로
  incorrect = TRUE
  plot(x, cex = 0.2)
  while(incorrect){  # TRUE => 무한루프
    incorrect = FALSE # 아리 코드에서 incorrect 값이 변화하지 않으면 종료시켜라
    yc <- linear_classifier(x, w, b) # 예측값
    for(i in 1:nrow(x)){ # 30번 
      if(Y[i] != yc[i]){ # 실제값 != 예측값 
        w <- w + learning_rate * Y[i] * x[i,] # 역전파 (가중치를 수정)
        b <- b + learning_rate * Y[i] * R^2 # 
        k <- k + 1
        if(k %% 5 == 0){
          intercept <- -b / w[[2]] # 절편
          slope <- -w[[1]] / w[[2]] # 기울기
          abline(intercept, slope, col = "red")
          cat("반복 # ", k, "\n")
          cat("계속하기 위해 [Enter]")
          line <- readline() # 잠깐 중지
        }
        incorrect = TRUE # y값의 예측값과 실제값이 같지 않으면 종료하지 마라
      }
    }
  }
  s = second_norm(w) # 방향값을 결정
  return(list(w = w/s, b = b/s, updates = k))
}

(p <- perceptron(x, Y))
(y <- linear_classifier(x, p$w, p$b))
plot(x, cex = 0.2)
points(subset(x, Y == 1), col = "black", pch = "+", cex = 2)
points(subset(x, Y == -1), col = "red", pch = "-", cex = 2)
intercept <- -p$b / p$w[[2]]
slope <- -p$w[[1]] / p$w[[2]]
abline(intercept, slope, col="green")

##################################################

df = data.frame(
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('n', 'n', 'n', 'y', 'y', 'y')) # 종속변수
)
df

library(nnet)
model_net1 = nnet(y ~ ., df, size = 1) # 신경망 모델, size => 가중치 레이어
model_net1 # weights (가중치) => 5개
summary(model_net1)
names(model_net1)
model_net1$wts # 가중치
model_net1$coefnames
model_net1$value # 가중치 decay값 : 감쇠효과 (처음에는 크게 => 점점 작게)
model_net1$fitted.values
model_net1$entropy # TRUE => 최소제곱법 대신에 최대우도법으로 fitting했음 (확률이 높은 것을 선택)
model_net1$softmax # FALSE => softmax를 사용하지 않았음

predict(model_net1, df)
p <- predict(model_net1, df, type="class")
p
table(p, df$y)

####################
# install.packages("devtools")
library(devtools)
library(nnet)
data(iris)
set.seed(123)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training = iris[idx,]
testing = iris[-idx,]

names(training)
nrow(training)
# 모델 생성 : Black Box model 
model_net_iris1 = nnet(Species ~ ., training, size = 1) # 분류 범주 : 3 # 4-1-3 
model_net_iris3 = nnet(Species ~ ., training, size = 3) # 4-3-3
# 평가
predict(model_net_iris1, testing, type = "class")
predict(model_net_iris3, testing, type = "class")
tb <- table(predict(model_net_iris1, testing, type = "class"), testing$Species)
sum(diag(tb)) / sum(tb)
tb <- table(predict(model_net_iris3, testing, type = "class"), testing$Species)
sum(diag(tb)) / sum(tb)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model_net_iris1)
plot.nnet(model_net_iris3)


# dummy변수화한 모델
# softmax = TRUE => 최소제곱법 => cost function : (예측-실제)^2 / 갯수
class.ind(iris$Species) # dummy변수화
m2 <- nnet(iris[, 1:4], class.ind(iris$Species), size = 3, softmax = TRUE, linout = TRUE) # linout = TURE => 3개의 확률값으로 출력
m2
summary(m2)
names(m2)
species_result = predict(m2, newdata = iris[, 1:4], type = "class")
species_result
predt <- table(species_result)
predt
predt / sum(predt)

###################################문제
# 금속의 타입을 6개의 class로 나눈 데이터 
library(mlbench)
data("Glass")
str(Glass)
summary(Glass)
table(Glass$Type)
# 분류모델 신경망 nnet를 이용해서 구현
idx = sample(1:nrow(Glass), 0.7 * nrow(Glass))
training = Glass[idx,]
testing = Glass[-idx,]

model_glass = nnet(Type ~ ., training, size = 25)
summary(model_glass)
pred_table <- table(predict(model_glass, testing, type = "class"),testing$Type)
pred_table
sum(diag(pred_table)) / sum(pred_table) # 0.3846154 정분류율은 범주의 분포가 균형을 이룰 때 효과적인 지표다.
plot.nnet(model_glass)


m2 <- nnet(Glass[, 1:9], class.ind(Glass$Type), size = 50, softmax = TRUE, linout = TRUE)
gpred <- predict(m2, newdata = Glass[, 1:9], type = "class")
predt <- table(Glass$Type, gpred)
mean(Glass$Type == gpred)

#######################
# install.packages("neuralnet")
library(neuralnet)

data(iris)
set.seed(1234)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training_iris = iris[idx,]
testing_iris = iris[-idx,]
dim(training_iris)
dim(testing_iris)

training_iris$Species <- ifelse(training_iris$Species == 'setosa', 1,
                                ifelse(training_iris$Species == 'versicolor', 2, 3))
testing_iris$Species <- ifelse(testing_iris$Species == 'setosa', 1,
                                ifelse(testing_iris$Species == 'versicolor', 2, 3))
head(training_iris); tail(training_iris)                                
str(training_iris)
# min-max 정규화 (normalization)
normal <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalization은 데이터를 나눈 후에 하는 것이 좋다.
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)
str(training_nor)
testing_nor <- as.data.frame(lapply(testing_iris, normal))
model_net = neuralnet(Species ~ ., data = training_nor,
                      hidden = c(6,2), # hyper parameter tuning
                      threshold = 0.01, # 경계선 => 정밀도가 0.01정도보다 넘어가면 무시
                      linear.output = F,
                      act.fct = 'logistic') # binary classification 활성화함수 : 0 ~ 1 사이의 값 (sigmoid함수)
plot(model_net) # fully connected 다 참여함 => 150 x 4    4 x 6    6 x 2   2 x 1
model_net$act.fct

class(testing_nor[-5])
model_result <- compute(model_net, testing_nor[-5])
model_net$weights # 가중치확인
cor(model_result$net.result, testing_nor$Species) # 1 => 0.954282 # hidden 2 =>9591812 # 6,2 => 0.973459

# 문제 : concrete 데이터 
# caret (parameter tuning)

library(neuralnet)
concrete <- read.csv("concrete.csv")
str(concrete)

set.seed(1234)
idx = sample(1:nrow(concrete), 0.7 * nrow(concrete))
training_con = concrete[idx,]
testing_con = concrete[-idx,]
dim(training_con)
dim(testing_con)
str(training_con)
# min-max 정규화 (normalization)
normal <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalization은 데이터를 나눈 후에 하는 것이 좋다.
training_nor <- as.data.frame(lapply(training_con, normal))
summary(training_nor)
str(training_nor)
testing_nor <- as.data.frame(lapply(testing_con, normal))
model_net = neuralnet(strength ~ ., data = training_nor,
                      hidden = c(5,3), # hyper parameter tuning
                      linear.output = T,
                      algorithm = 'rprop+',
                      learningrate = 0.01) # binary classification 활성화함수 : 0 ~ 1 사이의 값 (sigmoid함수)
plot(model_net) # fully connected 다 참여함 => 150 x 4    4 x 6    6 x 2   2 x 1
model_net$act.fct

class(testing_nor[-9])
model_result <- compute(model_net, testing_nor[-9])
model_net$weights # 가중치확인
cor(model_result$net.result, testing_nor$strength)
model_result$net.result

head(concrete)

####### caret 패키지를 이용해서 튜닝
library(car)
library(caret)
str(Prestige)
summary(Prestige) # 소득예측
trainIndex <- createDataPartition(Prestige$income, p = .7, list = F)
Prestige.train <- Prestige[trainIndex,]
Prestige.test <- Prestige[trainIndex,]
modelLookup(model = "nnet")
# size(특징수), decay(부패해간다.)
my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5,6,7)) # 1 5 10 15 실제로는 정확한 곳 근처에서
prestige.fit <- train(income ~ prestige + education, data = Prestige.train, 
                      method = "nnet", maxit = 1000, tuneGride = my.grid, trace = F, linout = 1)


prestige.fit$bestTune
prestige.predict <- predict(prestige.fit, newdata = Prestige.test)
cor(prestige.predict, Prestige.test$income)



