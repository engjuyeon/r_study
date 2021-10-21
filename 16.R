library(lsa)
vec1 = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
vec2 = c(0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0)
cosine(vec1, vec2)
vec3 = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0)
matrix = cbind(vec1, vec2, vec3)
cosine(matrix) # == A dot B
               #    -------
               #    |A| |B|

# install.packages("kernlab")
library(kernlab)
library(proxy)
# +, -, *, / 요소 연산 실행
(x <- matrix(1:16, nrow=4))
sqrt(sum((x[1,]-x[4,])^2)) # 1행과 4행의 거리값 (피타고라스 정리 => 유클리디안 거리)
sqrt(sum((x[,1]-x[,4])^2)) 
(dist <- dist(x, method="euclidean")) # 차의 제곱
(dist <- dist(x, method="minkowski", p=2)) # 민초우스키키 
(dist <- dist(x, method="binary")) 
(dist <- dist(x, method="manhattan")) # 직선거리리 
(simil <- simil(x, method="euclidean")) # 유사도

# 종속변수가 있는 데이터를 이용해서 hclust를 평가
idx = sample(1:dim(iris)[1], 40)
irisSample = iris[idx,]
species <- irisSample$Species # 백업
irisSample # 데이터만 존재 : 비지도학습의 대상
irisSample$Species = NULL
# dist 거리행렬
hc = hclust(dist(irisSample), method="ave") # hierachical 계층적
plot(hc, hang = -1, labels=iris$Species[idx])

rect.hclust(hc, k=3)
(groups = cutree(hc, k=3))
table(groups, species)

clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

# kmeans
set.seed(1)
iris2 <- iris
iris2$Species <- NULL
iris.scaled <- scale(iris2[, -5])
(kmeans.result <- kmeans(iris.scaled, 3, trace = T))
table(iris$Species, kmeans.result$cluster)
plot(iris.scaled[,c("Sepal.Length", "Sepal.Width")], xlim = c(-2, 2), ylim = c(-2, 2), col=kmeans.result$cluster)
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 5:7, pch = 10, cex = 10)
kmeans.result$centers
names(kmeans.result)
print(kmeans.result$totss)
sum(kmeans.result$withinss)
print(kmeans.result$withinss)
print(kmeans.result$betweenss)

############
library(caret)
set.seed(123)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list =F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
training.data <- scale(training[-5])
summary(training.data)

#비지도 학습 : 종속변수 제거
iris.kmeans <- kmeans(training.data[,-5], centers = 3, iter.max = 10000)

#종속변수 결정
training$cluster <- as.factor(iris.kmeans$cluster)
qplot(Petal.Width, Petal.Length, colour = cluster, data = training)
table(training$Species, training$cluster)

# k값을 결정하는 방법
# install.packages("NbClust")
library(NbClust)
nc <- NbClust(training.data, min.nc = 2, max.nc = 15, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]),
        xlab="클러스터수", ylab="기준", main="클러스터 수 확인")

dim(training.data)
wssplot <- function(data, nc = 15, seed = 1234){
  wss <- (nrow(data)-1) * sum(apply(data, 2, var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss) # 그룹내 거리값
    kmeans(data, centers = i)$withinss
  }
  plot(1:nc, wss, type="b", xlab="클러스터수", ylab="ss")
}
wssplot
wssplot(training.data)
training.data <- as.data.frame(training.data)
# 지도학습 : 비지도학습에서 결정된 클러스터링 그룹을 종속변수로 하여 지도학습
modFit <- train(x = training.data[,-5],
                y = training$cluster, # 클러스터 결과가 종속변수
                method="rpart") # 정보기반학습 DT
testing.data <- as.data.frame(scale(testing[-5]))
testClusterPred <- predict(modFit, testing.data)
table(testClusterPred, testing$Species) # 결과가 좋음()


# 문제 : 신체검사 ㄱ ㅕㄹ과를 아무런 정보도 없다고 생각하고 hclustering해보시오

bodycheck <- read.csv("bodycheck.csv", header = T)
str(bodycheck)
head(bodycheck)
bodycheck[, -1]

idist <- dist(bodycheck[,-1]) # 1,1 은 자기 자신이어서 -1
idist # dist 거리행렬
#거리값은 2개의 대산이 필요 => 1부터 시작 -> 14, 2부터 시작 -> 15

hc <- hclust(idist)
hc
plot(hc, hang = -1)
res <- rect.hclust(hc, k = 3, border = "red")

g1 <- subset(bodycheck, 번호 == 10| 번호 == 4 | 번호 == 8 | 번호 == 1 | 번호 == 15)
g2 <- subset(bodycheck, 번호 == 11| 번호 == 3 | 번호 == 5 | 번호 == 6 | 번호 == 14)
g3 <- subset(bodycheck, 번호 == 2| 번호 == 9 | 번호 == 13 | 번호 == 7 | 번호 == 12)

summary(g1)
summary(g2)
summary(g3)

# kmeans
body <-read.csv("bodycheck.csv", header = T)
df <- body[, -1]
df <- na.omit(df)
colnames(df) <- c("handpw", "height", "weight", "glass")
df <- scale(df)
df <- as.data.frame(df)
head(df)
#install.packages("factoextra")
library(factoextra)
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(k2, data = df)
cor(df, method = "pearson")
library(corrgram)
corrgram(df, upper.panel = panel.conf)
df$cluster <- as.factor(k2$cluster)
plot(df$handpw, df$weight , col = df$cluster)
points(k2$centers[,c("handpw", "weight")], col = c(1,2), pch = 8, cex =5)

plot(df$handpw, df$glass, col = df$cluster)
points(k2$centers[,c("handpw", "glass")], col = c(1,2), pch = 8, cex =5)
k2$centers # centers를 보고 그룹의 특징을 해석
# 1그룹 악력이 세고 신장이 크고 체중이 무겁고 안경을 쓴사람 => 눈이 안좋고 신체 발달이 좋은 사람들
# 2그룹 악력이 작고 신장이 작고 체중이 가볍고 안경을 안쓴사람 => 눈은 좋지만 신체적 조건이 불리한 사람들


#
library(ggplot2)
data("diamonds")
str(diamonds)
# price, carat, depth, table 변수 만 대상으로 탐색적, 확인적 군집분석을 실시하시오.
# 데이터 중 1000개만 sampling해서 작업하시오
# 탐색적 군집분석을 실시하고 군집수를 결정하시오
# 가격에 영향을 미치는 요인을 설명해 보시오.
# caret과 price 관계를 군집으로 구분해서 시각화 하시오.


idx = sample(1:nrow(diamonds), 1000)
sample = diamonds[idx,]
sample <- na.omit(sample)
str(sample)
sample <- sample[,c(-(2:4),-(8:10))]
sample <- scale(sample)
sample <- as.data.frame(sample) # KMEANS입력시 데이터프레임으로 변경
head(sample)

result <- hclust(dist(sample), method = "average")
result
plot(result, hang = -1)
res <- hcut(sample, k = 4, stand = T)
fviz_dend(res, rect = T, cex = 0.5, k_colors = c("#00AFBB","#2E9FDF","#E7B800","#FC4E07"))


k2 <- kmeans(sample, 3)
# 자동으로 차원축소해서 변수 특성을 모두 고려한 시각화를 구현
fviz_cluster(k2, data = sample, palette = c("#00AFBB","#2E9FDF","#E7B800"),
             ggtheme = theme_minimal(),
             main = " 클러스터링 시각화")
k2$centers # 그룹의 특성
sample$cluster <- k2$cluster
# 변수상관성 확인
cor(sample, method = "pearson")

# 시각화
plot(sample$carat, sample$price)
plot(sample$carat, sample$price , col = sample$cluster)
points(k2$centers[,c("carat", "price")], col = c(3,1,2), pch = 8, cex =5)

k2$centers # centers를 보고 그룹의 특징을 해석
?diamonds
# carat, table이 클수록 가격이 높아진다. 
# 1그룹 carat이 작고, depth %가 크고, table이 작고 가격이 저렴한 다이아몬드  => carat과 table이 작으면 비교적 다이아몬드의 가격이 저렴해진다.
# 2그룹 carat이 크고, depth %가 크고, table이 크고 가격이 비싼 다이아몬드  => carat이 크면 비교적 다이아몬드의 가격이 비싸진다.
# 3그룹 carat이 작고, depth %가 작고, table이 크고 가격이 저렴한 다이아몬드 => table이 크면 비교적 다이아몬드의 가격이 중간 저렴

#################################
#중심값을 이용해서 그룹의 명명식을 진행
#snsdata에서 3만명의 청소년에 대해 4개의 기본정보 (gradyear, gender, age, friends)
#36개의 관심분야
#성장물 코미디에서 고등학생을 5개의 그룹으로 분류
#범죄성향, 운동성향, 외모지상주의 성향, 무기력, 브레인 등
#성향정보에 대하여 그룹핑을 진행하고 각 그룹의 중심값을 분석해서 5개의 그룹이름으로 clustering 진행


 

teens <- read.csv("snsdata.csv")
str(teens)
head(teens)

# 결측치 포함해서 집계
table(teens$gender,useNA="ifany")
summary(teens$age)
# 청소년의 제외한 데이터는 결측 처리 
teens$age<-ifelse(teens$age>=13&teens$age<20,teens$age,NA)
summary(teens$age)
# 성별 데이터를 세 개의 더미변수로 만들기
# 더미변수는 0과 1값을 가지는 변수 
teens$female<-ifelse(teens$gender=="F",1,0)
# 이렇게 변수를 만들고 여성 비율을 계산할 때 0.7이 나오면 여자가 70퍼
# 즉 이렇게 해야 수치 계산이 쉬워짐!
teens$male<-ifelse(teens$gender=="M",1,0)
teens$no_gender<-ifelse(is.na(teens$gender),1,0)
table(teens$female,useNA="ifany")
table(teens$male,useNA="ifany")
# -> 여전히 변수에 na가 나옴 
# 이유: 값이 비어 있으면 테스트(ifelse문) 자체가 불가능함
table(teens$no_gender,useNA = "ifany")
# is.na함수를 이용해서 ifelse문을 활용했으므로 걸러짐 
# 오류 안나오게 하기
teens$female<-ifelse(teens$gender =="F" & !is.na(teens$gender),1,0)
teens$male<-ifelse(teens$gender =="M" & !is.na(teens$gender),1,0)
# 더 이상 오류 안나오는거 확인 
table(teens$female,useNA="ifany")
table(teens$male,useNA="ifany")
# 연령의 평균  
mean(teens$age,na.rm=TRUE)
# 연령의 성별 분포 
teens$age_fl<-floor(teens$age) # floor함수로 나이의 소수값을 없앤다. 
table(teens$age_fl,teens$gender) # 분포를 구할 때 웬만하면 table함수로 해결된다
# 졸업 연도별로 평균 나이 구하기 
# 졸업연도 데이터를 그룹짓고 이걸 평균 나이로 요약하기 ->
# 2006년 졸업 나이가 쭉 있으면 그 나이를 평균으로 구함 (2007,2008,2009도 동일)
teens %>% group_by(gradyear) %>% summarise(mean_age=mean(age,na.rm=T))
# aggregate(계산될 변수 ~기준 변수, 데이터, 함수, 함수 옵션)
aggregate(age~gradyear,teens,mean,na.rm=TRUE) # gradyear변수를 기준으로 age평균을 계산하고 요약하겠다.
# gradyear를 그룹으로 잡고(2006그룹,2007그룹...) 그룹 개수만큼 age 평균을 만듦
#ave는 그룹의 대표 값을 반환해주는 함수
#ave(그룹 대표 값을 구할 때 필요한 데이터, 그룹, 데이터를 받는 함수)
# x에는 첫 번째 인수가 들어감 
# 왜 function을 이용하였는가?
# na.rm=true라는 옵션을 주기 위해서이다.
# 세 번째 인수에는 함수 이름만 쓰거나 함수를 정의해야만 하도록 설계되어있다.
# teens$gradyear에 대한 대표값: 졸업연도에 해당하는 나이의 평균 
ave_age<-ave(teens$age,teens$gradyear,FUN=function(x) mean(x,na.rm=TRUE)) 
# teens$gradyear가 2006이면 2006년에 대한 teens$age의 평균을 구함
# teens$gradyear가 2007이면 2007년에 대한 teens$age의 평균을 구함
# 결과 예시) teens$gradyear=2006일 때의 대표값은 18이고 2007일 때의 대표값은 19이다!
# gradyear에 대한 age평균을 데이터 개수만큼 만듦 
teens_age<-ifelse(is.na(teens$age),ave_age,teens$age)
# teens$age가 결측값이면 대표값을 쓰고 아니면 일반 값을 씀 
# 데이터 프레임 5열부터 40열만 이용 <행과 열이 있는데 둘 중 하나만 쓴다면 열을 쓰겠다는 것>
interests<- teens[5:40]
#laaply를 써서 interests를 리스트로 만들고 scale함수를 써서 리스트별로 정규화 
interests_z<-as.data.frame(lapply(interests,scale))
interests_z
set.seed(2345) # 난수 고정
# 5개의 클러스터로 구성 (결과는 리스트로 나옴)
teen_clusters<-kmeans(interests_z,5)
teen_clusters$size
teens$cluster<-teen_clusters$cluster
table(teens$cluster)
# teen_clusters$cluster은 데이터마다 어떤 클러스터인지를 나타내는 데이터 프레임!
# 처음 5행의 데이터만 살펴보기 (열은 두 번째 인수에 해당)
teens[1:5,c("cluster","gender","age","friends")]
# 각 클러스터별로 연령 평균 구하기
aggregate(age~cluster,teens,mean)
# 각 클러스터별로 여성의 비율 구하기 
aggregate(female~cluster,teens,mean)
# 각 클러스별로 친구 수 구하기
aggregate(friends~cluster,teens,mean)
# 클러스터별 비중이 높은 단어를 표기하기!!
# teen_clusters$centers는 각 클러스터들 센터 데이터의 모음 
class(teen_clusters$centers)
teen_clusters$centers
# 위 행렬의 각 행(1)을 내림차순으로 조정해서 a행렬을 만듦
a<-apply(-teen_clusters$centers,1,order) # 행렬에 함수 적용해서 행렬 반환하는 함수
a
# a행렬은 36행 5열로 구성되고 내림차순 결과는 teen_clusters$centers의 인덱스로 저장됨
# a행렬 1열은 teen_clusters$centers 1행을 내림차순한 인덱스 결과!
# 즉 teens_clusters$centers[a[1,1]]은 teen_clusters$centers 1행에서 가장 큰 값을 가짐 
# 마찬가지로 teens_clusters$centers[a[2,1]]은 teen_clusters$centers 1행에서 두 번째 큰 값을 가짐
# 하지만 값이 필요한게 아니라 그 값에 대한 열 이름이 필요하므로 colnames함수를 사용함!
result <-NULL
for(i in 1:5){
  result<-cbind(result,colnames(teen_clusters$centers)[a[,i]])
}
result



###################
teens <- read.csv("snsdata.csv")
str(teens)
head(teens)

sum(is.na(teens))
colSums(apply(teens, 2, is.na)) #gender #age
table(teens$gender, useNA = "ifany")
summary(teens$gender)
summary(teens$age)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)

avg <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = T))
head(avg)
teens$age <- ifelse(is.na(teens$age), avg, teens$age)
teens$age[1:6]
teens$age
# colSums(apply(teens, 2, is.na)) #gender #age

table(teens$gender, useNA = "ifany")

interests <- teens[5:40]
summary(interests)

interests_n <- data.frame(lapply(interests, scale))
summary(interests_n)

teen_clusters <- kmeans(interests_n, 5)
names(teen_clusters)

# install.packages("factoextra")
library(factoextra)
fviz_cluster(teen_clusters, interests_n, ellipse.type = "norm")
teen_clusters$cluster[1:5]
teen_clusters$size #  4번 그룹
table(teen_clusters$cluster)

teen_clusters$centers
# 성향 보고 판단
# 1번 그룹 = 

teens$cluster <- teen_clusters$cluster
head(teens)


aggregate(data = teens, age ~ cluster, mean)
qplot(cluster, age, colours = gender, data = teens)

aggregate(data = teens, gender == 'F' ~ cluster, mean)

aggregate(data = teens, sex+drunk+drugs ~ god > 1, mean)
# 개인별로 3가지의 정보를 확인


# 노래를 좋아하면 춤도 좋아할 것이다.
aggregate(data = teens, dance ~ music > 1, mean)
# 노래를 좋아하면 춤도 좋아한다고 볼 수 있다.


# 운동에 관심이 많을 수록 술과 약을 멀리할 것이다.
aggregate(data = teens, drunk + drugs ~ sports > 1, mean)
# 술과 약을 오히려 더 할 것이다.

# 금발에 관심이 많으면 헤어에도 많을 것이다.
aggregate(data = teens, hair ~ blonde > 1, mean)
# 관심이 많다.

# 여자보다는 남자가 운동에 관심이 많을 것이다.
aggregate(data = teens, sports > 1 ~ gender, mean)
# 남자가 좀 더 관심이 많지만 운동의 관심 정도는 성별과 관련이 크게 없다고 볼 수 있다.


# sound : 소리 : signal => 같은 경우가 거의 없다. (시간지연)
# 비교 : DTW(Dynamic time warping)
#install.packages("dtwclust")
library(dtwclust)
data(uciCT)

str(CharTraj)
# 보간법 : 중간에 데이터가 없는 경우 다양한 커널 (함수를 이용해서 데이터 생성 )
series <- reinterpolate(CharTraj, new.length = max(lengths(CharTraj)))
series <- zscore(series) # NaN => 0
series
# time series 시계열 데이터를 클러스터링
pc.dtwlb <- tsclust(series, k = 20L,
                    distance = "dtw_lb", centroid = "pam", # 들어오는파형중에서 선택
                    seed = 3247, trace = TRUE, # 프린트 여부
                    control = partitional_control(pam.precompute = FALSE),
                    args = tsclust_args(dist = list(window.size = 20L)))
plot(pc.dtwlb)

OFFTHERAIL