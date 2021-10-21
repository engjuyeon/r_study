sqrt(5)
sqrt(c(1,2,3,4,5,6,7,8,9,10))
# sqrt(2,3) 이거는 안됨

# 자바 스크립트
square <- function(data){
  return (data^2)
}

square(5)
square(c(1,2,3,4,5))

point <- function(xval, yval){
  return (c(x = xval, y = yval))
}

p1 <- point(5, 6)
p2 <- point(2, 3)

euclidean.distance <- function(point1, point2, square.func){
  distance <- sqrt(
    square.func(point1['x'] - point2['x']) + square.func(point1['y'] - point2['y'])
  )
  return (c(distance = distance))
}

euclidean.distance(p1, p2, square)
                   
# 원점으로부터의 거리

# 데이터 비교
set.seed(100) #컴퓨터의 난수는 의사난수
h_korean <- rnorm(n=1000, mean=170, sd=10) # 정규분포
h_bushman <- rnorm(1000,140,8)
height <- data.frame(h_korean, h_bushman)
rm(h_korean, h_bushman)

head(height)
attach(height)
par(mfrow = c(1,2))
hist(h_korean, freq = TRUE, main = " 한국인 키 빈도 히스토그램") # FREQ
hist(h_korean, freq = FALSE, main = "한국인 키 확률밀도함수 그래프")

hist(h_bushman, freq = TRUE, main = " 부시맨 키 빈도 히스토그램") # FREQ
hist(h_bushman, freq = FALSE, main = "부시맨 키 확률밀도함수 그래프")

# 두 집단 비교
height <- transform(height, z2.h_korean = (h_korean - mean(h_korean))/ sd(h_korean),  z2.h_bushman = (h_bushman - mean(h_bushman))/ sd(h_bushman))  #z점수

height <- transform(height, z.h_korean = scale(h_korean), z.h_bushman = scale(h_bushman))
head(height)
hist(height$z.h_korean, freq=T, main="한국인 표준")
hist(height$z.h_bushman, freq=T, main="부시맨 표준")

# 문제 160cm의 키가 한국인과 부시맨에서 어떤 위치인가 비교해 보시오
# 0.05보다 다 작음
dnorm(160, mean(h_korean), sd(h_korean)) # 0.0237 
(170 - mean(h_korean))/sd(h_korean)
dnorm(160, mean(h_bushman), sd(h_bushman)) # 0.0019 극히 희박 # 3.468813e-05 => 0.0003 극히 희박
(170 - mean(h_bushman))/sd(h_bushman)

library(ggplot2)
ggplot(data.frame(x=c(-3,3)), aes(x=x))+ # 미적요소 x, y, color, size, shape, fill
  stat_function(fun=dnorm, colour="blue", size=1)+ # 통계함수 stat_function
  stat_function(fun=dt, args=list(df=3), colour="red", size=2)+ # stats::dt
  stat_function(fun=dt, args=list(df=1), colour="yellow", size=3)+
  annotate("segment", x=1.5, xend=2, y=0.4, yend=0.4, colour="blue", size=1)+
  annotate("segment", x=1.5, xend=2, y=0.37, yend=0.37, colour="red", size=2)+
  annotate("segment", x=1.5, xend=2, y=0.34, yend=0.34, colour="yellow", size=3)+
  annotate("text", x=2.4, y=0.4, label="N(0,1)")+
  annotate("text", x=2.4, y=0.37, label="t(3)")+
  annotate("text", x=2.4, y=0.34, label="t(1)")+
  ggtitle("정규분포와 t분포")


x = c(1,2,3,4,5,6)
y = c(7,8,9,10,11,12)

(z = table(x))
(z = table(x,y))
class(z)

# 경우의 수를 확인
( d <- data.frame(x = c("1", "2", "2", "1"), y = c("A", "B", "A", "B"), num = c(3, 5, 8, 7)))
(xt = xtabs(num ~ x+y, data=d))
class(xt)
margin.table(xt, 1) #행으로
margin.table(xt, 2) #열로
margin.table(xt)
prop.table(xt,1) # 행방향 확률
prop.table(xt,2)


age <- c(17,18,18,17,18,19,18,16,18,18)
table(age)
barplot(table(age),
        main="10명의 학생의 나이 수",
        xlab="나이",
        ylab="Count",
        border="red",
        col="blue",
        density=10  # 20
        )

Titanic  # 4차원테이블
class(Titanic)
margin.table(Titanic,1) #첫번째 table
margin.table(Titanic,2)
margin.table(Titanic,3)
margin.table(Titanic,4) 

barplot(margin.table(Titanic,1))
barplot(margin.table(Titanic,1))
barplot(margin.table(Titanic,1))
barplot(margin.table(Titanic,1))

(titanic.data = xtabs( ~Survived + Sex, data=Titanic)) # 회수 카운트
(titanic.data = xtabs(Freq~Survived + Sex, data = Titanic))


head(esoph)
str(esoph)
# ncases, ncontrols 를 각각 3개변수에 대하여 테이블 생성하라 ( 요약 - 도수)
# ncases로 한번 보고 ncontrols로 별도로 봐라
xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)
# | 범주형 변수 |
# xtabs를 이용해 다면으로 생성된 데이터 테이블을 하나의 표로 정리해서 보여주는 것 ftable
                # 열 ~ 행
ftable(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph))
ftable(xtabs(cbind(ncases, ncontrols) ~ agegp, data = esoph))

##
?UCBAdmissions
class(UCBAdmissions)
DF <- as.data.frame(UCBAdmissions)
# table(중복이 제거된 상태)에 있는 데이터를 데이터 프레임으로 변경하면 모든 데이터를 풀어서
# 하나의 데이터 프레임으로 보임(중복됨)
class(DF)
xtabs(Freq ~ Gender + Admit, DF)
xtabs(Freq ~ ., DF)
# 카이제곱분석은 table을 만들어 놓고 실행 (table을 만들고 summary를 해도 카이제곱분석을 해준다.)
# 독립성 검정 기대도수 - 관측도수의 분산값이 크다.
summary(xtabs(Freq ~ ., DF))
# 귀무가설 : 기대도수와 관측도수는 비슷하다.
# 대립가설 : 기대도수와 관측도수가 다르다.
# 관측도수 - 기대도수
# --------------------  결과값을 자유도 16의 카이제곱 분포 확률표에서 확인 : p-value 0
#     기대도수
# 유의 수준 0.05
# 귀무가설 기각하고 대립가설 채택한다.

x <- seq(1, 10, .1)
par(mfrow = c(2,3))
# 자유도에 따라 확률이 다르다.
plot(x, dchisq(x,6), type="l")
plot(x, dchisq(x,5), type="l")
plot(x, dchisq(x,4), type="l")
plot(x, dchisq(x,3), type="l")
plot(x, dchisq(x,2), type="l")
plot(x, dchisq(x,1), type="l")
par(mfrow=c(1,1))

# CrossTable
install.packages("gmodels")
library(gmodels)
CrossTable(mtcars$vs, mtcars$am)
# (관측치 - 기대도수)^2  (12-18)^2 /18
# ---------------------
#     기대도수


# BrandA, BrandB, BrandC 에 대하여 60명에게 선호도 조사
#   20     20      20
#   18     23      19
# -------------------------- 이 데이터에 대하여 적합도검증을하시오
# 귀무가설 : 선호도가 동일할 것이다.
# 대립가설 : 선호도가 다르다
# 카이 제곱
CHI <- ((18-20)^2 /20) + ((23-20)^2/20) + ((19-20)^2/20)
dchisq(CHI, 2) # 확률값으로 mapping , 자유도 N-1 = 2 # p-value :  0.352344
# 유의 수준 : 0.05
# 유의 수준보다 p-value가 크기 때문에 귀무가설 기각하지 못한다.
# 물을 동일한 국산을 사용하고 3개의 브랜드가 청주에 몰려있다.



# 사이다 선호도 조사
# 귀무가설 : 사이다에 대한 선호도 차이가 없다.
# 대립가설 : 사이다에 대한 선호도 차이가 있다.
# 유의수준 : 0.05
data<-textConnection(
  "사이다종류 관측도수
  1 14
  2 32
  3 17
  4 9
  5 18"
)

x <- read.table(data, header=T)
class(x)
chisq.test(x$관측도수)
# 검정통계량
# p-value : 0.002603
# 귀무가설을 기각하고 대립가설을 채택한다.

#기대도수 expect
expect <- (14+32+17+9+18)/5
#카이제곱
chi <- ((14-18)^2 / 18) + ((32-18)^2/18) + ((17-18)^2/18) + ((9-18)^2/18) + ((18-18)^2/18)

dchisq(chi, 4)


# 귀무가설 : 성별에 따라 운동량의 차이가 없다
# 대립가설 : 성별에 따라 운동량의 차이가 있다.
library(MASS)
library(gmodels)
str(survey)
View(survey)
head(survey[c("Sex", "Exer")])
xt = xtabs(~ Sex+ Exer, data=survey)
levels(survey$Exer)
CrossTable(survey$Sex, survey$Exer, rcprvyrf=af)
chisq.test(xtabs(~ Sex+ Exer, data=survey))
# 검정통계
# 문제 데이터 셀에 대하여 교차표를 생성
# 부모학력수준과 (level2) 과 자녀대학진항여부(pass)간에 관련성이 있는지 검정
# 귀무가설 : 교육방법에 따른 집단 간의 만족도에 차이 없다.

data <- read.csv("cleanDescriptive.csv", header = T)

head(data)
str(data)
class(data)

x <- data$level2
y <- data$pass2

res <- data.frame(Level = x, Pass = y)
res <- na.omit(res)
dim(res)
head(res)
xt <- xtabs(~ Level+Pass, data=res)
CrossTable(res$Level, res$Pass)
CrossTable(xt)
CrossTable(xt, expected = T)  # expected => p-value
CrossTable(xt, expected = T, chisq = T)
chisq.test(res$Level, res$Pass)
# 0.05 보다 큼 -> 귀무가설 기각하기 어려움 즉 만족도에 차이가 없다고 볼 수 있다.

# 문제 : Treatment : " Placebo " , " Treated " 위약효과, Improved: " None ", " Some ", " Marked "현저하게
# 류마티스 관절염에 대한 처리 데이터
# 귀무가설 : 플라시보효과나 처치나 별 차이가 없다.
# 대립가설 : 플라시보효과와 처치는 차이가 있다.

install.packages("vcd")
library(vcd)
str(Arthritis)
head(Arthritis)

# 1) 데이터를 준비

ArthData <- data.frame(Treatment<- Arthritis$Treatment, Improved<-Arthritis$Improved )
dim(ArthData)
sum(is.na(ArthData))
attach(Arthritis)
search()
detach(Arthritis)
# 2) 유의 수준을 결정하고 가설 설정
# 유의 수준 0.05
# 귀무가설 : 플라시보효과나 처치나 별 차이가 없다.
# 대립가설 : 플라시보효과와 처치는 차이가 있다.

ArthTable <- xtabs(~ Treatment+Improved, data=ArthData)
ArthTable

# 3) 검정통계량 
CrossTable(ArthTable)
CrossTable(ArthTable, expected = T, chisq = T)
CrossTable(ArthTable, chisq = T)



# 4) 유의수준과 비교해서 판단
chisq.test(ArthTable)
# p-value 0.0014값이 유의수준 0.05값보다 작고 카이제곱이 자유도 1, 유의수준 0.05 값인 3.841보다 크기 때문에
# 귀무가설을 기각. 플라시보 효과와 처치는 차이가 있다.



data <- read.csv("homogenity.csv", header=TRUE)
head(data)
nrow(data)
sum(is.na(data))

# 교육방법 (method) (1,2,3 => 방법1, 방법2, 방법3) , 만족도 (survey) 1매우만족 2만족 3보통 4불만족 5매우불만족
# 문제 : 숫자들을 문자데이터로 변환
# 문제 : 동질성 검정 (카이제곱검정과 동일)
# 문제 : 동질성 검정 해석
for(i in 1:nrow(data)){
  if(data$method[i] == 1){
    data$method[i] <- c("방법1")
  }else if(data$method[i] == 2){
    data$method[i] <- c("방법2")
  }else{
    data$method[i] <- c("방법3")
  }
  switch(data$survey[i],
         '1' = data$survey[i] <- c("매우 만족"),
         '2' = data$survey[i] <- c("만족"),
         '3' = data$survey[i] <- c("보통"),
         '4' = data$survey[i] <- c("불만족"),
         '5' = data$survey[i] <- c("매우 불만족"),
  )
}
#for(i in 1:length(data)){
#ifelse(data$method[i] == 1, (data$method[i] <- c("방법1")), 
#       ifelse(data$method[i] == 2, (data$method[i] <- c("방법2")), 
#              ifelse(data$method[i] == 3, (data$method[i] <- c("방법3")), NA)))
#}
data$method[data$method==1] = "방법1" 
data$method[data$method==2] = "방법2"
data$method[data$method==3] = "방법3"

for(i in 1:nrow(data)){
  switch(data$survey[i],
         '1' = data$survey[i] <- c("매우 만족"),
         '2' = data$survey[i] <- c("만족"),
         '3' = data$survey[i] <- c("보통"),
         '4' = data$survey[i] <- c("불만족"),
         '5' = data$survey[i] <- c("매우 불만족"),
  )
}

x <- data$method
y <- data$survey

CrossTable(x, y, chisq = TRUE)

chisq.test(x,y)

chisq.test(data$method, data$survey)      
# X-squared = 6.5447, df = 8, p-value = 0.5865 > 0.05
# 동질성 검정 해석
# 교육방법별로 만족도에 차이가 없다고 볼 수 있다.