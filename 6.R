# 데이터의 정보 파악
data(iris)
head(iris)
str(iris)
summary(iris)
dim(iris)
levels(iris$Species)


score4 <- c(3,3,6,7,7,10,10,10,11,13,30)
range(score4) # 범위
IQR(score4) # 1/4분위수범위

(n<-length(score4))
(max(score4)-min(score4))
range(score4) # 2개의 값
diff(range(score4))
mean(score4)
median(score4)
sum(score4) 
sort(score4) # 정렬
order(score4) # 위치
var(score4) # 분산
sd(score4) # 표준편차


total <- sum( (score4-mean(score4))*(score4-mean(score4)) )
total/length(score4)  # 49.27273 N


var_val <- total/(length(score4)-1) # n-1
sqrt(var_val)
sqrt(total/(length(score4)-1))

quantile(score4)
quantile(score4, 0.25)
fivenum(score4)
fivenum(score4)[4] - fivenum(score4)[2] # IQR(score4)   min 1사분위 median 3사분위 max
quantile(score4)[4]-fivenum(score4)[2]

table(score4) # 도수 분포표표

summary(score4)

lowercut = fivenum(score4)[2] - ( IQR(score4) * 1.5 ) # 이상치의 하한선
uppercut = fivenum(score4)[4] + ( IQR(score4) * 1.5 ) # 이상치의 상한선

score4>uppercut
score4<lowercut

boxplot(score4) # 30

weatherAUS <- read.csv("weatherAUS.csv") # read.csv(file.choose())
str(weatherAUS) # 36881 obs of 24 variables, 데이터 타입 확인
is.na(weatherAUS)  # NA값
sum(is.na(weatherAUS))  # NA값 수 행 기준 75908 : 한 행에 있는 NA를 보이는대로 1개씩 인식
# 이상치는 숫자로만 확인
weatherAUS <- na.omit(weatherAUS) # omit <- 결측값이 들어있는 행 전체를 데이터 셋에서 제거
sum(is.na(weatherAUS))
str(weatherAUS) # 17378 obs of 24 variables

# 문자가 아닌 애들만 이상치 처리 
# select(weatherAUS, MinTemp, MaxTemp....) 이 방법 대신
# apply(weatherAUS, 2, is.numeric) 
str(select_if(weatherAUS, is.numeric)) # 이 방법
outdata <- select_if(weatherAUS, is.numeric)
str(outdata)                     
# 이상치 확인을 위해 각 열 별로 IQR를 계산하고 3사분위수 1사분위수에 1.5*IQR값을 계산해주어야한다.
for(i in 1:ncol(outdata) - 1){
  fivenum(outdata[,i])
  uppercut = fivenum(outdata[,i])[4] + 1.5 * IQR(outdata[,i])
  lowercut = fivenum(outdata[,i])[2] - 1.5 * IQR(outdata[,i])
  out <- filter(outdata, outdata[,i] <= uppercut, outdata[,i] >= lowercut)
}
str(out)
# 변동계수
data(trees)
dim(trees)
head(trees)
summary(trees)
sd(trees$Volume)/mean(trees$Volume) # 평균 정규화
# 표준오차 : 표본 수가 많아지면 표준편차는 점점 줄어듬 표본수 고려 sqrt(n) 제곱근함수

x = c(0, 1, 2, 3, 4 ,5 ,6 ,7 ,8 ,9 ,10 ,11)
(sem <- sd(x)/sqrt(length(x)))
# 95% 신뢰구간 : 평균의
mean(x)
c(mean(x) - (2 * sem), mean(x)+ (2 * sem))
# 99% 신뢰구간
c(mean(x) - (3 * sem), mean(x)+ (3 * sem))

# 표준오차
data(trees)
stderr <- function(x) sd(x, na.rm = T) / sqrt(length(na.omit(x)))

(result = apply(trees, 2, stderr)) # 1이면 행, 2면 열방향
result[1]
# R프로그램이 쉽다고 하는 것은 벡터와 연산, BOOLEAN indexing 지원
# 벡터로 들어감 : 벡터단위로 함수작성
# 95% 신뢰구간
conf95 <- function(x) c(mean(x, na.rm=T) - (2*stderr(x)), mean(x) + (2*stderr(x)))
apply(trees, 2, conf95)

# 적률
# 분산 = (데이터 - 평균)^2 어디서부터 어디까지 데이터가 있는지
# 왜도 = (데이터 - 평균)^3 0을 기준으로 하여 좌우대칭의 정도
# 첨도 = (데이터 - 평균)^4 데이터 분포가 올라간 정도(기본3)보다 크면 뾰족, 작으면 완만 

install.packages("moments")
library(moments)
set.seed(1234)
n.sample <- rnorm(n= 10000, mean = 55, sd = 4.5) # random + norm 분포(정규분포)
skewness(n.sample) # 왜도 기준 0.
kurtosis(n.sample) # 첨도 기준 3.

install.packages("prob")
library(prob)
# 표본 공간
tosscoin(1)
(s= tosscoin(3, makespace='TRUE')) # 2x2x2 = 8 1/8
1/8

rolldie(1)
rolldie(2, nsides=4)
(rolldie(2, nsides=4, makespace="TRUE"))

cards() # 52장
cards(jokers="TRUE")
cards(makespace = 'TRUE')
tail(cards())
head(cards())

roulette()
roulette(european="TRUE", makespace = "TRUE")

library(prob) # 항아리
urnsamples(1:3, size=2, replace = TRUE, ordered = TRUE) # 복원추출
urnsamples(1:3, size=2, replace = FALSE, ordered = TRUE) # 비복원추출
urnsamples(c("A", "B", "C"), size=2, replace = FALSE, ordered = TRUE)
urnsamples(c("A", "B", "C"), size=2, replace = FALSE, ordered = FALSE)

s<-tosscoin(2, makespace = TRUE)
s
s[1:3]

c<-cards()
c
subset(c, suit=="Heart") # 카드 종류가 Hearts 인 경우
subset(c, rank %in% 7:9) # rank가 7~9사이에 속할 때


s<-cards(makespace = TRUE)
s
a = subset(s, suit=="Heart") # 카드 종류가 Hearts 인 경우
a
b = subset(s, rank %in% 7:9) # rank가 7~9사이에 속할 때

# 카드를 한장 뽑았는데 Heart이거나 7:9가 나올 표본공간은
union(a,b)


# 카드를 한 장 뽑았는데 Heart이면서 7:9가 나올 표본공간은
intersect(a,b)
Prob(a) # clober, diamond, heart, spade 13개(A, 2~10, J,Q,K)
Prob(s, suit=="Heart")
Prob(b)
Prob(s, rank %in% 7:9) # 12/52

Prob(union(a,b)) # 중복 제거됨

subset(s, rank %in% 7:9)
# 카드를 뽑앗는데 7,8,9 중에 나왔다 이 중에서 하드카드가 나올 확률? # 조건부확률
Prob(s, suit=="Heart", given = rank %in% 7:9) # 3/12

Prob(a) + Prob(b) - Prob(intersect(a,b)) # 교집합 , 중복 제거해줌

# 주변 확률, 결합 확률, 조건부 확률

0.67
0.44
0.56
0.35
0.23

# 숫자카드를 뽑았는데 spade, heart이면서 5번일 확률 # 결합확률
(26/52) * (1/13)
s = cards(makespace = TRUE)
quiz1 <- subset(s, suit=="Spade" | suit=="Heart")
quiz2 <- subset(s, rank == 5)
Prob(intersect(quiz1, quiz2))

# 2,3이 선택되엇고 그 중 diamond확률 # 조건부확률
Prob(s, suit=="Diamond", given = rank %in% 2:3)

# 확률을 확인하기 위해서 모집단 분포를 알아야 한다.
# 표본이 30개 이상이면 정규분포를 따른다. # 중심 극한의 원리
rnorm(1, 64.5, 2.5)
rnorm(5, 64.5, 2.5) # 5개를 뽑

(x <- rnorm(1000, 64.5, 2.5))
(x <- sort(x))
(d <- dnorm(x, 64.5, 2.5)) # 확률
par(mfrow=c(1,4))
hist(x, probability =  TRUE, main = "한국 남자들 몸무게 분포")
plot(x, d, main="한국 남자들 몸무게 분포", xlab="몸무게")
curve(dnorm(x), -3, 3)
plot(density(rnorm(10000,0,1))) # 표준 정규분포에서 데이터 획득
par(mfrow=c(1,1))


pnorm(0) # 누적확률 (반 확률)
pnorm(1)
pnorm(2)
pnorm(3)


(pnorm(1)-pnorm(0))*2 
(pnorm(2)-pnorm(0))*2
(pnorm(3)-pnorm(0))*2

#dnrom 분위수에 확률값
dnorm(0,0,1)
dnorm(1,0,1)
dnorm(2,0,1)
qnorm(0.3989423,0,1)
# rnorm, dnorm, pnorm(누적확률 0.5을 더해서 ), qnorm( 확률 -> 분위수)

# Z점수 : (관측값 - 평균) / 표준편차
# 평균이 100이고 표준편차가 10인 정규분포에서 50이 나올 확률?

dnorm(50, mean=100, sd=10) # dnorm(50, 100, 10)
dnorm((50-100)/10) # z점수로 바꿔서 입력 => 표준정규분포

# 문제 x ~ (평균 300, 50)인 정규분포에서 P(x>= 370)일 확률


pnorm(370, 300, 50, FALSE)

(x<- rnorm(300, 0, 1))
curve(dnorm(x,0,1), -4, 4, xlab="z", ylab="f(z)")
z = seq(((370-300)/ 50), 4, 0.01)
lines(z, dnorm(z), type="h", col="gray")
points(((370-300)/50), dnorm((370-300)/50))



# 문제 브랜드의 백열전구의 수명이 1500시간의 평균값과 75시간의 표준편차로 정규적으로 분포되었다.

# 1) 백열전구가 1410시간보다 덜 오래 갈 확률?



pnorm(1410, 1500, 75)

# 2) 백열전구가 1563 ~ 1648 시간에 오래 갈 확률?
pnorm(1648, 1500, 75) - pnorm(1563, 1500, 75)

# 3) 위의 구간을 정규분포 곡선으로 그려 표시
(x<- rnorm(10, 0, 1))
curve(dnorm(x,0,1), -4, 4, xlab="z", ylab="f(z)")
zvalue = ((1648-1500)/ 75) - ((1563-1500)/75)
z = seq(zvalue, 4, 0.01)
lines(z, dnorm(z), type="h", col="gray")


# 문제
# 생후 18개월 이상된 황수 무게 평균 500kg 표준 편차 50kg
# 무거운 순서대로 5%에 해당되는 황소 선발
# 몇 kg이상 황소를 선발??

qnorm(0.05,500,50, FALSE)
# 582.2427kg 이상 황소

