# 반복문
i <- c(1:10) # vector
i
for(n in i){
  print(n)
}

for(n in i){
  if(n %% 2 != 0)
    print(n)
}

i = 0
while(i < 10){
  i <- i + 1
  print(i)
}

cnt <- 1
repeat{
  print(cnt)
  cnt <- cnt + 2
  if(cnt > 15)
    break
}

sum <- 0
for (i in 1:10){
  sum <- sum + i
}
sum

#foreach 문 (모델 여러개를 고속으로 처리)
install.packages("foreach")
library(foreach)
foreach(i = 1:5) %do% {
  return(i)
}
foreach(i = 1:5, .combine = c) %do% {
  return(i)
}

coin <- c(2, 2, 0, 1, 1, 1, 2, 3, 1, 2, 2, 3, 3, 4, 1, 3, 2, 3, 3, 1, 2, 2, 1, 2, 2)
(coin.freq <- table(coin)) # 도수분포표
length(coin.freq)
plot(0:4, coin.freq, main = "4회 동전던지기 결과", xlab = "앞면이 나온 횟수", ylab = "빈도수", ylim = c(1, 11), type = "l")

(cumfreq = cumsum(coin.freq)) # cumulative 누적 합계

plot(0:4, cumfreq, main = "4회 동전던지기 결과", xlab = "앞면이 나온 횟수", ylab = "빈도수", ylim = c(1, 30), type = "l")

switch(2, "red", "green", "blue")


# 함수
f <- function(df2){
  df2$a = c("a", "b", "c")
}
df <- data.frame(a = c("d", "e", "f"))
df <- f(df) # 리턴 대입 // 값에 의해 매개변수 전달
df


# 재귀호출 : 반드시 종료 조건 존재 #
recursive.factorial <- function(x){
  if(x == 0)
    return(1)
  else
    return(x * recursive.factorial(x-1)) # 10 9 8 7 6 5 4 3 2 1 0
}                                                      48 12 4 2 1
recursive.factorial(10)


# 연산자 함수 string, number,,
`%divisible%` <- function(x, y){
  if(x %% y == 0)
    return(TRUE)
  else
    return(FALSE)
}

10 %divisible% 3
10 %divisible% 2
`%divisible%`(10, 5)

x <- c(5, 6, 7)
y <- c(1, 2, 3)
x %% y # 나머지 연산자
x %/% y # 몫연산자
x %*% y # 내적연산자
x %o% y # outer 연산자
x %in% y # 멤버 연산

gugudan <- function(i, j){
  for(x in i){
    cat("**", x, "단 **\n")
    for(y in j){
      cat(x, "*", y, "=", x*y, "\n")
    }
    cat("\n")
  }
}
i <- c(2:9)
j <- c(1:9)
gugudan(i,j)
# R함수는 명시적으로 리턴하지 않아도 마지막으로 연산한 것이 리턴
pow <- function(x, y = 2){
  result <- x^y
  #print(paste(x, "의", y, "승은 ", result))
}
a=pow(3)
(pow(3,1))

summation <- function(x){
  sumF <- 0
  for(n in x){
    sumF <- sumF + n
  }
  print(sumF)
}
# summation <- function(x){
#result = 0
#for( i in 1:length(x))
#  result <- result + x[i]
# return(result)}
x <- c(2, 5, 8, 10)
summation(x)

iris
NROW(iris)
names(iris)
class(iris)
sapply(iris, class)
boxplot(iris)

# 범주화
install.packages("ggplot2")
install.packages("MASS")
library(ggplot2)
library(MASS)
str(Cars93)
hist(Cars93$MPG.highway)
disc_1 <- Cars93[,c("Model", "MPG.highway")] # 고속도로 주행 연비 : 6단계로 분류
head(disc_1)
range(disc_1["MPG.highway"]) # 20 ~ 50
disc_1 <- within(disc_1, {
  MPG.highway_cd = character(0) # 범주형 변수
  MPG.highway_cd[MPG.highway >= 20 & MPG.highway < 25] = "20~25"
  MPG.highway_cd[MPG.highway >= 25 & MPG.highway < 30] = "25~30"
  MPG.highway_cd[MPG.highway >= 30 & MPG.highway < 35] = "30~35"
  MPG.highway_cd[MPG.highway >= 35 & MPG.highway < 40] = "35~40"
  MPG.highway_cd[MPG.highway >= 40 & MPG.highway < 45] = "40~45"
  MPG.highway_cd[MPG.highway >= 45 & MPG.highway <= 50] = "45~50"
  MPG.highway_cd = factor(MPG.highway_cd, level = c("20~25", "25~30", "30~35", "35~40", "40~45", "45~50"))
})
head(disc_1)
attributes(disc_1$MPG.highway_cd)
table(disc_1$MPG.highway_cd)


tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sum)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, mean)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sd)
ggplot(disc_1, aes(x=MPG.highway_cd, fill = MPG.highway_cd)) + geom_dotplot(binwidth = 0.3)


# 4구간 범주화
dim(disc_1)
dim(disc_1)[1]
disc_1$N <- seq(1:length(disc_1$MPG.highway))
disc_1$N
disc_1 <- within(disc_1, {
  MPG.highway_cd3 = character(0) # 범주형 변수
  MPG.highway_cd3[N <= 23] = "1st_Freq"
  MPG.highway_cd3[N >= 24 & N <= 46] = "2nd_Freq"
  MPG.highway_cd3[N >= 47 & N <= 69] = "3rd_Freq"
  MPG.highway_cd3[N >= 70] = "4th_Freq"
  MPG.highway_cd3 = factor(MPG.highway_cd3, level = c("1st_Freq", "2nd_Freq", "3rd_Freq", "4th_Freq"))
})
head(disc_1)
table(disc_1$MPG.highway_cd3)

tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd3, mean)

# one-hot-encoding
cust_id <- c("c01", "c02", "c03", "c04", "c05", "c06", "c07")
age <- c(25, 45, 31, 30, 49, 53, 27)
cust_profile <- data.frame(cust_id, age, stringsAsFactors = F)
cust_profile

cust_profile <- transform(cust_profile,
                          age_20 = ifelse(age >= 20 & age < 30, 1, 0),
                          age_30 = ifelse(age >= 30 & age < 40, 1, 0),
                          age_40 = ifelse(age >= 40 & age < 50, 1, 0),
                          age_50 = ifelse(age >= 50 & age < 60, 1, 0))
cust_profile

rm(iris)
# 정규화
head(iris)
head(iris[1:4])
# 0 ~ 1 사이의 값으로 변화
min_max_norm <- function(x){ # 매개변수로 열데이터
  (x - min(x)) / (max(x) - min(x))
}
# 열을 대상으로 하는 lapply
class(lapply(iris[1:4], min_max_norm)) # 출력이 list
iris_norm <- as.data.frame(lapply(iris[1:4], min_max_norm)) # Sepal.Length
head(iris_norm)
iris_norm$Species <- iris$Species
head(iris_norm)

# z 점수 정규화
iris$Sepal.Width <- (iris$Sepal.Width - mean(iris$Sepal.Width)) / sd(iris$Sepal.Width)
mean(iris$Sepal.Width)
# 2.034094 / 0.000000000...2034094 => 거의 0 => 0이 표현이 안됨 1 => 0.999999999999999999999999
sd(iris$Sepal.Width)
head(iris)
zNorm <- function(x){
  (x - mean(x)) / sd(x)
}

iris_zNorm <- as.data.frame(lapply(iris[1:4], zNorm))
head(iris_zNorm)
iris_zNorm$Species <- iris$Species

data("iris")
head(iris)
head(iris_zNorm)


####
(A <- c(1:9))
(B <- c(1:12))
(C <- c(1:15))
(my.lst <- list(A, B, C))
lapply(my.lst, sum)

cars
str(cars)
dt <- cars
(lmn_cars <- lapply(dt, min))
(lmn_cars <- sapply(dt,min))
class(lmn_cars)

avg <- function(x){
  (min(x) + max(x)) / 2
}

fcars <- sapply(dt, avg)
fcars

data(iris)
tapply(iris$Sepal.Width, iris$Species, median)

?mtcars
head(mtcars)
tail(mtcars)
summary(mtcars)
str(mtcars)
(exdata <- mtcars[1:10, 1:3]) # subset : slice
class(exdata)
apply(exdata, 2, mean)
(x = apply(exdata, 1, mean))
#plyr
install.packages("plyr")
library(plyr)
x <- data.frame(ID = c(1, 2, 3, 4, 5), height = c(160, 171, 173, 162, 165))
y <- data.frame(ID = c(5, 4, 1, 3, 2), weight = c(55, 73, 60, 57, 80))
(z <- join(x,y,by = "ID")) # left join
(z <- join(x,y,by = "ID", type = "inner"))
(z <- join(x,y,by = "ID", type = "full"))


x = data.frame(key1 = c(1, 1, 2, 2, 3),
               key2 = c('a', 'b', 'c', 'd', 'e'),
               val1 = c(10, 20, 30, 40, 50))
y = data.frame(key1 = c(3, 2, 2, 1, 1),
               key2 = c('e', 'd', 'c', 'b', 'a'),
               val1 = c(500, 400, 300, 200, 100))

(z <- join(x, y, by = c('key1', 'key2')))
(z <- join(x, y, by = c('key1', 'key2'), type = "inner"))
(z <- join(x, y, by = c('key1', 'key2'), type = "full"))
(z <- join(x, y, by = c('key1', 'key2'), type = "right"))
(z <- join(x, y, by = c('key1', 'key2'), type = "left"))

data(iris)
str(iris)

#ddply (데이터세트, 그룹기준, 내부함수, 적용할 함수)
# 내부함수 : transform, mutate, summarize, subset 
# 선형회귀함수 lm 결과 : 기울기(slope), 절편(intercept)
irisffects <- ddply(iris, .(Species), function(df) coef(lm(Sepal.Width~Sepal.Length, data = df)))

irisffects

# 문제 Sepal.Length 에 대해 종별로 평균을 구하시오
tapply(iris$Sepal.Length, iris$Species, mean)

ddply(iris, .(Species), summarise , avg = mean(Sepal.Length))


a <- ddply(iris, .(Species), summarise , avg = mean(Sepal.Length), std = round(sd(Sepal.Length), 2), min = min(Sepal.Length), max = max(Sepal.Length) )


install.packages("googleVis")
library(googleVis)
library(plyr)
data("Fruits")
Fruits
ddply(Fruits, .(Year), transform, average = mean(Sales))
ddply(Fruits, .(Fruit), transform, average = mean(Sales))
ddply(Fruits, .(Year), mutate, average = mean(Sales), avg_per = average / 100) # mutate 대신 transform 넣으면 오류 발생


#subset
data("airquality")
?airquality
head(airquality)
aq <- within(airquality, {
  lOzone <- log(Ozone)
  Month <- factor(month.abb[Month])
  cTemp <- round((Temp - 32) * 5/9, 1)
  S.cT <- Solar.R / cTemp
  rm(Day, Temp)
})
head(aq)

search()
attach(airquality)
transform(Ozone, logOzone = log(Ozone))
search()
detach(airquality)
search() # 패키지 확인
ls() # 변수명 확인

# iris : 다차원 인덱싱도 가능
# 모든 행이고 종이 세토사이고 열이름이 두개에 속하는 경우 출력
iris[, names(iris) %in% c("Sepal.Length", "Species"), iris$Species == "setosa"]
iris[, ! names(iris) %in% c("Sepal.Length", "Species"), iris$Species == "setosa"]

subset(iris, select = c(Sepal.Length, Species))
subset(iris, subset = (Species == "setosa"))
subset(iris, subset = (Species == "setosa"),  select = c(Sepal.Length, Species))

data("mtcars")
View(mtcars)

subset(mtcars, subset = ( cyl <= 6 & cyl >= 4), select = c(mpg, cyl, am))
subset(mtcars, subset = ( cyl == 6 | cyl == 4), select = c(mpg, cyl, am, hp))
subset(mtcars, subset = ( am == 0), select = c(mpg, cyl, am))
subset(mtcars, subset = ( mpg > 20), select = c(mpg, cyl, am))
a <- subset(mtcars, subset = ( am == 0 &  cyl == 6 | cyl == 4 ), select = mpg) # cyl %in% c(4,6)
mean(a$mpg)
subset(mtcars, subset = ( hp >= mean(hp)))


install.packages("reshape2")
library(reshape2)

subject <- c("대한이", "종로구", "에어콘")
time <- c(1, 1, 1)
age <- c(33, 3, NA)
weight <- c(90, NA, 20)
height <- c(2, 3, NA)
(mdata <- data.frame(subject= subject, time = time, age = age, weight = weight, height = height))
melt(mdata) # 데이터가 녹음
melt(mdata, id = 1:2)
melt(mdata, id = c("subject", "time"), measured = c("age", "weight", "height"))
(mdatar = melt(mdata, id=c("subject"), na.rm = TRUE))
dcast(mdatar, time + subject ~ variable)
dcast(mdatar, ... ~ variable)
dcast(mdatar, ... ~ subject)
dcast(mdatar, ... ~ time)

##
names(airquality) <- tolower(names(airquality))
class(airquality)
head(airquality)
summary(airquality)
str(airquality)
nrow(airquality)
(aqm <- melt(airquality, id = c("month", "day"), na.rm = TRUE))

head(aqm)
acast


# 위치 데이터
state_table <-
  data.frame( key=c("SE", "DJ", "DG", "SH", "QD"),
              name=c(" 서울", "대전", "대구", "상해", "칭따오"),
              country=c("한국", "한국", "한국", "중국", "중국"))
state_table
# 년도
month_table <-
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
month_table
# 상품 테이블
prod_table <-
  data.frame(key=c("Printer", "Tablet", "Laptop"),
             price=c(225, 570, 1120))
prod_table 
prod_table["Tablet",]
prod_table[prod_table["key"] == "Tablet"][2]
prod_table1 <- data.frame(printer = 225, Tablet = 570, Laptop = 1120)
gen_sales <- function(no_of_recs){
  loc <- sample(state_table$key, no_of_recs, replace = T, prob = c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace = T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace = T)
  prod <- sample(prod_table$key, no_of_recs, replace = T, prob = c(1, 3, 2))
  unit <- sample(c(1,2), no_of_recs, replace = T, prob = c(10,3))
#  amount <- unit*(prod_table[prod,]$price)
 amount <- unit*1000
  sales <- data.frame(month = time_month,
                      year = time_year,
                      loc = loc,
                      prod = prod,
                      unit = unit,
                      amount = amount)
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

sales_fact <- gen_sales(1000)
str(sales_fact)
head(sales_fact)


(revenue_cube <- 
    tapply(sales_fact$amount,
           sales_fact[,c("prod", "month", "year", "loc")],
           FUN = function(x){return(sum(x))}))
dimnames(revenue_cube)
revenue_cube
# OLAP 기능
revenue_cube[,"1", "2012",]
revenue_cube["Tablet","1","2012",]
scube <- revenue_cube[c("Tablet","Laptop"),c("1","2","3"),,c("SE","DJ")]

(revenue_quiz <-
    tapply(sales_fact$amount,
           sales_fact[,c("year", "month")],
           FUN = function(x){return(sum(x))}))
