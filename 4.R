install.packages("tidyverse")
library(tidyverse)
tidyverse_conflicts()
tidyverse_update()

library(tibble)
df <- tibble(
  x = runif(5),   # random + uniform 균등 분포 (1, 100, 1000이 나올 확률이 동일)
  y = rnorm(5)    # random + norm 정규분포 (확률이 다름 0 근처의 수가 나올 확률이 높음)
)
df$x
df[["x"]]


library(dplyr)
View(df)
rm(list=ls())
# tibble 생성 방법
class(mtcars)
(mtcars_tbl <- as_data_frame(mtcars))
(mtcars_tbl <- tbl_df(mtcars))
(mtcars_tbl <- as.tibble(mtcars))
class(mtcars_tbl)

install.packages("ggplot2")
library(ggplot2)
head(diamonds)
(df.diamonds_idel <- filter(diamonds, cut == "Ideal")) # 행선택
head(df.diamonds_idel)
(df.diamonds_idel <- dplyr::select(df.diamonds_idel, carat, cut, color, price, clarity))
head(df.diamonds_idel)
(df.diamonds_idel <- mutate(df.diamonds_idel, price_per_carat = price/carat))
head(df.diamonds_idel)
summarise(df.diamonds_idel, avg_price = mean(price, na.rm = TRUE))

# 데이터를 한 방향으로 전달한다.( 전명령어 처리 결과가 )
df.diamonds_idel_chained <- diamonds %>% # 데이터가 흘러가는 통로
  filter(cut== "Ideal") %>% 
  dplyr::select(carat, cut, color, price, clarity) %>% 
  mutate(price_per_carat = price/carat)

head(df.diamonds_idel_chained)  

(df.disorderderd_data <- data.frame(num_var = c(2,3,5,1,4)))

head(df.disorderderd_data)
arrange(df.disorderderd_data, num_var)# 정렬
arrange(df.disorderderd_data, desc(num_var))

head(diamonds)
diamonds %>% 
  filter(cut == "Ideal") %>% 
  ggplot(aes(x = color, y = price)) + # ggplot은 layer개념 처리 + #
  geom_boxplot()

diamonds %>% 
  filter(cut == "Ideal") %>% 
  ggplot(aes(price)) +
  geom_histogram() +
  facet_wrap(~ color)

data("iris")
class(iris)
head(iris, 6)
my_data <- as_data_frame(iris) # tibble
class(my_data)
summary(my_data)
iris %>% 
  group_by(Species) %>% # group_by 다음에는 집계함수
  summarise(meanSepLength = mean(Sepal.Length)) %>% 
  ggplot(aes(Species, meanSepLength)) + geom_bar(stat = 
                                                   "identity")
head(iris)

library(purrr)
iris %>% 
  mutate(x = 10000 * Sepal.Length) %>% # 파생변수
  select(Species, x) %>% 
  map_dfr(., ~format(.x, big.mark = ',')) # format이라는 함수를 적용, 함수형 프로그래밍

(by_cyl <- mtcars %>% group_by(cyl))
by_cyl %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
by_cyl %>%  filter(disp == max(disp))


by_vs_am <- mtcars %>%  group_by(vs, am)
by_vs <- by_vs_am %>%  dplyr::summarise(n = n()) # n() 카운트 함수
by_vs
by_vs %>%  summarise(n = sum(n))

by_vs %>% 
  ungroup() %>% # 그룹 해체
  dplyr::summarize(n = sum(n))
by_vs

by_cyl %>% 
  group_by(vs, am) %>% 
  group_vars()

library("tidyr")

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks
gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)

str(USArrests)
(my_data <- USArrests[c(1, 10, 20, 30),])
(my_data <- cbind(state = rownames(my_data), my_data))
my_data2 <- gather(my_data, # melt
                   key = "arrest_attribute",
                   value = "arrest_estimate",
                   Murder:UrbanPop)
my_data2

my_data3 <- spread(my_data2, # dcast
                   key = "arrest_attribute",
                   value = "arrest_estimate")
my_data3

my_data4 <- unite(my_data, # 변수 간의 값을 합치는 것
                  col = "Murder_Assault",
                  Murder, Assault,
                  sep = "_") # seperator
my_data4
separate(my_data4,
         col = "Murder_Assault",
         into = c("Murder", "Assault"),
         sep = "_")

# 문제
# mtcars에 대해 mpg를 class별로 그룹핑하고 disp, hwy 값의 중위수로 요약
data("mpg")
View(mtcars)
mt_data <- mpg %>% 
  group_by(class) %>% 
  summarise(displ_median = median(displ), hwy_median = median(hwy))

mpg %>% 
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) + # 평균, 분산, 표준편차, 표준오차(sqrt(n)), 변동계수
  geom_smooth() # 비선형 회귀 분석 : 신뢰구간 평균 +- 표준오차 2배수 == 95%신뢰구간
# 추정 : 점추정 - 틀릴 확률 높아짐
# 구간 추정 : 표준오차의 3배수 : 99% 신뢰구간

install.packages("hflights")
library(hflights)
head(hflights)
?hflights
dim(hflights)
library(data.table)
# plyr(data.frame) -> dplyr(tribble) -> dtplyr -> data.table(대용량데이터)
#                       주로사용

library(dtplyr)
hflights_df <- tbl_df(hflights)
hflights_df <- hflights
class(hflights_df)
arrange(hflights_df, Month, DayofMonth, desc(AirTime))
# 도착 지연 시간의 평균 (분)
summarize(hflights_df, delay = mean(ArrDelay, na.rm = T))
planes <- group_by(hflights_df, TailNum) #TailNum[3,320]
planes
# 목적지별로 그룹핑
destinations <- group_by(hflights_df, Dest)
destinations
# TailNum 별로 목적지에 대한 운항횟수
dplyr::summarise(destinations, planes = n_distinct(TailNum), flights = n())
# 년월일 별로 운항횟수
daily <- group_by(hflights_df, Year, Month, DayofMonth)
per_day <- dplyr::summarize(daily, number_flights = n())
per_day


# 년월일별로 지연정보를 그룹핑해서 정리한 후 30분을 초과하는 정보에 대하여 필터링 
a1 <- group_by(hflights, Year, Month, DayofMonth)
a2 <- dplyr::select(a1, Year:DayofMonth, ArrDelay, DepDelay)
a3 <- dplyr::summarise(a2, arr = mean(ArrDelay, na.rm = TRUE), dep = mean(DepDelay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)
a4
# 문제 위 명령을 pipe를 이용한 chaining 방법으로 전환
a <- hflights %>% 
  group_by(Year, Month, DayofMonth) %>% 
  dplyr::select(Year:DayofMonth, ArrDelay, DepDelay) %>% 
  dplyr::summarise(arr = mean(ArrDelay, na.rm = TRUE), dep = mean(DepDelay, na.rm = TRUE)) %>% 
  filter(arr > 30 | dep > 30)

View(hflights)
?hflights
# 비행편수를 구하기 위해 항공기별로 그룹화하시오
# 그룹화한 결과를 항공기별 횟수, Distance의 평균, Arrdelay의 도착지연 시간 평균을 요약하시오
# 위 결과에서 횟수가 20 초과, 거리의 평균이 2000 이하인 데이터만 출력하시오
# 위의 요약결과의 결측치 처리 : 위 결과값이 dist와 delay가 모두 1 이상인 경우만 필터링하시오
planes = group_by(hflights, TailNum)
pleansInfo = summarize(planes, count = n(), DistanceMean = mean(Distance, na.rm = T), ArrdelayMean = mean(ArrDelay, na.rm = T))
result = filter(pleansInfo, count > 20, DistanceMean  <= 2000)
filter(result, DistanceMean > 1, ArrdelayMean > 1)
# 평균 비행시간
summarize(hflights_df, air_mean = mean(AirTime, na.rm = T))
# 도착시간의 표준편차와 분산
summarise(hflights_df, arr_sd = sd(ArrTime, na.rm = T), arr_var = var(ArrTime, na.rm =  T))

flights <- read.csv("flights.csv", stringsAsFactors=F)
str(flights)
flights$date <- as.Date(flights$date)
flights <- na.omit(flights) # 행으로 제거
attach(flights)
sum(is.na(flights)) # 행단위로 출력
# 출발지연 60이상
filter(flights, dep_delay >= 60)

# 출발지연, 도착지연시간만 선택 출력
select(flights, dep_delay, arr_delay)
select(flights, ends_with("delay"))
# day, hour, minute 순 정렬
arrange(flights, date, hour, minute)
str(flights)

# 문제 dep_delay / arr_delay 차이 역순으로 출력
arrange(flights, desc(dep_delay - arr_delay))

# 문제 dep_delay 시간이 있는 것만 찾아서 date, hour 별로 그룹핑하고 그룹별로 평균지연시간과 개수를 카운팅하고 갯수가 10개 이상인 것만 출력
View(flights)
quiz <- flights %>%
  filter(!is.na(dep_delay)) %>% 
  group_by(date, hour) %>% 
  summarise(delayMean = mean(dep_delay), delayCount = n()) %>% 
  filter(delayCount >= 10)

View(quiz)

quiz2 <- flights %>% 
  group_by(dest) %>% 
  summarise(arrMean = mean(arr_delay), count = n()) %>% 
  arrange(desc(arrMean))


install.packages("modelr")
library(modelr)
# 과적합(일반화가 안됨)을 방지하기 위해 무작위로
# train accuracy는 높고 test의 accuracy가 낮으면 과적합
# train accuracy가 낮으면 : 데이터가 부족.( 변수를 늘리면 정확, 데이터를 늘리면 ,)
# train validation / test
# 32 X 11
ex <- resample_partition(mtcars, c(test = 0.3, train = 0.7)) # resample partition 무작위 순서로 한다.
class(ex)
lapply(ex, dim)
dim(mtcars)
boot <- bootstrap(mtcars, 100)
dim(boot) # 변수 2개의 조합으로 bootstrapping 데이터를 증강
# cross-validation :5덩어리로 나눠 돌아가면서 테스트와 학습 진행
cv1 <- crossv_kfold(mtcars, 5)
dim(cv1)
# Monte Calo 무작위
cv2 <- crossv_mc(mtcars, 100)

# 선형회귀
mod <- lm(mpg ~ wt, data = mtcars) # 모델에 의해 예측된 값 - 실제 값 - 잔차에러
#mean square error
mse(mod, mtcars)
rmse(mod, mtcars)
mae(mod, mtcars) # mean absolute error 절대값으로 부호를 떼엇을 때의 평균에러

# data.table => 대량의 데이터를 고속으로 처리할 때 필요한 패키지
library(data.table)
titanic <- read.csv("titanic3.csv", fileEncoding = "UTF-8")
str(titanic)
class(titanic)
titanic.dt <- data.table(titanic)
class(titanic.dt)
str(titanic.dt)
head(titanic.dt)
setkey(titanic.dt$pclass)
tables()# table 도수 분포표
setkeyv(titanic.dt, c("sex", "pclass"))
tables()
titanic.dt[pclass==1,]
titanic.dt[pclass==2]
class(titanic.dt[pclass==2])

# 인덱싱을 이용해서 데이터 처리 가능
titanic.dt[, mean(survived), by = "pclass"] # grouping
titanic.dt[,length(which(pclass=="1"))]
titanic.dt[pclass=="1",.N]
titanic.dt[,mean(survived), by="sex"]
titanic.dt[pclass=="1", mean(survived), by="sex"]
titanic.dt[pclass=="1", .N, by="sex"]

# 문제 1등급 승객 중 20세 이상인 성인 비율
titanic.dt[pclass=="1", length(which(age>=20))/.N, by="sex"]

# 정리 문제
# 아래의 data.frame이 완성될 수 있게 4번 5번을 완성하시오.
# empno pay  bonus
# 101   250  0.10
# 102   180  0.10
# 103   200  0.12
# 104   300  0.15
# 105   1000 0.00

# 데이터 생성 위해 empno, pay, bonus 변수 생성
empno <- c(101, 102, 103, 104, 105)
pay <- c(250, 180, 200, 300, 1000)
bonus <- c(0.10, 0.10, 0.12, 0.15, 0.00)
# 표의 형식대로 pay201801 데이터프레임 생성
pay201801 <- data.frame(empno, pay, bonus)

pay201801
# total 컬럼 추가. 아래의 순번에 내용 작성
# 1) 계산 :      total <-
total <- c(pay * (1 + bonus))

# 2) 함수 :      pay201801 <-          (pay201801, total)
pay201801 <- mutate(pay201801, total)

# dplyr패키지의 관련함수를 이용하여 총급여 300 이상인 사원번호와 총급여 출력
pay201801 %>% 
  select(empno, total) %>% 
  filter(total >= 300)

# 부서번호(deptno) 추가
# deptno
# 1
# 2
# 1
# 2
# 2

pay201801$deptno <- c(1, 2, 1, 2, 2)

# 부서별 급여 평균을 출력
# A tibble : 2 x 2
# deptno      mean_total
# <dbl>         <dbl>
#   1      1     250.
#   2      2     514.

payavg <- pay201801 %>% 
          group_by(deptno) %>% 
          summarise(mean_total = mean(total)) %>% 
          select(deptno, mean_total)
payavg      
# 그래프
pay201801 %>% 
  ggplot(aes(empno, pay)) + geom_bar(stat = "identity")

