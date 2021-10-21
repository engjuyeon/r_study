# 자기상관성(auto regression) -> 한 데이터에서 이전의 데이터가 나중의 데이터에 영향을 미치는것
# 평활법 : moving average(이동평균), 지수평활법법
# ARMR : 정상적인 시계열에 적용
# ARIMA : 비정상적인 시계열에 적용
# 차분 : 평균이 일정하지 않을 때 사용, 미분과 같다. 여러 번 할 수도 있다.

# 시계열분석절차
# 시계열 자료 객체 생성
# 시계열 자료 특성분석
# - 확인 : ACF, PACF, CV
# - AUTO.ARIMA(아리마 모형의 AR, diff횟수, MA에 대항 값 결정)
# - 안정적 시계열 파악 (diff나 log로 안정적으로 변환)
# 시계열 예측 모형
# - ARIMAX/TBATS/HOLT-Winters/Neural Network
# 모형 타당성 평가(유의성 검정)
# - 모형 적합도에 따른 잔차의 정상성 및 분석결과 검정
# - 시계열 자료의 상관분석 및 검정
# 최적 모형 선정
# 미래 값 예측




# 시계열 분석의 평활법
vts <- c(1325,1353,1305,1275, 1210, 1195)
mean(vts) # 단순 이동 평균법
mean(vts[1:5])*.4 + vts[6] * .6 # 가중 이동 평균법
mean(vts[1:3])*.4 + mean(vts[4:6]) * .6
mean(vts[1:3])*.3 + mean(vts[4:6]) * .7
mean(vts[1:3])*.2 + mean(vts[4:6]) * .8
((vts[4]*4) + (vts[3]*3) + (vts[2]*2) + (vts[1] * 1)) / (4+3+2+1)

# install.packages("TTR")
library(TTR) # Technical Trading rules
data(ttrc)
class(ttrc)
str(ttrc) # OHLC(Open, High, Low, Close, Volume => 개장가, 고가, 저가, 종가, 거래량)
sma.20 <- SMA(ttrc[,"Close"], 20) # simple moving average
ema.20 <- EMA(ttrc[,"Close"], 20) # exponential moving average
wma.20 <- WMA(ttrc[,"Close"], 20) # weighted moving average
class(sma.20)
ema.20
wma.20
par(mfrow = c(1,1))
plot(ttrc$Open)
plot(wma.20)
plot(ema.20)

# time series 데이터 생성
a <- ts(1:20, frequency = 12, start = c(2011, 3))
print(a)
str(a)
attributes(a)

kings <- scan("kings.dat")
class(kings)
kingstimeseries <- ts(kings)
kingstimeseries
plot.ts(kingstimeseries)

# 뉴욕 월별 출생일 수
births <- scan("births.dat")
birthstimeseries <- ts(births, frequency = 12, start = c(2008,1))
birthstimeseries
plot.ts(birthstimeseries)

# 기념품점의 매출액
# fancy.dat 파일 읽어서 1987, 1월부터 배치해보시오
fancy <- scan("fancy.dat")
fancytimeseries <- ts(fancy, frequency = 12, start = c(1987,1))
fancytimeseries
plot.ts(fancytimeseries)
logfancytimeseries <- log(fancytimeseries)
logfancytimeseries


plot.ts(kingstimeseries)
kingsSMA3 <- SMA(kingstimeseries, n = 3)

plot.ts(kingsSMA3)
kingsSMA8 <- SMA(kingstimeseries, n = 8)
plot.ts(kingsSMA8)

# install.packages("tseries")
library(tseries)
?kpss.test # 안정성, null hypothesis that x is level or trand stationary.
# 귀무가설이 안정적이다. 대립가설이 비안정적이다. , 정상적 시계열 데이터인지를 판별
kpss.test(kingsSMA8)
# p-value = 0.04 귀무가설 기각하고 대립가설을 채택 => 비안정적이다
kingsdiff <- diff(kingstimeseries, differences = 1) # 1차 차분분
kpss.test(kingsdiff) # p-value 안정적 -> 정상성 회복

acf(kingsdiff, lag.max=20)
pacf(kingsdiff, lag.max=20)        

#install.packages("forecast") # ARIMA
library(forecast)
auto.arima(kings) # Series: kings 
                  # ARIMA(0,1,1) 

kingsARIMA <- arima(kingstimeseries, order = c(0,1,1))
kingsARIMA
# 신뢰구간을 가진 앞으로의 5기에 대하여 예측
kingsforecatst <- forecast(kingsARIMA, h=5)
plot(kingsforecatst)

plot.ts(kingsforecatst$residuals) # 잔차가 많이 발생하고 있다.

# 분해시계열법
birthsdecompose <- decompose(birthstimeseries)
names(birthsdecompose)
birthsdecompose$random # 불규칙성
birthsdecompose$figure # the setimated seaonal figure only
birthsdecompose$trend# 추세 변동
birthsdecompose$seasonal# 계절적 요인
birthsdecompose$type # additive 가법적 모형으로 분석
plot(birthsdecompose)

# 계절성을 제외한 데이터의 사각화
birthsdecompose <- decompose(birthstimeseries)
birthsseasonal <- birthstimeseries - birthsdecompose$seasonal
plot(birthsseasonal)

# AirPassengers 승객 수 변화 데이터
data(AirPassengers)
class (AirPassengers)
start (AirPassengers)
end(AirPassengers)
frequency (AirPassengers)
summary (AirPassengers)
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)
plot (aggregate (AirPassengers , FUN=mean) )
boxplot(AirPassengers~cycle(AirPassengers)) 

# 정상성 테스트
# 귀무가설 : 비안정성, 대립 가설 : 안정성
adf.test((AirPassengers), alternative = "stationary", k = 0) # stationary
adf.test(log(AirPassengers), alternative = "stationary", k = 0)
adf.test(diff(log(AirPassengers)), alternative = "stationary", k = 0)


auto.arima(diff(log(AirPassengers))) # ARIMA(0,0,1)(0,1,1)
fit <- arima(log(AirPassengers), c(0,0,1), seasonal = list(order = c(0,1,1), period = 12))
pred <- predict(fit, n.ahead = 10*12)

# 추세가 반영되지 않은 예측
# log 역변환을 해서 값을 계산
ts.plot(AirPassengers, 2.718^pred$pred, log = "y", lty = c(1,3))
# 1949 ~ 1956년까지
air.model <- Arima(window(AirPassengers, end = 1956+11/12), order =c(0,0,1),
                   seasonal = list(order = c(0,1,1), period = 12), lambda = 0)
plot(forecast(air.model, h = 48)) # 4년 동안을 예측
lines(AirPassengers)
accuracy(air.model)
# 지수 평활모델을 이용한 예측
ets # Exponential smoothing state space model
fit <- ets(WWWusage)
plot(WWWusage)
lines(fitted(fit), col = 'red')
lines(fitted(fit, h =2), col = 'green')
