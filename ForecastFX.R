FX forecast

Active Clients

library(forecast)
library(tseries)

FX <- read.csv("FXSME1.csv")
FX.ts <- ts(FX, frequency = 12, start=c(2016, 01))
plot(decompose(FX.ts, type="multiplicative"))


0 вариант прогноз пачкой колонок
library(forecast) 
Depo <- read.csv("DepositSME1.csv", header=TRUE,  sep = ";") 
Depo1.ts <- ts(Depo, frequency = 12, start=c(2013,12))

ns <- ncol(Depo1.ts) 
h <- 24 
fcast <- matrix(NA,nrow=h,ncol=ns) 
for(i in 1:ns) 
    fcast[,i] <- forecast(retail[,i],h=h)$mean 

write(t(fcast),file="retailfcasts.csv",sep=",",ncol=ncol(fcast))  

retailfcasts <- read.csv("retailfcasts.csv", header=TRUE,  sep=",") 

0.1 вариант
Depo <- read.csv("DepositSME1.csv", header=TRUE,  sep = ";") 
Depo1.ts <- ts(Depo, frequency = 12, start=c(2013,12))
Depo.forecast <- stlf(Depo1.ts)
plot(Depo.forecast)



1 Variant
FX.forecast <- forecast(FX.ts, lambda=Lm)
plot(FX.forecast, xlab="Year", ylab="Active Clients")


BoxCox transformation
Lm <- BoxCox.lambda(FX.ts, method="loglik")

2 Variant
fit.nnm <- nnetar(FX.ts, lambda=Lm)
fcast.nnm <- forecast(fit.nnm, h=12, lambda=Lm)
plot(fcast.nnm, xlab="Year", ylab="Active Clients")
fcast.nnm

res <- residuals(fcast.nnm)

library(ggplot2)
autoplot(res) + xlab("Month") + ylab("") +
ggtitle("Residuals from NNTAR method")

gghistogram(res, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")

Box.test(res, lag=10, fitdf=0, type="Lj")

checkresiduals(fcast.nnm)

Нужно сравнительное окно выборки с чем сравниваем разброс
accuracy(fit.nnm, FX.ts)

3 Variant
fit.tbats <-tbats(FX.ts, lambda=Lm)
fcast.tbats <- forecast(fit.tbats,  h=12, lambda=Lm)
plot(fcast.tbats, xlab="Year", ylab="Active Clients")

4 Variant
fit.arima <- auto.arima(FX.ts, lambda=Lm)
fcast.arima <- forecast(fit.arima,  h=12, lambda=Lm)
plot(fcast.arima, xlab="Year", ylab="Active Clients")
fcast.arima

5 Variant

model01 <- arima(FX.ts, order=c(0,0,1))
model02 <- arima(FX.ts, order=c(0,0,2))
model03 <- arima(FX.ts, order=c(0,0,3))
model04 <- arima(FX.ts, order=c(0,0,4))
model05 <- arima(FX.ts, order=c(0,0,5))
model06 <- arima(FX.ts, order=c(0,0,6))
model07 <- arima(FX.ts, order=c(0,0,7))
model08 <- arima(FX.ts, order=c(0,0,8))
model09 <- arima(FX.ts, order=c(0,0,9))
model010 <- arima(FX.ts, order=c(0,0,10))
model011 <- arima(FX.ts, order=c(0,0,11))
model012 <- arima(FX.ts, order=c(0,0,12))
model013 <- arima(FX.ts, order=c(0,0,13))
model014 <- arima(FX.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

model011 best  min AIC

model0011 <- arima(FX.ts, order=c(0,0,11))
model1011 <- arima(FX.ts, order=c(1,0,11))
model2011 <- arima(FX.ts, order=c(2,0,11))
model3011 <- arima(FX.ts, order=c(3,0,11))
model4011 <- arima(FX.ts, order=c(4,0,11))

AIC(model0011, model1011,  model2011, model3011, model4011)

model1011 Best  min AIC

model1011 <- arima(FX.ts, order=c(1,0,11))
model1111 <- arima(FX.ts, order=c(1,1,11))
model1211 <- arima(FX.ts, order=c(1,2,11))
model1311 <- arima(FX.ts, order=c(1,3,11))
model1411 <- arima(FX.ts, order=c(1,4,11))
model1511 <- arima(FX.ts, order=c(1,5,11))

AIC(model1011, model1111, model1211, model1311, model1411, model1511)

model1311 Best model min AIC other error


Forecast
plot(FX.ts,  xlim=c(2016.01, 2020), ylim=c(0, 30000))
lines(predict(model1311, n.ahead=12, se.fit = TRUE)$pred, col="green")
lines(predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred, col="red")
lines(-predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred)
round(-predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred)
round(predict(model1311, n.ahead=12, se.fit = TRUE)$pred) 


New Clients

FX <- read.csv("FXSME2.csv")
FX.ts <- ts(FX, frequency = 12, start=c(2016, 01))
plot(decompose(FX.ts, type="multiplicative"))

1 Variant
FX.forecast <- forecast(FX.ts)
plot(FX.forecast, xlab="Year", ylab="New Clients")

BoxCox transformation
Lm <- BoxCox.lambda(FX.ts, method="loglik")

2 Variant
fit.nnm <- nnetar(FX.ts, lambda=Lm)
fcast.nnm <- forecast(fit.nnm, h=12, lambda=Lm)
plot(fcast.nnm, xlab="Year", ylab="New Clients")
fcast.nnm

3 Variant
fit.tbats <-tbats(FX.ts, lambda=Lm)
fcast.tbats <- forecast(fit.tbats,  h=12, lambda=Lm)
plot(fcast.tbats, xlab="Year", ylab="New Clients")

4 Variant
fit.arima <- auto.arima(FX.ts, lambda=Lm)
fcast.arima <- forecast(fit.arima,  h=12, lambda=Lm)
plot(fcast.arima, xlab="Year", ylab="New Clients")
fcast.arima

5 Variant

model01 <- arima(FX.ts, order=c(0,0,1))
model02 <- arima(FX.ts, order=c(0,0,2))
model03 <- arima(FX.ts, order=c(0,0,3))
model04 <- arima(FX.ts, order=c(0,0,4))
model05 <- arima(FX.ts, order=c(0,0,5))
model06 <- arima(FX.ts, order=c(0,0,6))
model07 <- arima(FX.ts, order=c(0,0,7))
model08 <- arima(FX.ts, order=c(0,0,8))
model09 <- arima(FX.ts, order=c(0,0,9))
model010 <- arima(FX.ts, order=c(0,0,10))
model011 <- arima(FX.ts, order=c(0,0,11))
model012 <- arima(FX.ts, order=c(0,0,12))
model013 <- arima(FX.ts, order=c(0,0,13))
model014 <- arima(FX.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

model01 best  min AIC

model001 <- arima(FX.ts, order=c(0,0,1))
model101 <- arima(FX.ts, order=c(1,0,1))
model201 <- arima(FX.ts, order=c(2,0,1))
model301 <- arima(FX.ts, order=c(3,0,1))
model401 <- arima(FX.ts, order=c(4,0,1))

AIC(model001, model101,  model201, model301, model401)

model001 Best  min AIC

model001 <- arima(FX.ts, order=c(0,0,1))
model011 <- arima(FX.ts, order=c(0,1,1))
model021 <- arima(FX.ts, order=c(0,2,1))
model031 <- arima(FX.ts, order=c(0,3,1))
model041 <- arima(FX.ts, order=c(0,4,1))
model051 <- arima(FX.ts, order=c(0,5,1))

AIC(model001, model011, model021, model031, model041, model051)

model021 Best model min AIC other error


Forecast
plot(FX.ts,  xlim=c(2016.01, 2020), ylim=c(0, 10000))
lines(predict(model021, n.ahead=12, se.fit = TRUE)$pred, col="green")
lines(predict(model021, n.ahead=12, se.fit = TRUE)$se + predict(model021, n.ahead=12, se.fit = TRUE)$pred, col="red")
lines(-predict(model021, n.ahead=12, se.fit = TRUE)$se + predict(model021, n.ahead=12, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred)
round(-predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred)
round(predict(model021, n.ahead=12, se.fit = TRUE)$pred) 


Number of Transaction

FX <- read.csv("FXSME3.csv")
FX.ts <- ts(FX, frequency = 12, start=c(2016, 01))
plot(decompose(FX.ts, type="multiplicative"))

1 Variant
FX.forecast <- forecast(FX.ts)
plot(FX.forecast, xlab="Year", ylab="NumbOfTransaction")


BoxCox transformation
Lm <- BoxCox.lambda(FX.ts, method="loglik")

2 Variant
fit.nnm <- nnetar(FX.ts, lambda=Lm)
fcast.nnm <- forecast(fit.nnm, h=12, lambda=Lm)
plot(fcast.nnm, xlab="Year", ylab="NumbOfTransaction")
fcast.nnm

3 Variant
fit.tbats <-tbats(FX.ts, lambda=Lm)
fcast.tbats <- forecast(fit.tbats,  h=12, lambda=Lm)
plot(fcast.tbats, xlab="Year", ylab="NumbofTransaction")
fcast.tbats

4 Variant
fit.arima <- auto.arima(FX.ts, lambda=Lm)
fcast.arima <- forecast(fit.arima,  h=12, lambda=Lm)
plot(fcast.arima, xlab="Year", ylab="NumbOfTransaction")
fcast.arima


5 Variant

model01 <- arima(FX.ts, order=c(0,0,1))
model02 <- arima(FX.ts, order=c(0,0,2))
model03 <- arima(FX.ts, order=c(0,0,3))
model04 <- arima(FX.ts, order=c(0,0,4))
model05 <- arima(FX.ts, order=c(0,0,5))
model06 <- arima(FX.ts, order=c(0,0,6))
model07 <- arima(FX.ts, order=c(0,0,7))
model08 <- arima(FX.ts, order=c(0,0,8))
model09 <- arima(FX.ts, order=c(0,0,9))
model010 <- arima(FX.ts, order=c(0,0,10))
model011 <- arima(FX.ts, order=c(0,0,11))
model012 <- arima(FX.ts, order=c(0,0,12))
model013 <- arima(FX.ts, order=c(0,0,13))
model014 <- arima(FX.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

model09 best  min AIC

model009 <- arima(FX.ts, order=c(0,0,9))
model109 <- arima(FX.ts, order=c(1,0,9))
model209 <- arima(FX.ts, order=c(2,0,9))
model309 <- arima(FX.ts, order=c(3,0,9))
model409 <- arima(FX.ts, order=c(4,0,9))

AIC(model009, model109,  model209, model309, model409)

model109 Best  min AIC

model109 <- arima(FX.ts, order=c(1,0,9))
model119 <- arima(FX.ts, order=c(1,1,9))
model129 <- arima(FX.ts, order=c(1,2,9))
model139 <- arima(FX.ts, order=c(1,3,9))
model149 <- arima(FX.ts, order=c(1,4,9))
model159 <- arima(FX.ts, order=c(1,5,9))

AIC(model109, model119, model129, model139, model149, model159)

model139       Best model min AIC other error


Forecast
plot(FX.ts,  xlim=c(2016.01, 2020), ylim=c(0, 200000))
lines(predict(model139, n.ahead=12, se.fit = TRUE)$pred, col="green")
lines(predict(model139, n.ahead=12, se.fit = TRUE)$se + predict(model139, n.ahead=12, se.fit = TRUE)$pred, col="red")
lines(-predict(model139, n.ahead=12, se.fit = TRUE)$se + predict(model139, n.ahead=12, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model139, n.ahead=12, se.fit = TRUE)$se + predict(model139, n.ahead=12, se.fit = TRUE)$pred)
round(-predict(model139, n.ahead=12, se.fit = TRUE)$se + predict(model139, n.ahead=12, se.fit = TRUE)$pred)
round(predict(model139, n.ahead=12, se.fit = TRUE)$pred) 

Export


FX <- read.csv("FXSME4.csv")
FX.ts <- ts(FX, frequency = 12, start=c(2016, 01))
plot(decompose(FX.ts, type="multiplicative"))

1 Variant
FX.forecast <- forecast(FX.ts)
plot(FX.forecast, xlab="Year", ylab="ExportUSD")


BoxCox transformation
Lm <- BoxCox.lambda(FX.ts, method="loglik")

2 Variant
fit.nnm <- nnetar(FX.ts, lambda=Lm)
fcast.nnm <- forecast(fit.nnm, h=12, lambda=Lm)
plot(fcast.nnm, xlab="Year", ylab="ExportUSD")
fcast.nnm


3 Variant
fit.tbats <-tbats(FX.ts, lambda=Lm)
fcast.tbats <- forecast(fit.tbats,  h=12, lambda=Lm)
plot(fcast.tbats, xlab="Year", ylab="ExportUSD")
fcast.tbats


4 Variant
fit.arima <- auto.arima(FX.ts, lambda=Lm)
fcast.arima <- forecast(fit.arima,  h=12,  lambda=Lm)
plot(fcast.arima, xlab="Year", ylab="ExportUSD")
fcast.arima


5 Variant error



Import

FX <- read.csv("FXSME5.csv")
FX.ts <- ts(FX, frequency = 12, start=c(2016, 01))
plot(decompose(FX.ts, type="multiplicative"))

1 Variant
FX.forecast <- forecast(FX.ts)
plot(FX.forecast, xlab="Year", ylab="ImportUSD")


BoxCox transformation
Lm <- BoxCox.lambda(FX.ts, method="loglik")

2 Variant
fit.nnm <- nnetar(FX.ts, lambda=Lm)
fcast.nnm <- forecast(fit.nnm, h=12, lambda=Lm)
plot(fcast.nnm, xlab="Year", ylab="ImportUSD")
fcast.nnm


3 Variant
fit.tbats <-tbats(FX.ts, lambda=Lm)
fcast.tbats <- forecast(fit.tbats,  h=12, lambda=Lm)
plot(fcast.tbats, xlab="Year", ylab="ImportUSD")
fcast.tbats


4 Variant
fit.arima <- auto.arima(FX.ts, lambda=Lm)
fcast.arima <- forecast(fit.arima,  h=12,  lambda=Lm)
plot(fcast.arima, xlab="Year", ylab="ImportUSD")
fcast.arima

5 Variant error



