DepoForecast 

library(forecast) 
library(tseries)
library(ggplot2)

Risk <- read.csv("Riskapp.csv", header=TRUE,  sep = ";") 
Risk.ts <- ts(Risk, frequency = 12, start=c(2015,01))

Risktrain.ts <- window(Risk.ts, start=c(2015,01), end=c(2017,12))
Riskvalid.ts <- window(Risk.ts, start=c(2018,01))

BoxCox transformation
Lm <- BoxCox.lambda(Risktrain.ts, method="loglik")

1 Vers
Risk.forecast1 <- stlf(Risktrain.ts, h=10, lambda=Lm)
Risk.forecast1.1 <- stlf(Risk.ts, h=14)
autoplot(Risk.forecast1)
autoplot(Risk.forecast1.1)

Остатки 1
res1 <- residuals(Risk.forecast1)
autoplot(res1) + xlab("Mth") + ylab("") +
ggtitle("Residuals from stlf method")

gghistogram(res1, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res1) + ggtitle("ACF of residuals")

Консолидировано
checkresiduals(Risk.forecast1)

Box.test(res1, lag=10, fitdf=0, type="Lj")

Остатки 1.1

res1.1 <- residuals(Depo.forecast1.1)
autoplot(res1.1) + xlab("Weeks") + ylab("") +
ggtitle("Residuals from stlf method")

gghistogram(res1.1, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res1.1) + ggtitle("ACF of residuals")

Консолидировано
checkresiduals(Depo.forecast1.1)

Box.test(res1.1, lag=10, fitdf=0, type="Lj")




2 Vers
Risk.forecast2 <- forecast(Risktrain.ts, h=10, lambda=Lm)
Risk.forecast2.1 <- forecast(Risktrain.ts, h=10)
autoplot(Risk.forecast2)
autoplot(Risk.forecast2.1)

Остатки 2
res2 <- residuals(Depo.forecast2)
autoplot(res1) + xlab("Weeks") + ylab("") +
ggtitle("Residuals from stlf method")

gghistogram(res1, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res1) + ggtitle("ACF of residuals")

Консолидировано
checkresiduals(Depo.forecast2)

Box.test(res2, lag=10, fitdf=0, type="Lj")

Остатки 2.1

res2.1 <- residuals(Depo.forecast2.1)
autoplot(res2.1) + xlab("Weeks") + ylab("") +
ggtitle("Residuals from stlf method")

gghistogram(res2.1, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res2.1) + ggtitle("ACF of residuals")

Консолидировано
checkresiduals(Depo.forecast2.1)

Box.test(res2.1, lag=10, fitdf=0, type="Lj")


3 Vers
fit.nnm <- nnetar(Risktrain.ts, lambda=Lm)
fcast3.1.nnm <- forecast(fit.nnm, h=10, lambda=Lm)
autoplot(fcast3.1.nnm)

fit.nnm <- nnetar(Risktrain.ts)
fcast3.2.nnm <- forecast(fit.nnm, h=10)
autoplot(fcast3.2.nnm)

Остатки3.1
res3.1 <- residuals(fcast3.1.nnm)

autoplot(res3.1) + xlab("Week") + ylab("") +
ggtitle("Residuals from NNTAR method")

gghistogram(res3.1, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res3.1) + ggtitle("ACF of residuals")

Box.test(res3.1, lag=10, fitdf=0, type="Lj")

Консолидировано
checkresiduals(fcast3.1.nnm)

Остатки3.2
res3.2 <- residuals(fcast3.2.nnm)

autoplot(res3.2) + xlab("Week") + ylab("") +
ggtitle("Residuals from NNTAR method")

gghistogram(res3.2, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res3.2) + ggtitle("ACF of residuals")

Box.test(res3.2, lag=10, fitdf=0, type="Lj")

Консолидировано
checkresiduals(fcast3.2.nnm)


4 Vers
fit.tbats4 <-tbats(Risktrain.ts, lambda=Lm)
fcast.tbats4 <- forecast(fit.tbats4,  h=10, lambda=Lm)
autoplot(fcast.tbats4)

fit.tbats4.1 <-tbats(Risktrain.ts)
fcast.tbats4.1 <- forecast(fit.tbats4.1,  h=10)
autoplot(fcast.tbats4.1)


Остатки4
res4 <- residuals(fcast.tbats4)

autoplot(res4) + xlab("Week") + ylab("") +
ggtitle("Residuals from TBATS method")

gghistogram(res4, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res4) + ggtitle("ACF of residuals")

Box.test(res4, lag=10, fitdf=0, type="Lj")

Консолидировано
checkresiduals(fcast.tbats4)

5 Vers

fit.arima <- auto.arima(Risk.ts, lambda=Lm)
fcast.arima <- forecast(fit.arima,  h=14, lambda=Lm)
autoplot(fcast.arima)

fit.arima5.1 <- auto.arima(Risktrain.ts)
fcast.arima5.1 <- forecast(fit.arima5.1,  h=10)
autoplot(fcast.arima5.1)

Остатки5
res5 <- residuals(fcast.arima)

autoplot(res5) + xlab("Week") + ylab("") +
ggtitle("Residuals from ARIMA1 method")

gghistogram(res5, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res5) + ggtitle("ACF of residuals")

Box.test(res5, lag=10, fitdf=0, type="Lj")
Консолидировано
checkresiduals(fcast.arima)

Остатки5.1

res5.1 <- residuals(fcast.arima5.1)

autoplot(res5.1) + xlab("Week") + ylab("") +
ggtitle("Residuals from ARIMA2 method")

gghistogram(res5.1, add.normal=TRUE) +
ggtitle("Histogram of residuals")
ggAcf(res5.1) + ggtitle("ACF of residuals")

Box.test(res5.1, lag=10, fitdf=0, type="Lj")
Консолидировано
checkresiduals(fcast.arima5.1)

6 Vers
model01 <- arima(DepoSMEtrain.ts, order=c(0,0,1))
model02 <- arima(DepoSMEtrain.ts, order=c(0,0,2))
model03 <- arima(DepoSMEtrain.ts, order=c(0,0,3))
model04 <- arima(DepoSMEtrain.ts, order=c(0,0,4))
model05 <- arima(DepoSMEtrain.ts, order=c(0,0,5))
model06 <- arima(DepoSMEtrain.ts, order=c(0,0,6))
model07 <- arima(DepoSMEtrain.ts, order=c(0,0,7))
model08 <- arima(DepoSMEtrain.ts, order=c(0,0,8))
model09 <- arima(DepoSMEtrain.ts, order=c(0,0,9))
model010 <- arima(DepoSMEtrain.ts, order=c(0,0,10))
model011 <- arima(DepoSMEtrain.ts, order=c(0,0,11))
model012 <- arima(DepoSMEtrain.ts, order=c(0,0,12))
model013 <- arima(DepoSMEtrain.ts, order=c(0,0,13))
model014 <- arima(DepoSMEtrain.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)
model013 best  min AIC

model0013 <- arima(DepoSMEtrain.ts, order=c(0,0,13))
model1013 <- arima(DepoSMEtrain.ts, order=c(1,0,13))
model2013 <- arima(DepoSMEtrain.ts, order=c(2,0,13))
model3013 <- arima(DepoSMEtrain.ts, order=c(3,0,13))
model4013 <- arima(DepoSMEtrain.ts, order=c(4,0,13))
AIC(model0013, model1013,  model2013, model3013, model4013)
model1013 best  min AIC

model1013 <- arima(DepoSMEtrain.ts, order=c(1,0,13))
model1113 <- arima(DepoSMEtrain.ts, order=c(1,1,13))
model1213 <- arima(DepoSMEtrain.ts, order=c(1,2,13))
model1313 <- arima(DepoSMEtrain.ts, order=c(1,3,13))
model1413 <- arima(DepoSMEtrain.ts, order=c(1,4,13))
model1513 <- arima(DepoSMEtrain.ts, order=c(1,5,13))

AIC(model1013, model1113, model1213, model1313, model1413, model1513)
model1213 Best model min AIC 

Forecast
plot(DepoSMEtrain.ts,  xlim=c(2015, 2019), ylim=c(0, 20000))
lines(predict(model1213, n.ahead=78, se.fit = TRUE)$pred, col="green")
lines(predict(model1213, n.ahead=78, se.fit = TRUE)$se + predict(model1213, n.ahead=78, se.fit = TRUE)$pred, col="red")
lines(-predict(model1213, n.ahead=78, se.fit = TRUE)$se + predict(model1213, n.ahead=78, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred)
round(-predict(model1311, n.ahead=12, se.fit = TRUE)$se + predict(model1311, n.ahead=12, se.fit = TRUE)$pred)
round(predict(model1311, n.ahead=12, se.fit = TRUE)$pred) 


Accuracy
accuracy(fcast.arima5.1, Riskvalid.ts)
accuracy(fcast.arima, Riskvalid.ts)
accuracy(fcast.tbats4, Riskvalid.ts)
accuracy(fcast.tbats4.1, Riskvalid.ts)
accuracy(fcast3.1.nnm, Riskvalid.ts)
accuracy(fcast3.2.nnm, Riskvalid.ts)
accuracy(Risk.forecast1, Riskvalid.ts)
accuracy(Risk.forecast1.1, Riskvalid.ts)



