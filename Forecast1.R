Portfolio <- read.csv("SwordDemand.csv")

Portf.ts <- ts(Portfolio, frequency = 12, start=c(2013,12))

plot(Portf.ts)

Перебираем значение параметра Ордер модель АРИМА
for (m in 1:14) {assign(paste("model0",m,sep=""), arima(Portf.ts, order=c(0,0,m)))}

лучшая модель с минимальным АIC model04
AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

Перебираем лаг авторегресии для model04
model004 <- arima(Portf.ts, order=c(0,0,4))
model104 <- arima(Portf.ts, order=c(1,0,4))
model204 <- arima(Portf.ts, order=c(2,0,4))
model304 <- arima(Portf.ts, order=c(3,0,4))
model404 <- arima(Portf.ts, order=c(4,0,4))

лучшая модель с минимальным АIC model004
AIC(model004, model104, model204, model304, model404)

Перебираем второй компонент для model004
model004 <- arima(Portf.ts, order=c(0,0,4))
model014 <- arima(Portf.ts, order=c(0,1,4))
model024 <- arima(Portf.ts, order=c(0,2,4))
model034 <- arima(Portf.ts, order=c(0,3,4))
model044 <- arima(Portf.ts, order=c(0,4,4))
model054 <- arima(Portf.ts, order=c(0,5,4))

лучшая модель с минимальным АIC model034
AIC(model004, model014 ,model024, model034, model044, model054)

Прогноз
plot(Portf.ts,  xlim=c(2013.12, 2019), ylim=c(0, 2000))

lines(predict(model034, n.ahead=12, se.fit = TRUE)$pred, col="green")

lines(predict(model034, n.ahead=12, se.fit = TRUE)$se + predict(model034, n.ahead=12, se.fit = TRUE)$pred, col="red")

lines(-predict(model034, n.ahead=12, se.fit = TRUE)$se + predict(model034, n.ahead=12, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model034, n.ahead=12, se.fit = TRUE)$se +  predict(model034, n.ahead=12, se.fit = TRUE)$pred)

round(-predict(model034, n.ahead=12, se.fit = TRUE)$se + predict(model034, n.ahead=12, se.fit = TRUE)$pred)

2 Вариант

model01 <- arima(Portf.ts, order=c(0,0,1))
model02 <- arima(Portf.ts, order=c(0,0,2))
model03 <- arima(Portf.ts, order=c(0,0,3))
model04 <- arima(Portf.ts, order=c(0,0,4))
model05 <- arima(Portf.ts, order=c(0,0,5))
model06 <- arima(Portf.ts, order=c(0,0,6))
model07 <- arima(Portf.ts, order=c(0,0,7))
model08 <- arima(Portf.ts, order=c(0,0,8))
model09 <- arima(Portf.ts, order=c(0,0,9))
model010 <- arima(Portf.ts, order=c(0,0,10))
model011 <- arima(Portf.ts, order=c(0,0,11))
model012 <- arima(Portf.ts, order=c(0,0,12))
model013 <- arima(Portf.ts, order=c(0,0,13))
model014 <- arima(Portf.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

plot(AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014), type="b")

model004 <- arima(Portf.ts, order=c(0,0,4))
model104 <- arima(Portf.ts, order=c(1,0,4))
model204 <- arima(Portf.ts, order=c(2,0,4))
model304 <- arima(Portf.ts, order=c(3,0,4))
model404 <- arima(Portf.ts, order=c(4,0,4))

AIC(model004, model104, model204, model304, model404)

model004 <- arima(Portf.ts, order=c(0,0,4))
model014 <- arima(Portf.ts, order=c(0,1,4))
model024 <- arima(Portf.ts, order=c(0,2,4))
model034 <- arima(Portf.ts, order=c(0,3,4))
model044 <- arima(Portf.ts, order=c(0,4,4))
model054 <- arima(Portf.ts, order=c(0,5,4))

AIC(model004, model014, model024, model034, model044, model054)

model034
plot(Portf.ts,  xlim=c(2013.12, 2019), ylim=c(0, 2000))

lines(predict(model034, n.ahead=12, se.fit = TRUE)$pred, col="green")

Депозиты

1 вариант
Depo <- read.csv("DepositSME.csv")

Depo.ts <- ts(Depo, frequency = 12, start=c(2013,12))

Depo.forecast <- forecast(Depo.ts)

plot(Depo.forecast)

2 вариант

fit.nn <- nnetar(Depo.ts)
fcast.nn <- forecast(fit.nn)
plot(fcast.nn)

3 вариант

fit.tbats <-tbats(Depo.ts)
fcast.tbats <- forecast(fit.tbats)
plot(fcast.tbats)

4 вариант
fit.arima <- auto.arima(Depo.ts)
fcast.arima <- forecast(fit.arima)
plot(fcast.arima)

5 вариант
plot(Depo.ts)

model01 <- arima(Depo.ts, order=c(0,0,1))
model02 <- arima(Depo.ts, order=c(0,0,2))
model03 <- arima(Depo.ts, order=c(0,0,3))
model04 <- arima(Depo.ts, order=c(0,0,4))
model05 <- arima(Depo.ts, order=c(0,0,5))
model06 <- arima(Depo.ts, order=c(0,0,6))
model07 <- arima(Depo.ts, order=c(0,0,7))
model08 <- arima(Depo.ts, order=c(0,0,8))
model09 <- arima(Depo.ts, order=c(0,0,9))
model010 <- arima(Depo.ts, order=c(0,0,10))
model011 <- arima(Depo.ts, order=c(0,0,11))
model012 <- arima(Depo.ts, order=c(0,0,12))
model013 <- arima(Depo.ts, order=c(0,0,13))
model014 <- arima(Depo.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

model014 best  model01 max AIC
model0014 <- arima(Depo.ts, order=c(0,0,14))
model1014 <- arima(Depo.ts, order=c(1,0,14))
model2014 <- arima(Depo.ts, order=c(2,0,14))
model3014 <- arima(Depo.ts, order=c(3,0,14))
model4014 <- arima(Depo.ts, order=c(4,0,14))

AIC(model0014, model1014,  model2014, model3014, model4014)

model1014 Best model001 max AIC

model1014 <- arima(Depo.ts, order=c(1,0,14))
model1114 <- arima(Depo.ts, order=c(1,1,14))
model1214 <- arima(Depo.ts, order=c(1,2,14))
model1314 <- arima(Depo.ts, order=c(1,3,14))
model1414 <- arima(Depo.ts, order=c(1,4,14))
model1514 <- arima(Depo.ts, order=c(1,5,14))

AIC(model1014, model1114, model1214, model1314, model1414, model1514)

model1314 Best model1014 max AIC


Прогноз
plot(Depo.ts,  xlim=c(2013.12, 2019), ylim=c(0, 15000))

lines(predict(model1014, n.ahead=12, se.fit = TRUE)$pred, col="green")

lines(predict(model1014, n.ahead=12, se.fit = TRUE)$se + predict(model1014, n.ahead=12, se.fit = TRUE)$pred, col="red")

lines(-predict(model1014, n.ahead=12, se.fit = TRUE)$se + predict(model1014, n.ahead=12, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model1014, n.ahead=12, se.fit = TRUE)$se +  predict(model1014, n.ahead=12, se.fit = TRUE)$pred)

round(-predict(model1014, n.ahead=12, se.fit = TRUE)$se + predict(model1014, n.ahead=12, se.fit = TRUE)$pred)

plot(decompose(Depo.ts, type="multiplicative"))


Прогноз 2017

0 вариант прогноз пачкой колонок
library(forecast) 
Depo <- read.csv("DepositSME1.csv", header=TRUE,  sep = ";") 
Depo1.ts <- ts(Depo, frequency = 12, start=c(2013,12))

ns <- ncol(Depo1.ts) 
h <- 24 
fcast <- matrix(NA,nrow=h,ncol=ns) 
for(i in 1:ns) 0
    fcast[,i] <- forecast(retail[,i],h=h)$mean 

write(t(fcast),file="retailfcasts.csv",sep=",",ncol=ncol(fcast))  

retailfcasts <- read.csv("retailfcasts.csv", header=TRUE,  sep=",") 

0.1 вариант
Depo <- read.csv("DepositSME1.csv", header=TRUE,  sep = ";") 
Depo1.ts <- ts(Depo, frequency = 12, start=c(2013,12))
Depo.forecast <- stlf(Depo1.ts)
plot(Depo.forecast)

1 вариант
Depo <- read.csv("DepositSME1.csv")
Depo1.ts <- ts(Depo, frequency = 12, start=c(2013,12))
Depo.forecast <- forecast(Depo1.ts)
plot(Depo.forecast)
2вариант
fit.nn <- nnetar(Depo1.ts)
fcast.nn <- forecast(fit.nn)
plot(fcast.nn)

3вариант
fit.tbats <-tbats(Depo1.ts)
fcast.tbats <- forecast(fit.tbats)
plot(fcast.tbats)

4 вариант
fit.arima <- auto.arima(Depo1.ts)
fcast.arima <- forecast(fit.arima)
plot(fcast.arima)

5 вариант
plot(Depo1.ts)

model01 <- arima(Depo1.ts, order=c(0,0,1))
model02 <- arima(Depo1.ts, order=c(0,0,2))
model03 <- arima(Depo1.ts, order=c(0,0,3))
model04 <- arima(Depo1.ts, order=c(0,0,4))
model05 <- arima(Depo1.ts, order=c(0,0,5))
model06 <- arima(Depo1.ts, order=c(0,0,6))
model07 <- arima(Depo1.ts, order=c(0,0,7))
model08 <- arima(Depo1.ts, order=c(0,0,8))
model09 <- arima(Depo1.ts, order=c(0,0,9))
model010 <- arima(Depo1.ts, order=c(0,0,10))
model011 <- arima(Depo1.ts, order=c(0,0,11))
model012 <- arima(Depo1.ts, order=c(0,0,12))
model013 <- arima(Depo1.ts, order=c(0,0,13))
model014 <- arima(Depo1.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

model010 best 

model0010 <- arima(Depo1.ts, order=c(0,0,10))
model1010 <- arima(Depo1.ts, order=c(1,0,10))
model2010 <- arima(Depo1.ts, order=c(2,0,10))
model3010 <- arima(Depo1.ts, order=c(3,0,10))
model4010 <- arima(Depo1.ts, order=c(4,0,10))

AIC(model0010, model1010,  model2010, model3010, model4010)

model1010 Best 

model1010 <- arima(Depo1.ts, order=c(1,0,10))
model1110 <- arima(Depo1.ts, order=c(1,1,10))
model1210 <- arima(Depo1.ts, order=c(1,2,10))
model1310 <- arima(Depo1.ts, order=c(1,3,10))
model1410 <- arima(Depo1.ts, order=c(1,4,10))
model1510 <- arima(Depo1.ts, order=c(1,5,10))

AIC(model1010, model1110, model1210, model1310, model1410, model1510)

model1310 Best 


Прогноз
plot(Depo1.ts,  xlim=c(2013.12, 2018), ylim=c(0, 15000))

lines(predict(model1310, n.ahead=12, se.fit = TRUE)$pred, col="green")

lines(predict(model1310, n.ahead=12, se.fit = TRUE)$se + predict(model1310, n.ahead=12, se.fit = TRUE)$pred, col="red")

lines(-predict(model1310, n.ahead=12, se.fit = TRUE)$se + predict(model1310, n.ahead=12, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model1310, n.ahead=12, se.fit = TRUE)$se +  predict(model1310, n.ahead=12, se.fit = TRUE)$pred)

round(-predict(model1310, n.ahead=12, se.fit = TRUE)$se + predict(model1310, n.ahead=12, se.fit = TRUE)$pred)

plot(decompose(Depo1.ts, type="multiplicative"))




Прогноз портфель  2017
Portfolio <- read.csv("SwordDemand1.csv")
Portf.ts <- ts(Portfolio, frequency = 12, start=c(2013,12))
plot(Portf.ts)

Portf.forecast <- forecast(Portf.ts)
plot(Portf.forecast)

fit.nn <- nnetar(Portf.ts)
fcast.nn <- forecast(fit.nn)
plot(fcast.nn)


fit.tbats <-tbats(Portf.ts)
fcast.tbats <- forecast(fit.tbats)
plot(fcast.tbats)


fit.arima <- auto.arima(Portf.ts)
fcast.arima <- forecast(fit.arima)
plot(fcast.arima)

plot(Portf.ts)

model01 <- arima(Portf.ts, order=c(0,0,1))
model02 <- arima(Portf.ts, order=c(0,0,2))
model03 <- arima(Portf.ts, order=c(0,0,3))
model04 <- arima(Portf.ts, order=c(0,0,4))
model05 <- arima(Portf.ts, order=c(0,0,5))
model06 <- arima(Portf.ts, order=c(0,0,6))
model07 <- arima(Portf.ts, order=c(0,0,7))
model08 <- arima(Portf.ts, order=c(0,0,8))
model09 <- arima(Portf.ts, order=c(0,0,9))
model010 <- arima(Portf.ts, order=c(0,0,10))
model011 <- arima(Portf.ts, order=c(0,0,11))
model012 <- arima(Portf.ts, order=c(0,0,12))
model013 <- arima(Portf.ts, order=c(0,0,13))
model014 <- arima(Portf.ts, order=c(0,0,14))

AIC(model01, model02, model03, model04, model05, model06, model07, model08, model09, model010, model011, model012, model013, model014)

model04

model004 <- arima(Portf.ts, order=c(0,0,4))
model104 <- arima(Portf.ts, order=c(1,0,4))
model204 <- arima(Portf.ts, order=c(2,0,4))
model304 <- arima(Portf.ts, order=c(3,0,4))
model404 <- arima(Portf.ts, order=c(4,0,4))

AIC(model004, model104,  model204, model304, model404)

model004 Best 

model004 <- arima(Portf.ts, order=c(0,0,4))
model014 <- arima(Portf.ts, order=c(0,1,4))
model024 <- arima(Portf.ts, order=c(0,2,4))
model034 <- arima(Portf.ts, order=c(0,3,4))
model044 <- arima(Portf.ts, order=c(0,4,4))
model054 <- arima(Portf.ts, order=c(0,5,4))

AIC(model004, model014, model024, model034, model044, model054)

model034 Best 

Прогноз
plot(Portf.ts,  xlim=c(2013.12, 2018), ylim=c(0, 1500))

lines(predict(model034, n.ahead=12, se.fit = TRUE)$pred, col="green")

lines(predict(model034, n.ahead=12, se.fit = TRUE)$se + predict(model034, n.ahead=12, se.fit = TRUE)$pred, col="red")

lines(-predict(model034, n.ahead=12, se.fit = TRUE)$se + predict(model034, n.ahead=12, se.fit = TRUE)$pred, col="red")

Цифры
round(predict(model1310, n.ahead=12, se.fit = TRUE)$se +  predict(model1310, n.ahead=12, se.fit = TRUE)$pred)

round(-predict(model1310, n.ahead=12, se.fit = TRUE)$se + predict(model1310, n.ahead=12, se.fit = TRUE)$pred)

plot(decompose(Depo.ts, type="multiplicative"))

