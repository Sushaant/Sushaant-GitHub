library(fpp2)
library(uroot)
library(forecast)

TimeSeries <- read.csv(file = "D:/SEM 1/Stat TABA/TS/TimeSeries.csv")
TimeSeries
data <- TimeSeries[ ,2]
fd<-ts(data,start=c(2012,1),end=c(2020,12),frequency=12)
fd
autoplot(fd)
autoplot(decompose(fd))
plot(decompose(diff(fd)))
plot(fd)
df <- diff(fd) # d = 1
plot(df)
acf(fd) # q = 0
pacf(fd) # p = 0



#For SES
TimeSeries.ses<-ses(fd, h=3*12)
autoplot(TimeSeries.ses) + autolayer (fitted(TimeSeries.ses),series = "Fitted")
summary(TimeSeries.ses)

#holt's winter model

TimeSeries.holt<-hw(fd, model = "AAA", h=3*12)
autoplot(TimeSeries.holt) + autolayer (fitted(TimeSeries.holt),series = "Fitted")
summary(TimeSeries.holt)


#ARIMA
nsdiffs(fd)
ar <- arima(fd, order=c(2,0,0), seasonal =c(1,0,0))
accuracy(ar)
summary(ar)
forecast(ar,3*12)
autoplot(forecast(ar, 3*12)) + autolayer (fitted(ar),series = "Fitted")
qqnorm(ar$residuals)
qqline(ar$residuals)
Box.test(ar$residuals, type = "Ljung-Box")
fit <-auto.arima(fd)
fit
