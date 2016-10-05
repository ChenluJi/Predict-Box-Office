revenue.budget.time<-read.csv("year.budget.revenue.csv")
library(forecast)
library(TSA)
budget<-ts(revenue.budget.time$Average.of.Budget[19:82],start = 1948,frequency = 1)
revenue<-ts(revenue.budget.time$Average.of.Revenue[19:82],start = 1948,frequency = 1)
budget.arima<-auto.arima(log(budget))
revenue.arima<-auto.arima(log(revenue))
budget.arima$residuals
revenue.arima$residuals
fit.budget<-Arima(log(budget),order = c(2,1,0),include.drift = TRUE)
fit.revenue<-Arima(log(revenue),order = c(1,1,1),include.drift = TRUE)

plot(fitted(fit.budget))
lines(log(budget),col="blue")

plot(fitted(fit.revenue))
lines(log(revenue),col="blue")
