# read the data from the local system

getwd()
setwd("C:/Users/AD/Documents/Empirical_Finance")

Gold <- read.csv("Gold.csv", header = TRUE)
attach(Gold)
Gold

# include all the library packages necessary for the time series prediction model

library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

# Divide the data into two part Training and test data and convert it to log format

lnstock=log(Price[1:480])
lnstock

#ACF, PACF and Dickey-Fuller test

acf(lnstock, lag.max = 20, main="Auto-Correlation Function(ACF)")
pacf(lnstock, lag.max = 20, main="Partial Auto-Correlation Function(PACF)")
difflnstock=diff(diff(lnstock,1))
difflnstock
adf.test(lnstock)
adf.test(difflnstock)


#Time series and auto.arima

pricearima <- ts(lnstock, start=c(1980,01),frequency=12)
fitlnstock <- auto.arima(pricearima)
#fitlnstock <- arima(pricearima, order=c(5,1,3))
fitlnstock
plot(pricearima,type='l')
title('Gold Price')
exp(lnstock)

#Forecasted values from arima

forecastedvalues_ln=forecast(fitlnstock,h=12)
forecastedvalues_ln
plot(forecastedvalues_ln)

fvx=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(fvx)
finalforecastvalues

#Percentage Error
df<-data.frame(Price[1:480],finalforecastvalues)
col_headings<-c("Actual Prices", "Forecasted Prices")
names(df)<-col_headings
attach(df)
percentage_error=((df$'Actual Price'-df$'Forecasted Price')/(df$'Actual Price'))
percentage_error
mean(percentage_error)
df




















