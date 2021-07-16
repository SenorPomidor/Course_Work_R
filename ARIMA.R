#Forecasting using ARIMA

library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(astsa)

#Loading data
getSymbols("AMZN", 
	from = "2015-01-01", 
	to = "2021-05-15", 
	src =  "yahoo", 
	adjust =  TRUE)

#Amazon Stock Price Chart
Stock_Data = `AMZN`[,4]
plot(Stock_Data, main = "Amazon Stock Price Chart")

#Logarithmic profitableness on Amazon stock
stock = diff(log(Stock_Data))
stock = stock[!is.na(stock)]
plot(stock,type='l', main='Logarithmic profitableness on Amazon stock')

#A graph with lags
acf2(diff(Stock_Data))

#Finding the residual indicators of the Auto ARIMA model
auto.arima(Stock_Data, seasonal = FALSE) # 0, 1, 1
fit = auto.arima(Stock_Data, seasonal = FALSE)
tsdisplay(residuals(fit), main='Residual indicators of the Auto ARIMA model')

#Forecasting future stock price
term = 90
fcast = forecast(fit, h=term)
plot(fcast)

#Error output
accuracy(fcast)
