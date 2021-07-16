#Forecasting using Prophet

library(prophet)
library(plyr)
library(rstan)

#Loading data
amazon = read.csv("AMZN.csv", header=TRUE)
amazon = data.frame(amazon$Date, amazon$Adj.Close)

#Forecasting future stock price
colnames(amazon) = c("ds", "y")
amazon$y = as.double(amazon$y)
amazon$ds = as.Date(amazon$ds, format = '%Y-%m-%d')
model = prophet(amazon)
future = make_future_dataframe(model, periods = 90)
tail(future)
forecast = predict(model, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
dyplot.prophet(model, forecast)
prophet_plot_components(model, forecast)

#Errors output
metric_cv = cross_validation(model, 
    initial = 200, 
    horizon = 90,
    period = 45,
    units = "days")
metric_pm = performance_metrics(metric_cv, 
    metrics = c("mse", "rmse", "mae", "mape"), 
    rolling_window = 1)
print(metric_pm)
print(metric_cv)
