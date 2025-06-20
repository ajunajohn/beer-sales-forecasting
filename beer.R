beer = read.csv("beer.csv")
head(beer)


beer_t = ts(beer, start = c(2001,1), end = c(2018,4), frequency = 4)
beer_train = window(beer_t, start=c(2001,1), end=c(2016,2))
beer_test = window(beer_t, start=c(2016,3), end=c(2018,4))

plot(beer_t, col="Green", main ="Time series for beer")

plot(aggregate(beer_t, nfrequency=1))
# shows clear increasing trend

library("forecast")

seasonplot(beer_t, main ="Seasonal plot for quarters", year.labels = TRUE)
# As expected max production in 4th quarter followed by 1st Quarter

boxplot(beer_t~cycle(beer_t))
#monthplot(beer_ts)

plot(log(beer_t))

# Arima model
lag.plot(beer_t, lags = 9, do.lines = FALSE)
# high correlation at lag 4 and 8

acf(beer_train)
# clearly it shows series is not stationary 

#######Remove seasonality########
#We have season lag of 4 since quarterly data

beer_diffseason = diff(beer_train,4)
plot(beer_diffseason)
plot(decompose(beer_diffseason))
# trend could still be seen

ndiffs(beer_diffseason)
beer_difftrend = diff(beer_diffseason,1)
plot(beer_difftrend)
# now looks random

library(tseries)
adf.test(beer_difftrend)
kpss.test(beer_difftrend) 

acf(beer_difftrend)
pacf(beer_difftrend)

#spike gradually decreases from 2 in pacf and we will start from 1 for Q from acf plot
library(astsa)
sarima(beer_train,2,1,0,0,1,1,4)

#let us try with other values 
fitArima = Arima(beer_train,order= c(2,1,0), seasonal = list(order = c(0,1,2),period = 4))
fitArima
# AIC increases 

fitAuto =auto.arima(beer_train)
fitAuto
# gives better estimate for AIC value

# let us check mape value
plot(forecast((fitArima), h = 10))
arima_forecast = forecast((fitArima), h = 10)
Vec1 = cbind(beer_test,arima_forecast$mean)
mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1])
# .02548643

plot(forecast((fitAuto), h = 10))
autoArima_forecast = forecast((fitAuto), h = 10)
Vec1 = cbind(beer_test,autoArima_forecast$mean)
mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1])
#.02532261 - better then previous model

# Prediction for next 2 years
forecast((fitAuto), h = 18)

# Holt winter's

grid =as.data.frame(matrix(0,729,4))
colnames(grid) = c("alpha","beta","gamma","MAPE")
counter = 1
for(a in 1:9){
  for(b in 1:9){
    for (g in 1:9){
      grid$alpha[counter] = a/10
      grid$beta[counter]  = b/10
      grid$gamma[counter] = g/10
      beer_hwfit = HoltWinters(beer_train, alpha= a/10, beta= b/10, gamma= g/10,seasonal = "add")
      beer_hw_pred = forecast(beer_hwfit, 10)
      Vec1 = cbind(beer_test,beer_hw_pred$mean)
      grid$MAPE[counter] = mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1])
      counter = counter + 1
    }
  }
}
grid[which.min(grid$MAPE),]

#alpha beta gamma      MAPE
#555   0.7  0.8   0.6 0.0218498

# forecast for next 2 years
beer_hwfit = HoltWinters(beer_train, alpha= .7, beta= .8, gamma= .6,seasonal = "add")
beer_hw_pred = forecast(beer_hwfit, 18)
beer_hw_pred

# amother method similar to auto arima
fit.ets = ets(beer_train)
fit.ets


plot(fit.ets)
plot(forecast((fit.ets), h = 10))
ets_forecast = forecast((fit.ets), h = 10)
Vec1 = cbind(beer_test,ets_forecast$mean)
mean(abs(Vec1[,1]-Vec1[,2])/Vec1[,1])
#.029611 higher then arima
