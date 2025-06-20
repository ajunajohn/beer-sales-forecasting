install.packages("forecast")
install.packages("ggthemes")
install.packages("TSA")
install.packages("TeachingDemos")
install.packages("astsa")
install.packages("fpp2")
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('forecast')
library('TSA')
library('tseries')
library('caret')
library('TeachingDemos')
library('astsa')
library(fpp2)
library(stats)
library('xts')
#Load the files for analysis and do the basic data type analysis

getwd()
beer_data <- read.csv('beer (1).csv',header = TRUE)
attach(beer_data)
View(beer_data)


#Exploratory Data analysis
str(beer_data)
nrow(beer_data)
class(beer_data)
#Convert the data into time series object
beer_timeseries <- ts(beer_data, frequency = 4, start = c(2003,1))
beer_timeseries
start(beer_timeseries)
end(beer_timeseries)
#Checking the data whether its a Timeseries data or not
is.ts(beer_timeseries)
summary(beer_timeseries)
str(beer_timeseries)
class(beer_timeseries)
periodicity(beer_timeseries)
plot.ts(beer_timeseries, col = "blue", main = "Beer sales time series data between 2003 and 2020")
ts.plot(beer_timeseries, col="blue" ,main="Beer Sales in past 18 years", xlab="Years", ylab="Sales", type="b")
#the data seems to have trend and seasonality
plot(beer_timeseries)

deltat(beer_timeseries)
frequency(beer_timeseries)
abline(reg=lm(beer_timeseries ~ time(beer_timeseries)),col = "lightgray")## plotting the trend line of linear regression
cycle(beer_timeseries)

Annual_beerSales = aggregate(beer_timeseries,nfrequency = 1)
Annual_beerSales
str(Annual_beerSales)
summary(Annual_beerSales)
plot.ts(Annual_beerSales,col="green", main="Annual consumption of beer YOY")

ggsubseriesplot(beer_timeseries,facets=TRUE)
gglagplot(beer_timeseries)
#Seasonal plot of Beer Sales
ggseasonplot(beer_timeseries,facets=TRUE,polar = TRUE)
seasonplot(beer_timeseries,year.labels = TRUE,year.labels.left = TRUE,col=1:18,xlab = "Quarter",ylab = "ML")


#Autocorrelation Analysis
#ACF and PACF plots
acf(beer_timeseries,lag.max = 24)
pacf(beer_timeseries,lag.max = 24)
#Both plots show a cutoff
# 
# #To check whether beer_ts series is stationary or non-stationary.
adf.test(beer_timeseries,alternative = "stationary")
# the series is not stationary as pvalue is greater than .05
periodogram(beer_timeseries)

boxplot(beer_timeseries~cycle(beer_timeseries))
hist(beer_timeseries)
plot(density(log(beer_timeseries)))
shapiro.test(beer_timeseries)
#pvalue is less than alpha. Hence the series is normally distributed


#Decompose data

#decompose_beer_data1 = stl(beer_timeseries,s.window = "periodic")
plot(decompose_beer_data)
decompose_beer_data=decompose(beer_timeseries, type="additive")
plot (decompose(beer_timeseries, type="additive"))
#The series has both trend and seasonality.Seasonality is constant. Trend is more important than seasonality
#We will adjust/ remove seasonality and check
attributes(decompose_beer_data)
decompose_beer_data$trend
decompose_beer_data$seasonal
deseasonal_beer=seasadj(decompose_beer_data)
plot(deseasonal_beer)

adf.test(deseasonal_beer,alternative = "stationary")
#the time series is non stationary

acf(deseasonal_beer,main='ACF for deseasonalised Series',lag.max = 24)
pacf(deseasonal_beer,main='PACF for deseasonalised Series',lag.max = 24)


##Differencing the time series data

ndiffs(deseasonal_beer)
count_d1=diff(deseasonal_beer,differences = 1)
count_d1
plot(count_d1)
adf.test(count_d1,alternative ="stationary")
#plot and adf test is confirming that the time series is stationary now 

acf(count_d1,lag.max = 24)
pacf(count_d1,lag.max = 24)

#Splitting into training and test sets
beer_seasadjtrain=window(deseasonal_beer,end=c(2016,4))
beer_seasadjtest=window(deseasonal_beer,start=c(2017,1))

#Splitting original series into training and test sets
beerTStrain=window(beer_timeseries,end=c(2016,4))
beerTStest=window(beer_timeseries,start=c(2017,1))

#Holt Winter's model on original data - Additive
winter_model1=ets(beerTStrain,model = "AAA")
autoplot(forecast(winter_model1))
summary(winter_model1)
#Forecast
fcast_ets1=forecast(winter_model1,h=24)
plot(fcast_ets1)
#Check accuracy on test data
accuracy(fcast_ets1,beerTStest)

#Holt Winter's model on original data - multiplicative
#winter_model=ets(beerTStrain,model = "MAM")
#autoplot(forecast(winter_model))
#summary(winter_model)
#Forecast
#fcast_ets=forecast(winter_model,h=24)
#plot(fcast_ets)
#Check accuracy on test data
#accuracy(fcast_ets,beerTStest)


#Holt Winter's model on log data
winter_logmodelAdd=ets(log(beerTStrain),model = "AAA")
summary(winter_logmodelAdd)
#winter_logmodel=ets(log(beerTStrain),model = "MAM")
#summary(winter_logmodel)
#Forecast
fcast_logets=forecast(winter_logmodelAdd,h=24)
plot(fcast_logets)
#Check accuracy on test data
accuracy(fcast_logets,log(beerTStest))

#MAPE value is .46 for train data and .389 for test data.
#The model is valid and accurate

tsdisplay(residuals(winter_logmodel),lag.max = 45,main = "Holt Winter's model of original series Residuals")
#residuals variance is less constant

Box.test(winter_logmodel$residuals)
#pvalue is greater than alpha. Residuals are independent. 
hist(winter_logmodel$residuals)
plot(density(winter_logmodel$residuals))
shapiro.test(winter_logmodel$residuals)
#pvalue is greater than alpha. Hence residuals are normally distributed

#ARIMA Model

beerArima= arima(beer_seasadjtrain,order=c(0,1,0))
summary(beerArima)

tsdisplay(residuals(beerArima),lag.max = 15,main = "Model Residuals")
#residuals variance is not constant

Box.test(beerArima$residuals)
#pvalue is greater than alpha. Residuals are not independent. That is there is a problem of auto correlation
hist(beerArima$residuals)
plot(density(beerArima$residuals))
shapiro.test(beerArima$residuals)

#pvalue is higher than alpha. Hence residuals are normally distributed, but there is 
#a problem of autocorrelation
#this is not a valid model and accuracy is not aceptable


#fitting with Auto arima for original train data with trend and seasonality
fit3=auto.arima(beerTStrain,seasonal = TRUE)
fit3
summary(fit3)

tsdisplay(residuals(fit3),lag.max = 45,main = "Auto Arima Model of original series Residuals")
#residuals variance is less constant

Box.test(fit3$residuals)
#pvalue is greater than alpha. Residuals are independent. 
hist(fit3$residuals)
plot(density(fit3$residuals))
shapiro.test(fit3$residuals)
#pvalue is greater than alpha. Hence residuals are normally distributed
#This is a valid model

fit4=auto.arima(log(beerTStrain),seasonal = TRUE)
fit4
summary(fit4)

tsdisplay(residuals(fit4),lag.max = 45,main = "Auto Arima Model of original log series Residuals")
#residuals variance is almost constant

Box.test(fit4$residuals)
#pvalue is greater than alpha. Residuals are independent. 
hist(fit4$residuals)
plot(density(fit4$residuals))
shapiro.test(fit4$residuals)
#pvalue is greater than alpha. Hence residuals are normally distributed
#This is a valid model

#Forecasting using ARIMA model()
fcast=forecast(fit4,h=24)
plot(fcast)

auto.arima(log(beer_timeseries),stepwise=FALSE) %>% forecast(h=24) %>% autoplot()

#Accuracy of the forecast
f7=forecast(fit4)
accuracy(f7,log(beerTStest))
#MAPE value is .4498 for train data and .3788 for test data.
#The model is valid and more accurate



