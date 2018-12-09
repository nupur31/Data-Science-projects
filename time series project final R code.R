library(stats)
library(dplyr)
library(lubridate)
library(Metrics)
#library(help = "stats")
library(forecast)
library(tseries)


setwd("/Users/nupur/Upgrad R/predictive analytic 2/time series analysis/project")
retail_data<-read.csv("Global Superstore.csv")
str(retail_data)

#datacleaning
summary(retail_data) #no na values 

#subsetting retail_data into 21 subsets based on 7 Market and 3 segments
retail_data$Market
#eu,Africa,Canada,EMEA,EU,LATAM,US
retail_data$Segment
#Consumer,Corporate,Home Office

#extracting order month 
retail_data$Order.Date
retail_data$Order.Date<-as.Date(retail_data$Order.Date, "%d-%m-%Y")
retail_data$order_month<-format(retail_data$Order.Date, "%m")
retail_data$order_year<-format(retail_data$Order.Date, "%Y")

#Using group_by and summarise function to create 21 Segment-Market combinations to see which combo is giving heighest and consistent profit.
retail_data1<-retail_data%>%group_by(order_year,order_month,Market,Segment)%>%summarise(sales=sum(Sales),qty=sum(Quantity),profit=sum(Profit))
retail_agg_data<-retail_data1%>%group_by(Market, Segment)%>%summarise(mean_profit=mean(profit),cv_profit=sd(profit)*100/mean(profit))%>%arrange(desc(mean_profit),cv_profit)
View(retail_agg_data)
#From the retail_agg_data dataset its visible eu-Consumer & apac-Consumer combo are most profitable and consistent Market-Segment for the company


#################################predicting 6 months sales & qty for apac consumer###########################

apac_consumer1<-filter(retail_data, Market=="APAC" & Segment=="Consumer")
apac_consumer1
apac_consumer_sale<-apac_consumer1%>%group_by(order_year,order_month)%>%summarise(sales=sum(Sales))%>%arrange(order_year,order_month)

apac_consumer_sale
apac_consumer_sale<-apac_consumer_sale[,c(-1,-2)]
apac_consumer_sale<-apac_consumer_sale%>% mutate(month=row_number())
apac_consumer_sale<-apac_consumer_sale[c(2,1)]
apac_consumer_sale

apac_consumer_qty<-apac_consumer1%>%group_by(order_year,order_month)%>%summarise(qty=sum(Quantity))%>%arrange(order_year,order_month)
apac_consumer_qty<-apac_consumer_qty[,c(-1,-2)]
apac_consumer_qty<-apac_consumer_qty%>% mutate(month=row_number())
apac_consumer_qty<-apac_consumer_qty[c(2,1)]
apac_consumer_qty

######################################Forecasting sale for apac consumer market###########################
nrow(apac_consumer_sale)
total_timeser_apac_sale<-ts(apac_consumer_sale$sales,frequency=12,start=c(2011,1))
apac_consumer_sale_train <- apac_consumer_sale[1:40,]
apac_consumer_sale_ts <- ts(apac_consumer_sale_train$sales,frequency=12,start=c(2011,1))
plot(apac_consumer_sale_ts)
plot(total_timeser_apac_sale)

#Smoothing the series - Moving Average Smoothing

w <- 2
smoothedseries_apac_sale <- stats::filter(apac_consumer_sale_ts, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)
lines(smoothedseries_apac_sale, col="red")
#Smoothing left end of the time series

diff <- smoothedseries_apac_sale[w+2] - smoothedseries_apac_sale[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_sale[i] <- smoothedseries_apac_sale[i+1] - diff
}

str(smoothedseries_apac_sale)
#Smoothing right end of the time series

n <- length(apac_consumer_sale_ts)
diff <- smoothedseries_apac_sale[n-w] - smoothedseries_apac_sale[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_sale[i] <- smoothedseries_apac_sale[i-1] + diff
}

#Plot the smoothed time series

timevals_in_apac_sale <- apac_consumer_sale_train$month
lines(smoothedseries_apac_sale, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_apac_sale <- as.data.frame(cbind(timevals_in_apac_sale, as.vector(smoothedseries_apac_sale)))
colnames(smootheddf_apac_sale) <- c('month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
#plot(apac_consumer_sale_ts)
plot(apac_consumer_sale_ts)
lines(smoothedseries_apac_sale, col="green")
lmfit_apac_sale <- lm(Sales ~ sin(0.5*month) * poly(month,1) + cos(0.5*month) * poly(month,1)
             + month, data=smootheddf_apac_sale)

str(timevals_in_apac_sale)

global_pred_apac_sale <- predict(lmfit_apac_sale, month=timevals_in_apac_sale)
summary(global_pred_apac_sale)

lines(ts(global_pred_apac_sale,frequency = 12,start=c(2011,1)), col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_apac_sale <- apac_consumer_sale_ts-global_pred_apac_sale
plot(local_pred_apac_sale, col='red', type = "l")
#check if local_pred_apac_sale is stationary
acf(local_pred_apac_sale)
acf(local_pred_apac_sale, type="partial")
#fitting arma model on local pred component
armafit_apac_sale<- auto.arima(local_pred_apac_sale)

tsdiag(armafit_apac_sale)


#We'll check if the residual series is white noise

resi_apac_sale <- local_pred_apac_sale-fitted(armafit_apac_sale)
plot(resi_apac_sale)

adf.test(resi_apac_sale,alternative = "stationary")  #Dickey-Fuller shows residual series is stationary
kpss.test(resi_apac_sale)                            #KPSS test shows residual is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

apac_consumer_sale_test <- apac_consumer_sale[41:48,]
timevals_out_apac_sale <- c(apac_consumer_sale_test$month,49,50,51,52,53,54)

global_pred_out_apac_sale <- predict(lmfit_apac_sale,data.frame(month =timevals_out_apac_sale))
str(data.frame(month =timevals_out_apac_sale))

fcast_apac_sale <- global_pred_out_apac_sale
fcast_apac_sale
plot(total_timeser_apac_sale)
lines(ts(smoothedseries_apac_sale,frequency=12,start=c(2011,1)), col="blue", lwd=2)
lines(ts(global_pred_apac_sale,frequency=12,start=c(2011,1)), col='red', lwd=2)

lines(ts(fcast_apac_sale,frequency=12,start=c(2014,5)),col="purple", lwd=2, type = 'l')

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- mape(data.matrix(fcast_apac_sale[1:8]),data.matrix(apac_consumer_sale_test[,2]))
MAPE_class_dec1  #0.216

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_apac_sale<- c(ts(global_pred_apac_sale),ts(global_pred_out_apac_sale))
class_dec_pred_apac_sale<-ts(class_dec_pred_apac_sale,frequency=12,start=c(2011,1))
#plotting classical decomposition series for 54 months
plot(class_dec_pred_apac_sale, col = "red",ylim=range(class_dec_pred_apac_sale,total_timeser_apac_sale))
#ploting original time series
total_timeser_apac_sale<-ts(total_timeser_apac_sale,frequency = 12, start=c(2011,1))
lines(total_timeser_apac_sale, col = "black")

#So, that was classical decomposition, now let's do an ARIMA fit

nrow(apac_consumer_sale)
total_timeser_apac_sale<-ts(apac_consumer_sale$sales,frequency = 12,start=c(2011,1))
apac_consumer_sale_train <- apac_consumer_sale[1:40,]
apac_consumer_sale_ts <- ts(apac_consumer_sale_train$sales,frequency = 12, start=c(2011,1))
plot(apac_consumer_sale_ts)
plot(total_timeser_apac_sale)

autoarima_apac_sale <- auto.arima(apac_consumer_sale_ts)
summary(autoarima_apac_sale)
plot(forecast(autoarima_apac_sale, h=14))

tsdiag(autoarima_apac_sale)
plot(autoarima_apac_sale$x, col="black")
lines(fitted(autoarima_apac_sale), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_apac_sale <- apac_consumer_sale_ts - fitted(autoarima_apac_sale)
plot(resi_auto_arima_apac_sale)
adf.test(resi_auto_arima_apac_sale,alternative = "stationary")
kpss.test(resi_auto_arima_apac_sale)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_apac_sale <- forecast(autoarima_apac_sale, h=8)

MAPE_auto_arima_apac_sale <- mape(data.matrix(fcast_auto_arima_apac_sale$mean),data.matrix(apac_consumer_sale_test[,2]))
MAPE_auto_arima_apac_sale  #0.13
#forecasting apac-consumer sales for next six months
fcast_auto_arima_apac_sale <- forecast(autoarima_apac_sale, h = 14)

fcast_auto_arima_apac_sale_val<-data.frame(timevals_out_apac_sale,as.numeric(fcast_auto_arima_apac_sale$mean))
View(fcast_auto_arima_apac_sale_val)
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_apac_sale <- c(fitted(autoarima_apac_sale),ts(fcast_auto_arima_apac_sale$mean,frequency=12,start=c(2011,1)))

auto_arima_pred_apac_sale<-ts(auto_arima_pred_apac_sale,frequency = 12,start=c(2011,1))
#plotting fitted+predicted series for 54 months
plot(auto_arima_pred_apac_sale, col = "red",ylim=range(auto_arima_pred_apac_sale,total_timeser_apac_sale))
#plotting original series
lines(total_timeser_apac_sale, col = "black")

acf(ts(autoarima_apac_sale$residuals),main='ACF Residual')
pacf(ts(autoarima_apac_sale$residuals),main='PACF Residual')


#################################Forecasting apac-consumer qty for next 6 months#########################
apac_consumer_qty<-apac_consumer1%>%group_by(order_year,order_month)%>%summarise(qty=sum(Quantity))%>%arrange(order_year,order_month)
apac_consumer_qty<-apac_consumer_qty[,c(-1,-2)]
apac_consumer_qty<-apac_consumer_qty%>% mutate(month=row_number())
apac_consumer_qty<-apac_consumer_qty[c(2,1)]
apac_consumer_qty

nrow(apac_consumer_qty)
plot(apac_consumer_qty, type="l")
total_timeser_apac_qty<-ts(apac_consumer_qty$qty,frequency=12,start=c(2011,1))
apac_consumer_qty_train <- apac_consumer_qty[1:40,]
apac_consumer_qty_ts <- ts(apac_consumer_qty_train$qty,frequency=12,start=c(2011,1))
plot(apac_consumer_qty_ts)
plot(total_timeser_apac_qty)

#Smoothing the series - Moving Average Smoothing

w <- 2
smoothedseries_apac_qty<- stats::filter(apac_consumer_qty_ts, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)
lines(smoothedseries_apac_qty, col="red")
#Smoothing left end of the time series

diff <- smoothedseries_apac_qty[w+2] - smoothedseries_apac_qty[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_qty[i] <- smoothedseries_apac_qty[i+1] - diff
}


#Smoothing right end of the time series

n <- length(apac_consumer_qty_ts)
diff <- smoothedseries_apac_qty[n-w] - smoothedseries_apac_qty[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_qty[i] <- smoothedseries_apac_qty[i-1] + diff
}

#Plot the smoothed time series

timevals_in_apac_qty <- apac_consumer_qty_train$month
lines(smoothedseries_apac_qty, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_apac_qty <- as.data.frame(cbind(timevals_in_apac_qty, as.vector(smoothedseries_apac_qty)))
colnames(smootheddf_apac_qty) <- c('month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
#plot(apac_consumer_sale_ts)
plot(apac_consumer_qty_ts)

lmfit_apac_qty<- lm(Sales ~ sin(0.6*month) * poly(month,1) + cos(0.6*month) * poly(month,1)
             + month, data=smootheddf_apac_qty)

str(smootheddf_apac_qty)
global_pred_apac_qty <- predict(lmfit_apac_qty, month=timevals_in_apac_qty)
summary(global_pred_apac_qty)
lines(ts(global_pred_apac_qty,frequency=12,start=c(2011,1)), col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_apac_qty <- apac_consumer_qty_ts-global_pred_apac_qty
plot(local_pred_apac_qty, col='red', type = "l")
acf(local_pred_apac_qty)
acf(local_pred_apac_qty, type="partial")
armafit_apac_qty<- auto.arima(local_pred_apac_qty)
armafit_apac_qty
tsdiag(armafit_apac_qty)
#lines(timevals_in1,armafit1,col='purple', lwd=2)

#We'll check if the residual series is white noise

resi_apac_qty<- local_pred_apac_qty-fitted(armafit_apac_qty)
plot(resi_apac_qty)
adf.test(resi_apac_qty,alternative = "stationary")
kpss.test(resi_apac_qty)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

apac_consumer_qty_test <- apac_consumer_qty[41:48,]
timevals_out_apac_qty <- c(apac_consumer_qty_test$month,49,50,51,52,53,54)
#timevals_out_apac_qty
#lmfit1
global_pred_out_apac_qty <- predict(lmfit_apac_qty,data.frame(month =timevals_out_apac_qty))

fcast_apac_qty <- global_pred_out_apac_qty

plot(total_timeser_apac_qty)
lines(smoothedseries_apac_qty, col="blue", lwd=2)
lines(ts(global_pred_apac_qty,frequency=12,start=c(2011,1)), col='red', lwd=2)

lines(ts(fcast_apac_qty,frequency=12,start=c(2014,5)),col="purple", lwd=3)

#Now, let's compare our prediction with the actual values, using MAPE
#fcast1
#apac_consumer_sale_test

MAPE_class_dec_apac_qty <- mape(data.matrix(fcast_apac_qty[1:8]),data.matrix(apac_consumer_qty_test[,2]))
MAPE_class_dec_apac_qty  #0.317

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_apac_qty<- c(ts(global_pred_apac_qty),ts(global_pred_out_apac_qty))
str(class_dec_pred_apac_qty)
class_dec_pred_apac_qty<-ts(class_dec_pred_apac_qty,frequency=12,start=c(2011,1))
str(class_dec_pred_apac_qty)
#plotting classical decomposition series for 54 months

plot(class_dec_pred_apac_qty, col = "red",ylim=range(class_dec_pred_apac_qty,total_timeser_apac_qty))
#ploting original time series
lines(total_timeser_apac_qty, col = "black")




#So, that was classical decomposition, now let's do an ARIMA fit

nrow(apac_consumer_qty)
total_timeser1<-ts(apac_consumer_qty$qty,frequency = 12,start=c(2011,1))
apac_consumer_qty_train <- apac_consumer_qty[1:40,]
apac_consumer_qty_ts <- ts(apac_consumer_qty_train$qty,frequency = 12, start=c(2011,1))
plot(apac_consumer_qty_ts)
plot(total_timeser_apac_qty)
autoarima_apac_qty <- auto.arima(apac_consumer_qty_ts)
summary(autoarima_apac_qty)
plot(forecast(autoarima_apac_qty, h=20))

tsdiag(autoarima_apac_qty)
plot(autoarima_apac_qty$x, col="black")
lines(fitted(autoarima_apac_qty), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_apac_qty <- apac_consumer_qty_ts - fitted(autoarima_apac_qty)
plot(resi_auto_arima_apac_qty)
adf.test(resi_auto_arima_apac_qty,alternative = "stationary")
kpss.test(resi_auto_arima_apac_qty)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_apac_qty <- forecast(autoarima_apac_qty, h = 8)

MAPE_auto_arima_apac_qty <- mape(data.matrix(fcast_auto_arima_apac_qty$mean),data.matrix(apac_consumer_qty_test[,2]))
MAPE_auto_arima_apac_qty  #0.15

#Forecasting apac-consumer qty for next 6 months
fcast_auto_arima_apac_qty <- forecast(autoarima_apac_qty, h=14)
fcast_auto_arima_apac_qty_val<-data.frame(timevals_out_apac_qty,as.numeric(fcast_auto_arima_apac_qty$mean))
View(fcast_auto_arima_apac_qty_val)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_apac_qty <- c(fitted(autoarima_apac_qty),ts(fcast_auto_arima_apac_qty$mean))
auto_arima_pred_apac_qty<-ts(auto_arima_pred_apac_qty,frequency = 12,start=c(2011,1))
#plotting fitted+predicted series for 54 months
plot(auto_arima_pred_apac_qty, col = "red",ylim=range(auto_arima_pred_apac_qty,total_timeser_apac_qty))
#plotting original series
lines(total_timeser_apac_qty, col = "black")


acf(ts(autoarima_apac_qty$residuals),main='ACF Residual')
pacf(ts(autoarima_apac_qty$residuals),main='PACF Residual')


######################################predicting 6 months sales & qty for EU consumers###################### 


eu_consumer1<-filter(retail_data, Market=="EU" & Segment=="Consumer")
eu_consumer1
eu_consumer_sale<-eu_consumer1%>%group_by(order_year,order_month)%>%summarise(sales=sum(Sales))%>%arrange(order_year,order_month)

eu_consumer_sale
eu_consumer_sale<-eu_consumer_sale[,c(-1,-2)]
eu_consumer_sale<-eu_consumer_sale%>% mutate(month=row_number())
eu_consumer_sale<-eu_consumer_sale[c(2,1)]
eu_consumer_sale

eu_consumer_qty<-eu_consumer1%>%group_by(order_year,order_month)%>%summarise(qty=sum(Quantity))%>%arrange(order_year,order_month)
eu_consumer_qty<-eu_consumer_qty[,c(-1,-2)]
eu_consumer_qty<-eu_consumer_qty%>% mutate(month=row_number())
eu_consumer_qty<-eu_consumer_qty[c(2,1)]
eu_consumer_qty

#########################################Forecasting sale for eu consumer market############################
nrow(eu_consumer_sale)
total_timeser_eu_sale<-ts(eu_consumer_sale$sales,frequency=12,start=c(2011,1))
eu_consumer_sale_train <- eu_consumer_sale[1:40,]
eu_consumer_sale_ts <- ts(eu_consumer_sale_train$sales,frequency=12,start=c(2011,1))
plot(eu_consumer_sale_ts)
plot(total_timeser_eu_sale)

#Smoothing the series - Moving Average Smoothing

w <- 1
smoothedseries_eu_sale <- stats::filter(eu_consumer_sale_ts, 
                                          filter=rep(1/(2*w+1),(2*w+1)), 
                                          method='convolution', sides=2)
lines(smoothedseries_eu_sale, col="red")
#Smoothing left end of the time series

diff <- smoothedseries_eu_sale[w+2] - smoothedseries_eu_sale[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_eu_sale[i] <- smoothedseries_eu_sale[i+1] - diff
}

str(smoothedseries_eu_sale)
#Smoothing right end of the time series

n <- length(eu_consumer_sale_ts)
diff <- smoothedseries_eu_sale[n-w] - smoothedseries_eu_sale[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_eu_sale[i] <- smoothedseries_eu_sale[i-1] + diff
}

#Plot the smoothed time series

timevals_in_eu_sale <- eu_consumer_sale_train$month
lines(smoothedseries_eu_sale, col="blue", lwd=2)
str(smoothedseries_eu_sale)
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_eu_sale <- as.data.frame(cbind(timevals_in_eu_sale, as.vector(smoothedseries_eu_sale)))
colnames(smootheddf_eu_sale) <- c('month', 'Sales')
smootheddf_eu_sale

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
#plot(eu_consumer_sale_ts)
plot(eu_consumer_sale_ts)
lines(smoothedseries_eu_sale, col="green")
lmfit_eu_sale <- lm(Sales ~ sin(0.5*month) * poly(month,1) + cos(0.5*month) * poly(month,1)
                      + month, data=smootheddf_eu_sale)

str(smootheddf_eu_sale)
global_pred_eu_sale <- predict(lmfit_eu_sale, month=timevals_in_eu_sale)
summary(global_pred_eu_sale)

lines(ts(global_pred_eu_sale,frequency=12,start=c(2011,1)), col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_eu_sale <- eu_consumer_sale_ts-global_pred_eu_sale
plot(local_pred_eu_sale, col='red', type = "l")
acf(local_pred_eu_sale)
acf(local_pred_eu_sale, type="partial")
armafit_eu_sale<- auto.arima(local_pred_eu_sale)
armafit_eu_sale
tsdiag(armafit_eu_sale)
#lines(timevals_in1,armafit1,col='purple', lwd=2)

#We'll check if the residual series is white noise

resi_eu_sale <- local_pred_eu_sale-fitted(armafit_eu_sale)
plot(resi_eu_sale)
adf.test(resi_eu_sale,alternative = "stationary")
kpss.test(resi_eu_sale)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

eu_consumer_sale_test <- eu_consumer_sale[41:48,]
timevals_out_eu_sale <- c(eu_consumer_sale_test$month,49,50,51,52,53,54)
timevals_out_eu_sale
global_pred_out_eu_sale <- predict(lmfit_eu_sale,data.frame(month =timevals_out_eu_sale))
global_pred_out_eu_sale
fcast_eu_sale <- global_pred_out_eu_sale

plot(total_timeser_eu_sale)
lines(smoothedseries_eu_sale, col="blue", lwd=2)
lines(ts(global_pred_eu_sale,frequency=12,start=c(2011,1)), col='red', lwd=2)
lines(ts(fcast_eu_sale,frequency=12,start=c(2014,5)),col="purple", lwd=2)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- mape(data.matrix(fcast_eu_sale[1:8]),data.matrix(eu_consumer_sale_test[,2]))
MAPE_class_dec1  #0.298

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_eu_sale<- c(ts(global_pred_eu_sale),ts(global_pred_out_eu_sale))
class_dec_pred_eu_sale<-ts(class_dec_pred_eu_sale,frequency=12,start=c(2011,1))
#plotting classical decomposition series for 54 months
plot(class_dec_pred_eu_sale, col="red", ylim=range(class_dec_pred_eu_sale,total_timeser_eu_sale))
#plotting original time series

lines(ts(total_timeser_eu_sale,frequency=12,start=c(2011,1)), col = "black")




#So, that was classical decomposition, now let's do an ARIMA fit

nrow(eu_consumer_sale)
total_timeser_eu_sale<-ts(eu_consumer_sale$sales,frequency = 12,start=c(2011,1))
eu_consumer_sale_train <- eu_consumer_sale[1:40,]
eu_consumer_sale_ts <- ts(eu_consumer_sale_train$sales,frequency = 12, start=c(2011,1))
plot(eu_consumer_sale_ts)
plot(total_timeser_eu_sale)

autoarima_eu_sale <- auto.arima(eu_consumer_sale_ts)
summary(autoarima_eu_sale)
plot(forecast(autoarima_eu_sale, h=20))

tsdiag(autoarima_eu_sale)
plot(autoarima_eu_sale$x, col="black")
lines(fitted(autoarima_eu_sale), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_eu_sale <- eu_consumer_sale_ts - fitted(autoarima_eu_sale)
plot(resi_auto_arima_eu_sale)
adf.test(resi_auto_arima_eu_sale,alternative = "stationary")
kpss.test(resi_auto_arima_eu_sale)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_eu_sale <- forecast(autoarima_eu_sale, h=8)
str(fcast_auto_arima_eu_sale)
MAPE_auto_arima_eu_sale <- mape(data.matrix(fcast_auto_arima_eu_sale$mean),data.matrix(eu_consumer_sale_test[,2]))
MAPE_auto_arima_eu_sale  #0.169
fcast_auto_arima_eu_sale <- forecast(autoarima_eu_sale, h = 14)
fcast_auto_arima_eu_sale_val<-data.frame(timevals_out_eu_sale,as.numeric(fcast_auto_arima_eu_sale$mean))
View(fcast_auto_arima_eu_sale_val)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_eu_sale <- c(fitted(autoarima_eu_sale),ts(fcast_auto_arima_eu_sale$mean,frequency=12,start=c(2011,1)))
auto_arima_pred_eu_sale<-ts(auto_arima_pred_eu_sale,frequency = 12,start=c(2011,1))
#plotting predicted arima series for 54 months
plot(auto_arima_pred_eu_sale, col = "red",ylim=range(total_timeser_eu_sale,auto_arima_pred_eu_sale))
#plotting original series
lines(total_timeser_eu_sale, col = "black")

acf(ts(autoarima_eu_sale$residuals),main='ACF Residual')
pacf(ts(autoarima_eu_sale$residuals),main='PACF Residual')

##############################Forecasting eu-consumer qty for next 6 months#################################

eu_consumer_qty<-eu_consumer1%>%group_by(order_year,order_month)%>%summarise(qty=sum(Quantity))%>%arrange(order_year,order_month)
eu_consumer_qty<-eu_consumer_qty[,c(-1,-2)]
eu_consumer_qty<-eu_consumer_qty%>% mutate(month=row_number())
eu_consumer_qty<-eu_consumer_qty[c(2,1)]
eu_consumer_qty

nrow(eu_consumer_qty)
plot(eu_consumer_qty, type="l")
total_timeser_eu_qty<-ts(eu_consumer_qty$qty,frequency=12,start=c(2011,1))
eu_consumer_qty_train <- eu_consumer_qty[1:40,]
eu_consumer_qty_ts <- ts(eu_consumer_qty_train$qty,frequency=12,start=c(2011,1))
plot(eu_consumer_qty_ts)
plot(total_timeser_eu_qty)

#Smoothing the series - Moving Average Smoothing

w <- 1
smoothedseries_eu_qty<- stats::filter(eu_consumer_qty_ts, 
                                        filter=rep(1/(2*w+1),(2*w+1)), 
                                        method='convolution', sides=2)
lines(smoothedseries_eu_qty, col="red")
#Smoothing left end of the time series

diff <- smoothedseries_eu_qty[w+2] - smoothedseries_eu_qty[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_eu_qty[i] <- smoothedseries_eu_qty[i+1] - diff
}


#Smoothing right end of the time series

n <- length(eu_consumer_qty_ts)
diff <- smoothedseries_eu_qty[n-w] - smoothedseries_eu_qty[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_eu_qty[i] <- smoothedseries_eu_qty[i-1] + diff
}

#Plot the smoothed time series

timevals_in_eu_qty <- eu_consumer_qty_train$month
lines(smoothedseries_eu_qty, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_eu_qty <- as.data.frame(cbind(timevals_in_eu_qty, as.vector(smoothedseries_eu_qty)))
colnames(smootheddf_eu_qty) <- c('month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
#plot(eu_consumer_sale_ts)
plot(eu_consumer_qty_ts)

lmfit_eu_qty<- lm(Sales ~ sin(0.5*month) * poly(month,1) + cos(0.5*month) * poly(month,1)
                    + month, data=smootheddf_eu_qty)

str(smootheddf_eu_qty)
global_pred_eu_qty <- predict(lmfit_eu_qty, month=timevals_in_eu_qty)
summary(global_pred_eu_qty)
lines(ts( global_pred_eu_qty,frequency=12,start=c(2011,1)), col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_eu_qty <- eu_consumer_qty_ts-global_pred_eu_qty
plot(local_pred_eu_qty, col='red', type = "l")
acf(local_pred_eu_qty)
acf(local_pred_eu_qty, type="partial")
armafit_eu_qty<- auto.arima(local_pred_eu_qty)
armafit_eu_qty
tsdiag(armafit_eu_qty)
#lines(timevals_in1,armafit1,col='purple', lwd=2)

#We'll check if the residual series is white noise

resi_eu_qty<- local_pred_eu_qty-fitted(armafit_eu_qty)
plot(resi_eu_qty)
adf.test(resi_eu_qty,alternative = "stationary")
kpss.test(resi_eu_qty)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

eu_consumer_qty_test <- eu_consumer_qty[41:48,]
timevals_out_eu_qty <- c(eu_consumer_qty_test$month,49,50,51,52,53,54)
#timevals_out_eu_qty
#lmfit1
global_pred_out_eu_qty <- predict(lmfit_eu_qty,data.frame(month =timevals_out_eu_qty))

fcast_eu_qty <- global_pred_out_eu_qty

plot(total_timeser_eu_qty)
lines(smoothedseries_eu_qty, col="blue", lwd=2)
lines(ts(global_pred_eu_qty,frequency=12,start=c(2011,1)), col='red', lwd=2)

lines(ts(fcast_eu_qty,frequency=12,start=c(2014,5)),col="purple", lwd=2)

#Now, let's compare our prediction with the actual values, using MAPE
#fcast1
#eu_consumer_sale_test
MAPE_class_dec_eu_qty <- mape(data.matrix(fcast_eu_qty[1:8]),data.matrix(eu_consumer_qty_test[,2]))
MAPE_class_dec_eu_qty  #0.27

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_eu_qty<- c(ts(global_pred_eu_qty),ts(global_pred_out_eu_qty))
class_dec_pred_eu_qty<-ts(class_dec_pred_eu_qty,frequency=12,start=c(2011,1))
#plotting classical decomposition series for 54 months
plot(class_dec_pred_eu_qty, col="red",ylim=range(class_dec_pred_eu_qty,total_timeser_eu_qty))
#plotting original time series
lines(total_timeser_eu_qty, col = "black")


#So, that was classical decomposition, now let's do an ARIMA fit

nrow(eu_consumer_qty)
total_timeser1<-ts(eu_consumer_qty$qty,frequency = 12,start=c(2011,1))
eu_consumer_qty_train <- eu_consumer_qty[1:40,]
eu_consumer_qty_ts <- ts(eu_consumer_qty_train$qty,frequency = 12, start=c(2011,1))
plot(eu_consumer_qty_ts)
plot(total_timeser_eu_qty)
autoarima_eu_qty <- auto.arima(eu_consumer_qty_ts)
summary(autoarima_eu_qty)
plot(forecast(autoarima_eu_qty, h=20))

tsdiag(autoarima_eu_qty)
plot(autoarima_eu_qty$x, col="black")
lines(fitted(autoarima_eu_qty), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_eu_qty <- eu_consumer_qty_ts - fitted(autoarima_eu_qty)
plot(resi_auto_arima_eu_qty)
adf.test(resi_auto_arima_eu_qty,alternative = "stationary")
kpss.test(resi_auto_arima_eu_qty)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_eu_qty <- forecast(autoarima_eu_qty, h = 8)

MAPE_auto_arima_eu_qty <- mape(data.matrix(fcast_auto_arima_eu_qty$mean),data.matrix(eu_consumer_qty_test[,2]))
MAPE_auto_arima_eu_qty  #0.195
#forecasting EU consumer qty for next 6 months
fcast_auto_arima_eu_qty <- forecast(autoarima_eu_qty, h=14)
fcast_auto_arima_eu_qty_val<-data.frame(timevals_out_eu_qty,as.numeric(fcast_auto_arima_eu_qty$mean))
View(fcast_auto_arima_eu_qty_val)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_eu_qty <- c(fitted(autoarima_eu_qty),ts(fcast_auto_arima_eu_qty$mean))
auto_arima_pred_eu_qty<-ts(auto_arima_pred_eu_qty,frequency=12,start=c(2011,1))

plot(auto_arima_pred_eu_qty, col = "red",ylim=range(auto_arima_pred_eu_qty,total_timeser_eu_qty))
lines(total_timeser_eu_qty, col = "black")
acf(ts(autoarima_eu_qty$residuals),main='ACF Residual')
pacf(ts(autoarima_eu_qty$residuals),main='PACF Residual')

