# Loading the required libraries
library(forecast)
library(tseries)
library(graphics)
library(lubridate)
library(dplyr)
library(tidyr)

# Loading the file into R
global_store <- c("Global Superstore.csv")
ylab1 <- c("Profit")
ylab2 <- c("Sales")
ylab3 <- c("Quantity")
xlab1 <- c("Months Since Jan 2011")
xcol <- c(1)
ycol <- c(2)
title <- c("Analysis since Jan 2011")
rawdata <- read.csv(global_store)

sapply(rawdata, function(x) sum(is.na(x)))
#         Row.ID       Order.ID     Order.Date      Ship.Date      Ship.Mode 
#              0              0              0              0              0 
#    Customer.ID  Customer.Name        Segment           City          State 
#              0              0              0              0              0 
#        Country    Postal.Code         Market         Region     Product.ID 
#              0          41296              0              0              0 
#       Category   Sub.Category   Product.Name          Sales       Quantity 
#              0              0              0              0              0 
#       Discount         Profit  Shipping.Cost Order.Priority 
#              0              0              0              0 
# Only Postal Code has 41,296 missing values. Since we are not considering Postal Code column for our analysis, we will ignore this missing value column and consider all data rows for analysis.

# Preparing date fields for date manipulation - Using Order date as the date for time series analysis
# and counting the months for the same, as required as per the assignment
rawdata$Ship.Date <- as.Date(rawdata$Ship.Date,format="%d-%m-%Y")
rawdata$Order.Date <- as.Date(rawdata$Order.Date, format ="%d-%m-%Y")

# Using Order date as the base date for time series analysis and counting months since first order date, as stated in assignment objectives
base_date <- as.Date(min(rawdata$Order.Date))
rawdata$Months <- (year(rawdata$Order.Date) - year(base_date)) * 12 + month(rawdata$Order.Date) - month(base_date) + 1

# Subset initial dataset to include only relevant rows of Market, Segment, Order Date, Order Month since Jan 2011, Quantity, Sales, Profit
rawdata_final <- subset(rawdata, select = c("Market", "Segment","Order.Date","Months","Quantity","Sales","Profit"))

# Prepare Order Month level summary for all Markets and Segments
data_sum <- summarise(group_by(rawdata_final, Market, Segment, Months),
                      quantity = sum(Quantity,na.rm=TRUE),
                      sales = sum(Sales,na.rm=TRUE),
                      profit = sum(Profit,na.rm=TRUE))

# Compute coefficient of variation for each Market and Segment
data_bkt <- summarise(group_by(data_sum, Market, Segment),
                      profit_mean = mean(profit),
                      profit_sd = sd(profit),
                      profit_cov = profit_sd/profit_mean)

# Sorting each Market and Segment by coefficient of variation so that the top 2 Market-Segment combinations will be the most consistently profitable bucket  
data_bkt_sorted <- data.frame(data_bkt[order(data_bkt$profit_cov),])
i <- sapply(data_bkt_sorted, is.factor)
data_bkt_sorted[i] <- lapply(data_bkt_sorted[i], as.character)

# Preparing month level demand and sales data for most consistently profitable bucket of EMEA Home Office
profit1_data <- data_sum[(data_sum$Market==data_bkt_sorted[1,c("Market")] & data_sum$Segment==data_bkt_sorted[1,c("Segment")]),]

# Preparing month level demand and sales data for 2nd most consistently profitable bucket of EMEA Corporate
profit2_data <- data_sum[(data_sum$Market==data_bkt_sorted[2,c("Market")] & data_sum$Segment==data_bkt_sorted[2,c("Segment")]),]

# Creating separate datasets for analysis demand and sales of the two most consistent and profitable segments
profit1_demand <- as.data.frame(profit1_data[order(profit1_data$Months),c("Months","quantity")])
profit1_sales <- as.data.frame(profit1_data[order(profit1_data$Months),c("Months","sales")])
profit2_demand <- as.data.frame(profit2_data[order(profit2_data$Months),c("Months","quantity")])
profit2_sales <- as.data.frame(profit2_data[order(profit2_data$Months),c("Months","sales")])

# Detaching dplyr package so that "filter" function can be used for time series analysis
detach(package:dplyr)

###################################################################################################################################
#                         Forecasting demand for most profitable market-segment bucket - EU Consumer                              #
###################################################################################################################################

total_timeser <- ts(profit1_demand$quantity)
nrow(profit1_demand)
# [1] 48

# Now we create the model using the first 42 rows.
# Then we will test the model on the remaining 6 rows later.
indata <- profit1_demand[1:42,]
timeser <- ts(indata$quantity)
plot(timeser)

# Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- filter(timeser, 
						 filter=rep(1/(2*w+1),(2*w+1)), 
						 method='convolution', sides=2)

# Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

# Plot the smoothed time series
timevals_in <- indata$Months
lines(smoothedseries, col="blue", lwd=2)

# Building a model on the smoothed time series using classical decomposition
# First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'quantity')

# Now, let's fit a multiplicative model with trend and seasonality to the data
# Seasonality will be modelled using a sinusoid function
lmfit <- lm(quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
lmfit
# Call:
# lm(formula = quantity ~ sin(0.5 * Month) * poly(Month, 3) + cos(0.5 * 
#     Month) * poly(Month, 3) + Month, data = smootheddf)
#
# Coefficients:
#                      (Intercept)                  sin(0.5 * Month)  
#                           368.13                            -62.65  
#                  poly(Month, 3)1                   poly(Month, 3)2  
#                           583.80                           -140.61  
#                  poly(Month, 3)3                  cos(0.5 * Month)  
#                           149.11                            -42.89  
#                            Month  sin(0.5 * Month):poly(Month, 3)1  
#                               NA                           -112.21  
# sin(0.5 * Month):poly(Month, 3)2  sin(0.5 * Month):poly(Month, 3)3  
#                           110.34                            -33.83  
# poly(Month, 3)1:cos(0.5 * Month)  poly(Month, 3)2:cos(0.5 * Month)  
#                          -151.57                             10.67  
# poly(Month, 3)3:cos(0.5 * Month)  
#                           -63.54  

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  144.4   303.7   359.1   364.6   449.8   526.4
lines(timevals_in, global_pred, col='red', lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
# Series: local_pred 
# ARIMA(2,0,0) with zero mean     
#
# Coefficients:
#          ar1      ar2
#       -0.6341  -0.6158
# s.e.   0.1173   0.1131
# 
# sigma^2 estimated as 6937:  log likelihood=-245.89
# AIC=497.79   AICc=498.42   BIC=503

# We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#	Augmented Dickey-Fuller Test
#
# data:  resi
# Dickey-Fuller = -6.6825, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi)
#	KPSS Test for Level Stationarity
#
# data:  resi
# KPSS Level = 0.023531, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months
outdata <- profit1_demand[43:48,]
colnames(outdata) <- c('Month', 'quantity')
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
global_pred_out
#        1        2        3        4        5        6 
# 611.0899 686.4904 735.2470 745.9300 716.9744 658.4093
local_pred_out <- predict(armafit,n.ahead = 6)
local_pred_out
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 16.826629 -7.343469 -5.705894  8.140665 -1.648253 -3.968175
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1]  83.29020  98.62463 100.21834 109.37763 110.66353 111.85993

# Adding the global and local predictions to prepare the forecast
fcast <- global_pred_out + local_pred_out$pred
fcast
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
#        1        2        3        4        5        6 
# 627.9165 679.1469 729.5411 754.0707 715.3262 654.4411

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec
# [1] 31.45475

# Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(fcast))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

# So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# Series: timeser 
# ARIMA(2,1,0)                    
#
# Coefficients:
#           ar1      ar2
#       -0.7359  -0.5879
# s.e.   0.1224   0.1185
#
# sigma^2 estimated as 20151:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

# Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
# 	Augmented Dickey-Fuller Test
#
# data:  resi_auto_arima
# Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi_auto_arima)
#	KPSS Test for Level Stationarity
#
# data:  resi_auto_arima
# KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
fcast_auto_arima
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 452.7129 448.8772 491.8447 462.4816 458.8288 478.7789
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 141.9557 146.8243 150.0427 178.7573 186.2463 191.9294

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
# [1] 30.13319
# ARIMA exhibits lower MAPE than classical decomposition. So, ARIMA is the recommended model.

# Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

######################### Recommended Model for EU Consumer Demand Forecast: ARIMA #########################

###################################################################################################################################
#                         Forecasting sales for most profitable market-segment bucket - EU Consumer                               #
###################################################################################################################################

total_timeser <- ts(profit1_sales$sales)
nrow(profit1_sales)
# [1] 48

# Now we create the model using the first 42 rows.
# Then we will test the model on the remaining 6 rows later.
indata <- profit1_sales[1:42,]
timeser <- ts(indata$sales)
plot(timeser)

# Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- filter(timeser, 
						 filter=rep(1/(2*w+1),(2*w+1)), 
						 method='convolution', sides=2)

# Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

# Plot the smoothed time series
timevals_in <- indata$Months
lines(smoothedseries, col="blue", lwd=2)

# Building a model on the smoothed time series using classical decomposition
# First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'sales')

# Now, let's fit an additive model with trend and seasonality to the data
# Seasonality will be modelled using a sinusoid function
lmfit <- lm(sales ~ sin(0.5*Month) + poly(Month,3) + cos(0.5*Month) + Month, data=smootheddf)
lmfit
# Call:
# lm(formula = sales ~ sin(0.5 * Month) + poly(Month, 3) + cos(0.5 * 
#     Month) + Month, data = smootheddf)
#
# Coefficients:
#      (Intercept)  sin(0.5 * Month)   poly(Month, 3)1   poly(Month, 3)2  
#            29525             -5482             40252            -10185  
#  poly(Month, 3)3  cos(0.5 * Month)             Month  
#            16193             -3385                NA  

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   4421   25520   30840   29010   34770   39750
lines(timevals_in, global_pred, col='red', lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
# Series: local_pred 
# ARIMA(0,0,1) with zero mean     
#
# Coefficients:
#          ma1
#       -0.7522
# s.e.   0.1276
# 
# sigma^2 estimated as 73448911:  log likelihood=-440.37
# AIC=884.73   AICc=885.04   BIC=888.21

# We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#	Augmented Dickey-Fuller Test
#
# data:  resi
# Dickey-Fuller = -3.8308, Lag order = 3, p-value = 0.02682
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi)
#	KPSS Test for Level Stationarity
#
# data:  resi
# KPSS Level = 0.10029, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months
outdata <- profit1_sales[43:48,]
colnames(outdata) <- c('Month', 'sales')
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
global_pred_out
#        1        2        3        4        5        6
# 44793.69 49941.92 54467.51 57844.17 59882.92 60780.93
local_pred_out <- predict(armafit,n.ahead = 6)
local_pred_out
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] -279.2472    0.0000    0.0000    0.0000    0.0000    0.0000
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1]  8570.234 10723.926 10723.926 10723.926 10723.926 10723.926

# Adding the global and local predictions to prepare the forecast
fcast <- global_pred_out + local_pred_out$pred
fcast
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
#        1        2        3        4        5        6
# 44514.45 49941.92 54467.51 57844.17 59882.92 60780.93

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec
# [1] 23.18592

# Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(fcast))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

# So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# Series: timeser 
# ARIMA(2,1,0)                    
#
# Coefficients:
#           ar1      ar2
#       -0.5796  -0.4906
# s.e.   0.1346   0.1310
#
# sigma^2 estimated as 160341957:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

# Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
# 	Augmented Dickey-Fuller Test
#
# data:  resi_auto_arima
# Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi_auto_arima)
#	KPSS Test for Level Stationarity
#
# data:  resi_auto_arima
# KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
fcast_auto_arima
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 39297.86 37221.06 42062.87 40275.32 38936.08 40589.28
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 12662.62 13736.00 14142.19 16297.93 17479.13 18187.77

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
# [1] 28.9226
# Classical decomposition exhibits lower MAPE than ARIMA. So, classical decomposition is the recommended model.

# Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

######################### Recommended Model for EU Consumer Sales Forecast: Classical decomposition #########################

###################################################################################################################################
#                         Forecasting demand for 2nd most profitable market-segment bucket - APAC Consumer                        #
###################################################################################################################################

total_timeser <- ts(profit2_demand$quantity)
nrow(profit2_demand)
# [1] 48

# Now we create the model using the first 42 rows.
# Then we will test the model on the remaining 6 rows later.
indata <- profit2_demand[1:42,]
timeser <- ts(indata$quantity)
plot(timeser)

# Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- filter(timeser, 
						 filter=rep(1/(2*w+1),(2*w+1)), 
						 method='convolution', sides=2)

# Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

# Plot the smoothed time series
timevals_in <- indata$Months
lines(smoothedseries, col="blue", lwd=2)

# Building a model on the smoothed time series using classical decomposition
# First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'quantity')

# Now, let's fit an additive model with trend and seasonality to the data
# Seasonality will be modelled using a sinusoid function
lmfit <- lm(quantity ~ sin(0.5*Month) + poly(Month,3) + cos(0.5*Month) + Month, data=smootheddf)
lmfit
# Call:
# lm(formula = quantity ~ sin(0.5 * Month) + poly(Month, 3) + cos(0.5 * 
#     Month) + Month, data = smootheddf)
#
# Coefficients:
#      (Intercept)  sin(0.5 * Month)   poly(Month, 3)1   poly(Month, 3)2  
#           412.74            -71.13            621.37             29.14  
#  poly(Month, 3)3  cos(0.5 * Month)             Month  
#            23.18            -35.32                NA  

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  181.7   329.4   398.8   406.2   489.4   586.6
lines(timevals_in, global_pred, col='red', lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
# Series: local_pred 
# ARIMA(0,0,0) with zero mean
# 
# sigma^2 estimated as 13433:  log likelihood=-259.21
# AIC=520.42   AICc=520.52   BIC=522.16

# We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#	Augmented Dickey-Fuller Test
#
# data:  resi
# Dickey-Fuller = -4.9927, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi)
#	KPSS Test for Level Stationarity
#
# data:  resi
# KPSS Level = 0.02922, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months
outdata <- profit2_demand[43:48,]
colnames(outdata) <- c('Month', 'quantity')
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
global_pred_out
#        1        2        3        4        5        6 
# 602.1200 652.9370 695.3661 722.1876 730.1083 720.5717
local_pred_out <- predict(armafit,n.ahead = 6)
local_pred_out
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 0 0 0 0 0 0
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 115.8991 115.8991 115.8991 115.8991 115.8991 115.8991

# Adding the global and local predictions to prepare the forecast
fcast <- global_pred_out + local_pred_out$pred
fcast
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
#        1        2        3        4        5        6
# 602.1200 652.9370 695.3661 722.1876 730.1083 720.5717

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec
# [1] 21.53303

# Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(fcast))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

# So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# Series: timeser 
# ARIMA(0,1,0)                    
#
# sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

# Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
# 	Augmented Dickey-Fuller Test
#
# data:  resi_auto_arima
# Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi_auto_arima)
#	KPSS Test for Level Stationarity
#
# data:  resi_auto_arima
# KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
fcast_auto_arima
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 721 721 721 721 721 721
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 159.2675 225.2382 275.8593 318.5349 356.1329 390.1240

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
# [1] 26.24458
# Classical decomposition exhibits lower MAPE than ARIMA. So, classical decomposition is the recommended model.

# Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

######################### Recommended Model for APAC Consumer Demand Forecast: Classical decomposition #########################

###################################################################################################################################
#                         Forecasting sales for 2nd most profitable market-segment bucket - APAC Consumer                         #
###################################################################################################################################

total_timeser <- ts(profit2_sales$sales)
nrow(profit2_sales)
# [1] 48

# Now we create the model using the first 42 rows.
# Then we will test the model on the remaining 6 rows later.
indata <- profit2_sales[1:42,]
timeser <- ts(indata$sales)
plot(timeser)

# Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- filter(timeser, 
						 filter=rep(1/(2*w+1),(2*w+1)), 
						 method='convolution', sides=2)

# Smoothing left end of the time series
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

# Smoothing right end of the time series
n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

# Plot the smoothed time series
timevals_in <- indata$Months
lines(smoothedseries, col="blue", lwd=2)

# Building a model on the smoothed time series using classical decomposition
# First, let's convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'sales')

# Now, let's fit an additive model with trend and seasonality to the data
# Seasonality will be modelled using a sinusoid function
lmfit <- lm(sales ~ sin(0.5*Month) + poly(Month,3) + cos(0.5*Month) + Month, data=smootheddf)
lmfit
# Call:
# lm(formula = sales ~ sin(0.5 * Month) + poly(Month, 3) + cos(0.5 * 
#     Month) + Month, data = smootheddf)
#
# Coefficients:
#      (Intercept)  sin(0.5 * Month)   poly(Month, 3)1   poly(Month, 3)2  
#            35050             -6108             54396             -3642  
#  poly(Month, 3)3  cos(0.5 * Month)             Month  
#            -2148             -2214                NA

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  14750   28090   35240   34500   41570   50470
lines(timevals_in, global_pred, col='red', lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
# Series: local_pred 
# ARIMA(0,0,0) with zero mean     
#
# sigma^2 estimated as 1.09e+08:  log likelihood=-448.24
# AIC=898.49   AICc=898.59   BIC=900.23

# We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#	Augmented Dickey-Fuller Test
#
# data:  resi
# Dickey-Fuller = -4.7148, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi)
#	KPSS Test for Level Stationarity
#
# data:  resi
# KPSS Level = 0.029285, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months
outdata <- profit2_sales[43:48,]
colnames(outdata) <- c('Month', 'sales')
timevals_out <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
global_pred_out
#        1        2        3        4        5        6 
# 46648.11 50066.44 52888.94 54467.49 54448.22 52857.83 
local_pred_out <- predict(armafit,n.ahead = 6)
local_pred_out
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 0 0 0 0 0 0
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 10441.52 10441.52 10441.52 10441.52 10441.52 10441.52

# Adding the global and local predictions to prepare the forecast
fcast <- global_pred_out + local_pred_out$pred
fcast
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
#        1        2        3        4        5        6 
# 46648.11 50066.44 52888.94 54467.49 54448.22 52857.83

# Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec
# [1] 23.93068

# Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(fcast))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

# So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
# Series: timeser 
# ARIMA(0,1,1)                    
#
# Coefficients:
#           ma1
#       -0.7559
# s.e.   0.1381
#
# sigma^2 estimated as 170108828:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

# Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
# 	Augmented Dickey-Fuller Test
#
# data:  resi_auto_arima
# Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary
# Interpretation: Reject null hypothesis. Residual data is stationary.
kpss.test(resi_auto_arima)
#	KPSS Test for Level Stationarity
#
# data:  resi_auto_arima
# KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1
# null hypothesis: stationary
# Interpretation: Fail to reject null hypothesis. Residual data is stationary.

# Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
fcast_auto_arima
# $pred
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 44898.7 44898.7 44898.7 44898.7 44898.7 44898.7
#
# $se
# Time Series:
# Start = 43 
# End = 48 
# Frequency = 1 
# [1] 13042.58 13425.61 13798.02 14160.64 14514.20 14859.35

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
# [1] 27.68952
# Classical decomposition exhibits lower MAPE than ARIMA. So, classical decomposition is the recommended model.

# Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

######################### Recommended Model for APAC Consumer Sales Forecast: Classical decomposition #########################