#####################################################################################################
#                              RETAIL-GIANT SALES FORECASTING CASE STUDY
#####################################################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation

#####################################################################################################
#                              BUSINESS UNDERSTANDING
#####################################################################################################

#Objective:"Global Mart" is an online store super giant having worldwide operations. 
#It takes orders and delivers across the globe and deals with all the major product categories.We need
#to forecast the sales and the demand for the next 6 months, that would help you manage
#the revenue and inventory accordingly.

#####################################################################################################
#                              DATA UNDERSTANDING
#####################################################################################################

#Loading Dataset
global_store <- read.csv("Global Superstore.csv")

#The data currently has the transaction level data, where each row represents a particular order made 
#on the online store. There are 24 attributes related to each such transaction. The "Market" attribute 
#has 7-factor levels representing the geographical market sector that the customer belongs to. 
#The "Segment" attribute tells which of the 3 segments that customer belongs to. 

#Loading Neccessary libraries
#------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forecast)
library(tseries)
require(graphics)


#View the dataset
#------------------------------------------
View(global_store)

#Understanding Dimensions
#------------------------------------------
dim(global_store)   #51290 obs. of 24 variables

#Structure of the dataset
#------------------------------------------
str(global_store)

#Exploring the data
#------------------------------------------
summary(global_store) 

summary(global_store$Segment)
#There are 3 segments - Consumer,Corporate,Home office
summary(global_store$Category)
#There are 3 category -Furniture,Office Supplies,Technology
summary(global_store$Market)
#There are 7 Markets - Africa,APAC,Canada,EMEA,EU,LATAM,US


#####################################################################################################
#                              DATA CLEANING & DATA PREPARATION
#####################################################################################################

#checking NA values in dataset
#------------------------------------------
sapply(global_store, function(x) sum(is.na(x))) #Postal code contain 41296 NA values

#Checking  for duplicates.
#-----------------------------------------------
anyDuplicated(global_store) #No duplicates present

# checking for blank "" values in training dataset
#------------------------------------------
sapply(global_store, function(x) length(which(x == "")))   #No blank values present

#Converting Order.Date and Ship.Date to standard date format
#----------------------------------------------------------------------------------------
global_store$Order.Date <- as.Date(global_store$Order.Date, "%d-%m-%Y")
global_store$Ship.Date <- as.Date(global_store$Ship.Date, "%d-%m-%Y")

#Checking NA value percentage in the dataset
#------------------------------------------
sum(is.na(global_store))/nrow(global_store)
#80.5% of postal codes column contains NA values and removing postal code column as it unnecessary for 
#forecasting
global_store$Postal.Code <- NULL

#Step 1:Segmenting dataset into 21 subsets based on market and customer
#-------------------------------------------------------------------------
#We need to segment the whole dataset into the 21 subsets based on the market and the customer 
#segment level.
global_store$Market_Segment <- paste(global_store$Market,global_store$Segment,sep="_")

#split() divides sales dataset on the basis of market segment and assigning output to list
LIST <- split(global_store,global_store$Market_Segment)
#Using invisible() to return temporary invisible copy of the subsets created and we are assigning
#the subsets to be displayed in the global environment.
invisible(lapply(LIST, function(x){assign(paste(x$Market_Segment[1]), x , envir = .GlobalEnv)}))
# Also we can assign them to the global environment using list2env()function
#list2env(LIST, envir = .GlobalEnv)

#Step 2.Converting the transaction-level data into a time series
#-------------------------------------------------------------------------
#We need to aggregate the 3 attributes  - Sales, Quantity & Profit, over the Order Date to arrive
#at monthly values for these attributes.Also,we need to find coefficient of variation(CV)of the Profit for
#all 21 market segments to find the 2 most profitable and consistently profitable segments. 

#Checking Order Dates duration
#--------------------------------
summary(global_store$Order.Date)
#It is of range between 2011-01-01 and 2014-12-31
global_store$Order.Year_Month <- format(as.Date(global_store$Order.Date), "%Y-%m")

#Aggregating data by Sales, Profit & Quantity & CV
#----------------------------------------------------
#CV is often expressed as a percentage, and is defined as the ratio of the standard deviation to the mean.
#Therefore CV Of profit will be sd(Profit)*100/mean(Profit)

global_store_aggregated <- global_store %>%  group_by(Market_Segment) %>% 
                    summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)/mean(Profit))            

#Aggregating data monthly on each atribute
global_store_monthly <- global_store %>% group_by(Market_Segment, Order.Year_Month) %>% 
                  summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)/mean(Profit))

colnames(global_store_monthly) <- c("Market_Segment","Order.Year_Month","Total_Sales","Total_Profit",
                             "Total_Quantity","CV")

#Plotting is performed to identify the most profitable segments
#-------------------------------------------------------------------------
#wWe have to consider each market levels,segments,profit and sales
global_store_plot <- global_store[,c("Market","Segment","Profit","Sales")] %>% 
                group_by(Market,Segment)%>% summarise(., sum(Sales),sum(Profit),sd(Profit)/mean(Profit))

colnames(global_store_plot) = c("Market","Segment","Sales","Profit","CV")

#Plotting market and segments producing large sales
ggplot(global_store_plot, aes(Segment, Sales, fill=Market)) + geom_bar(stat="identity",position="dodge")+
     xlab("Segment")+ylab("Sales")+ggtitle("Total Sales")

#Plotting market and segments generating large profit
ggplot(global_store_plot, aes(Segment, Profit, fill=Market)) + geom_bar(stat="identity",position="dodge")+
     xlab("Segment")+ylab("Profit")+ggtitle("Total Profit")

#Plotting coefficient of variation Coeff. in  profit wrt Markets and Segments
ggplot(global_store_plot, aes(Segment, CV, fill=Market)) + geom_bar(stat="identity",position="dodge")+
    xlab("Segment")+ ylab("Coefficient of Variation")+
    ggtitle("Coefficient of Variation wrt Markets and Segments")

#Based on the plots we can observe that APAC_Consumer & EU_Consumer are two market_segments
#with highest Profit & lowest CV .We are not considering US Market eventhough its CV iS low since its 
#profit is very low

#Considering the 2 most profitable segments-APAC_Consumer & EU_Consumer
#-------------------------------------------------------------------------
profit_segments <- subset(global_store_monthly, Market_Segment == "APAC_Consumer" | 
                            Market_Segment == "EU_Consumer")
str(profit_segments)

#Subsetting APAC_Consumer and EU_Consumer separately
APAC_Consumer_segment <- subset(profit_segments,Market_Segment == "APAC_Consumer")
APAC_Consumer_segment$MonthNum<-c(1:nrow(APAC_Consumer_segment))
APAC_Consumer_segment <- APAC_Consumer_segment[,c(7,3:6)]
colnames(APAC_Consumer_segment) <- c("Month","Sales","Profit","Quantity","CV")

#sum(APAC_Consumer_segment$Profit) - 222817.6
#sd(APAC_Consumer_segment$CV)/mean(APAC_Consumer_segment$CV) - 0.63

EU_Consumer_segment <- subset(profit_segments,Market_Segment == "EU_Consumer")
EU_Consumer_segment$MonthNum<-c(1:nrow(EU_Consumer_segment))
EU_Consumer_segment <- EU_Consumer_segment[,c(7,3:6)]
colnames(EU_Consumer_segment) <- c("Month","Sales","Profit","Quantity","CV")

#sum(EU_Consumer_segment$Profit) -188687.7
#sd(APAC_Consumer_segment$CV)/mean(APAC_Consumer_segment$CV) - 0.624
#####################################################################################################
#                      MODEL BUILDING AND MODEL EVALUATION
#####################################################################################################

#--------------------------APAC CONSUMER SALES FORECAST---------------------------------------------
#---------------------------------------------------------------------------------------------------
#Converting dataframe to time series
#------------------------------------------
nrow(APAC_Consumer_segment) #48

#Let's create the model for first 42 rows.
#Then we can test the model on the remaining 6 rows later

APAC_Consumer_Sales_totaltimeser <- ts(APAC_Consumer_segment$Sales)
APAC_Consumer_Sales_indata <- APAC_Consumer_segment[1:42,]
APAC_Consumer_Sales_timeser <- ts(APAC_Consumer_Sales_indata$Sales)
plot(APAC_Consumer_Sales_timeser)

APAC_Consumer_Sales_test <- APAC_Consumer_segment[43:48,]

#Smoothing the series - Moving Average Smoothing
#------------------------------------------------

w <-1
smoothedseries <- stats::filter(APAC_Consumer_Sales_timeser, filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Consumer_Sales_timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- APAC_Consumer_Sales_indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
summary(lmfit)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- APAC_Consumer_Sales_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#ARIMA(0,0,0) with zero mean  

#sigma^2 estimated as 8.8e+07:  log likelihood=-443.75
#AIC=889.49   AICc=889.59   BIC=891.23


#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's forecast for the next 6 months

timevals_out <- APAC_Consumer_Sales_test$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_Consumer_Sales_test$Sales)[5]
MAPE_class_dec #31.07429

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_Consumer_Sales_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")


#Exponential smoothing
#------------------------------------------

exp_smoothedseries <- HoltWinters(APAC_Consumer_Sales_timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
plot(APAC_Consumer_Sales_timeser, col="black")
lines(fitted(exp_smoothedseries)[,1], col="red", lwd=1)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
exp_timevals_in <- c(1:42)
exp_smootheddf <- as.data.frame(cbind(exp_timevals_in, as.vector(fitted(exp_smoothedseries)[,1])))
colnames(exp_smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Exp_smooth <- lm(Sales ~ sin(0.5*Month) + Month, data=exp_smootheddf)
global_pred <- predict(lmfit_Exp_smooth, Month=exp_timevals_in)
summary(global_pred)
lines(exp_timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- APAC_Consumer_Sales_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 116119090:  log likelihood=-449.57
#AIC=901.14   AICc=901.24   BIC=902.87
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
exp_timevals_out <- APAC_Consumer_Sales_test$Month
global_pred_out <- predict(lmfit_Exp_smooth,data.frame(Month = exp_timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_Consumer_Sales_test$Sales)[5]
MAPE_class_dec #21.22845


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_Consumer_Sales_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")

#Model 2: So, that was classical decomposition, now let's do an ARIMA fit
#-------------------------------------------------------------------------
autoarima <- auto.arima(APAC_Consumer_Sales_timeser)
autoarima
#ARIMA(0,1,1) 
#sigma^2 estimated as 174361555:  log likelihood=-447.11
#AIC=898.23   AICc=898.55   BIC=901.66

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- APAC_Consumer_Sales_timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer_Sales_test$Sales)[5]
MAPE_auto_arima #27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(APAC_Consumer_Sales_totaltimeser, col = "black")
lines(auto_arima_pred, col = "red")

#In APAC_Consumere Sales - we could see  MAPE for 3 different models:
# Moving average -31.07429
# Exponential Smoothing - 21.22845
# Auto Arima - 27.68952

#FORECASTING APAC_SALES
#-------------------------
# Forecasting next 6 months sales using MAPE Exponential smoothing method since MAPE is less.

timevals_out <- c(49:54)
APAC_sales_forecast <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(APAC_sales_forecast))
plot(ts(APAC_Consumer_Sales_test$Sales), col = "black",xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")

#--------------------------APAC CONSUMER QUANTITY FORECAST---------------------------------------------
#---------------------------------------------------------------------------------------------------
#Converting dataframe to time series
#-----------------------------------------
nrow(APAC_Consumer_segment) #48

#Let's create the model for first 42 rows.
#Then we can test the model on the remaining 6 rows later

APAC_Consumer_Quantity_totaltimeser <- ts(APAC_Consumer_segment$Quantity)
APAC_Consumer_Quantity_indata <- APAC_Consumer_segment[1:42,]
APAC_Consumer_Quantity_timeser <- ts(APAC_Consumer_Quantity_indata$Quantity)
plot(APAC_Consumer_Quantity_timeser)

APAC_Consumer_Quantity_test <- APAC_Consumer_segment[43:48,]

#Smoothing the series - Moving Average Smoothing
#-----------------------------------------------

w <-1
smoothedseries <- stats::filter(APAC_Consumer_Quantity_timeser, filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Consumer_Quantity_timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- APAC_Consumer_Quantity_indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
summary(lmfit)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- APAC_Consumer_Quantity_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#ARIMA(0,0,0) with zero mean  

#sigma^2 estimated as 8.8e+07:  log likelihood=-443.75
#AIC=889.49   AICc=889.59   BIC=891.23


#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's forecast for the next 6 months

timevals_out <- APAC_Consumer_Quantity_test$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_Consumer_Quantity_test$Quantity)[5]
MAPE_class_dec #62.10289

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_Consumer_Quantity_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")


## Exponential smoothing
#-----------------------------------------

exp_smoothedseries <- HoltWinters(APAC_Consumer_Quantity_timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
plot(APAC_Consumer_Quantity_timeser, col="black")
lines(fitted(exp_smoothedseries)[,1], col="red", lwd=1)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
exp_timevals_in <- c(1:42)
exp_smootheddf <- as.data.frame(cbind(exp_timevals_in, as.vector(fitted(exp_smoothedseries)[,1])))
colnames(exp_smootheddf) <- c('Month', 'Quantity')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Exp_smooth <- lm(Quantity ~ sin(0.5*Month) + Month, data=exp_smootheddf)
global_pred <- predict(lmfit_Exp_smooth, Month=exp_timevals_in)
summary(global_pred)
lines(exp_timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- APAC_Consumer_Quantity_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 14980:  log likelihood=-261.5
#AIC=525   AICc=525.1   BIC=526.74
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
exp_timevals_out <- APAC_Consumer_Quantity_test$Month

global_pred_out <- predict(lmfit_Exp_smooth,data.frame(Month = exp_timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_Consumer_Quantity_test$Quantity)[5]
MAPE_class_dec #25.0091


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_Consumer_Quantity_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")

#Model 2: So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(APAC_Consumer_Quantity_timeser)
autoarima
#ARIMA(0,1,0) 
#sigma^2 estimated as 25366:  log likelihood=-266.07
#AIC=534.14   AICc=534.24   BIC=535.85

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- APAC_Consumer_Quantity_timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer_Quantity_test$Quantity)[5]
MAPE_auto_arima #26.24458

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(APAC_Consumer_Quantity_totaltimeser, col = "black")
lines(auto_arima_pred, col = "red")

#In APAC_Consumer Quantity- we could see  MAPE for 3 different models:
# Moving average - 62.10289
# Exponential Smoothing - 25.0091
# Auto Arima - 26.24458

#FORECASTING APAC_CONSUMER QUANTITY
#--------------------------------------
# Forecasting next 6 months quantity using MAPE Exponential smoothing method since MAPE is less.

timevals_out <- c(49:54)
APAC_Quantity_forecast <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(APAC_Quantity_forecast))
plot(ts(APAC_Consumer_Quantity_test$Quantity), col = "black",xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")

#--------------------------EU CONSUMER SALES FORECAST---------------------------------------------
#---------------------------------------------------------------------------------------------------
#Converting dataframe to time series
#--------------------------------------
nrow(EU_Consumer_segment) #48

#Let's create the model for first 42 rows.
#Then we can test the model on the remaining 6 rows later

EU_Consumer_Sales_totaltimeser <- ts(EU_Consumer_segment$Sales)
EU_Consumer_Sales_indata <- EU_Consumer_segment[1:42,]
EU_Consumer_Sales_timeser <- ts(EU_Consumer_Sales_indata$Sales)
plot(EU_Consumer_Sales_timeser)

EU_Consumer_Sales_test <- EU_Consumer_segment[43:48,]

#Smoothing the series - Moving Average Smoothing
#-----------------------------------------------

w <-1
smoothedseries <- stats::filter(EU_Consumer_Sales_timeser, filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Consumer_Sales_timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- EU_Consumer_Sales_indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
summary(lmfit)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- EU_Consumer_Sales_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#ARIMA(0,0,0) with zero mean  

#sigma^2 estimated as 92551568:  log likelihood=-444.8
#AIC=891.61   AICc=891.71   BIC=893.35

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's forecast for the next 6 months

timevals_out <- EU_Consumer_Sales_test$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,EU_Consumer_Sales_test$Sales)[5]
MAPE_class_dec #92.95788

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_Consumer_Sales_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")


## Exponential smoothing
#--------------------------------------

exp_smoothedseries <- HoltWinters(EU_Consumer_Sales_timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
plot(EU_Consumer_Sales_timeser, col="black")
lines(fitted(exp_smoothedseries)[,1], col="red", lwd=1)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
exp_timevals_in <- c(1:42)
exp_smootheddf <- as.data.frame(cbind(exp_timevals_in, as.vector(fitted(exp_smoothedseries)[,1])))
colnames(exp_smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Exp_smooth <- lm(Sales ~ sin(0.5*Month) + Month, data=exp_smootheddf)
global_pred <- predict(lmfit_Exp_smooth, Month=exp_timevals_in)
summary(global_pred)
lines(exp_timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- EU_Consumer_Sales_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 116622422:  log likelihood=-449.66
#AIC=901.32   AICc=901.42   BIC=903.06
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
exp_timevals_out <-EU_Consumer_Sales_test$Month

global_pred_out <- predict(lmfit_Exp_smooth,data.frame(Month = exp_timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,EU_Consumer_Sales_test$Sales)[5]
MAPE_class_dec #24.60974


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_Consumer_Sales_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")

#Model 2: So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(EU_Consumer_Sales_timeser)
autoarima
#ARIMA(2,1,0) 
#sigma^2 estimated as 168564623:  log likelihood=-445.84
#AIC=897.67   AICc=898.32   BIC=902.81

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- EU_Consumer_Sales_timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer_Sales_test$Sales)[5]
MAPE_auto_arima #27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(EU_Consumer_Sales_totaltimeser, col = "black")
lines(auto_arima_pred, col = "red")

#In EU_Consumer Sales - we could see  MAPE for 3 different models:
# Moving average -92.95788
# Exponential Smoothing - 24.60974
# Auto Arima - 28.9226

#FORECASTING EU_CONSUMER SALES
#--------------------------------------
# Forecasting next 6 months sales using MAPE Exponential smoothing method since MAPE is less.

timevals_out <- c(49:54)
EU_sales_forecast <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(EU_sales_forecast))
plot(ts(EU_Consumer_Sales_test$Sales), col = "black",xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")

#--------------------------EU CONSUMER QUANTITY FORECAST---------------------------------------------
#---------------------------------------------------------------------------------------------------
#Converting dataframe to time series
#--------------------------------------
nrow(EU_Consumer_segment) #48

#Let's create the model for first 42 rows.
#Then we can test the model on the remaining 6 rows later

EU_Consumer_Quantity_totaltimeser <- ts(EU_Consumer_segment$Quantity)
EU_Consumer_Quantity_indata <- EU_Consumer_segment[1:42,]
EU_Consumer_Quantity_timeser <- ts(EU_Consumer_Quantity_indata$Quantity)
plot(EU_Consumer_Quantity_timeser)

EU_Consumer_Quantity_test <- EU_Consumer_segment[43:48,]

#Smoothing the series - Moving Average Smoothing
#--------------------------------------

w <-1
smoothedseries <- stats::filter(EU_Consumer_Quantity_timeser, filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Consumer_Quantity_timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- EU_Consumer_Quantity_indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
summary(lmfit)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- EU_Consumer_Quantity_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#ARIMA(2,0,0) with zero mean  

#sigma^2 estimated as 7284:  log likelihood=-245.89
#AIC=497.79   AICc=498.42   BIC=503

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's forecast for the next 6 months

timevals_out <- EU_Consumer_Quantity_test$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,EU_Consumer_Quantity_test$Quantity)[5]
MAPE_class_dec #30.39741

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_Consumer_Quantity_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")


## Exponential smoothing
#--------------------------------------

exp_smoothedseries <- HoltWinters(EU_Consumer_Quantity_timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
plot(EU_Consumer_Quantity_timeser, col="black")
lines(fitted(exp_smoothedseries)[,1], col="red", lwd=1)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
exp_timevals_in <- c(1:42)
exp_smootheddf <- as.data.frame(cbind(exp_timevals_in, as.vector(fitted(exp_smoothedseries)[,1])))
colnames(exp_smootheddf) <- c('Month', 'Quantity')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Exp_smooth <- lm(Quantity ~ sin(0.5*Month) + Month, data=exp_smootheddf)
global_pred <- predict(lmfit_Exp_smooth, Month=exp_timevals_in)
summary(global_pred)
lines(exp_timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- EU_Consumer_Quantity_timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
#ARIMA(1,0,2) with zero mean 

#sigma^2 estimated as 13370:  log likelihood=-257.81
#AIC=523.63   AICc=524.71   BIC=530.58
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
exp_timevals_out <- EU_Consumer_Quantity_test$Month

global_pred_out <- predict(lmfit_Exp_smooth,data.frame(Month = exp_timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast, EU_Consumer_Quantity_test$Quantity)[5]
MAPE_class_dec #27.40062


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_Consumer_Quantity_totaltimeser , col = "black")
lines(class_dec_pred, col = "red")

#Model 2: So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(EU_Consumer_Quantity_timeser)
autoarima
#ARIMA(2,1,0)  
#sigma^2 estimated as 21185:  log likelihood=-261.9
#AIC=529.8   AICc=530.44   BIC=534.94

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- EU_Consumer_Quantity_timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer_Quantity_test$Quantity)[5]
MAPE_auto_arima #30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(EU_Consumer_Quantity_totaltimeser, col = "black")
lines(auto_arima_pred, col = "red")

#In EU_Consumer Quantity - we could see  MAPE for 3 different models:
# Moving average - 30.39741
# Exponential Smoothing - 27.40062
# Auto Arima - 30.13319

#FORECASTING EU_CONSUMER_QUANTITY
#--------------------------------------
# Forecasting next 6 months quantity using MAPE Exponential smoothing method since MAPE is less.

timevals_out <- c(49:54)
EU_Quantity_forecast <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(EU_Quantity_forecast))
plot(ts(EU_Consumer_Quantity_test$Quantity), col = "black",xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")
