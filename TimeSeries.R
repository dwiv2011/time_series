#' ---
#' title: "Forecasting"
#' author: "Deepak"
#' date: "08/05/2021"
#' output: html_document
#' ---
#' #### Importing Library
#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------------

#install.packages(c("imputeTS","forecast","fpp2","wavelets","zoo","ggpubr","TSA","xts","timeSeries"))
 #importing librar
#install.packages("forecast","fpp")
library(imputeTS)
library(forecast)
library(fpp2)
library(wavelets)
library(zoo)
library(ggpubr)
library(TSA)
library(xts)
library(timeSeries)
library(tseries) 
library(knitr)


#' 
#' 
## ------------------------------------------------------------------------------------------------------------
Data_csv<- read.csv("SouvenirSales.csv")
print(head(Data_csv))
# converting in timeseris
data_ts <- ts(Data_csv$Sales, start = c(1995,1), frequency = 12)
# plotting the data
autoplot(data_ts) +   ylab("Sales") + xlab("Year") + ggtitle("Sales Data") + theme(plot.title = element_text(hjust = 0.5))

# Decomposing the plot Using Additive 
A1 <- decompose(data_ts, type= "additive")
#print(summary(A1))
g1 <- autoplot(A1)
# Decomposing the plot Using multiplicative
A2 <- decompose(data_ts, type= "multiplicative")
#print(summary(A2))
g2 <- autoplot(A2)
ggarrange(g1,g2)



#' 
## ------------------------------------------------------------------------------------------------------------
#Splitting data in train and test

train<- window(data_ts,end=c(2000,12), frequency=12)
test<- window(data_ts,start=c(2001,1), frequency=12)

#Linear trend and additive  model

trend_lnr<-tslm(train ~ trend+season)
train_pred_lnr <- forecast(trend_lnr, h=length(test ), level = 0)
plot(train_pred_lnr, xlab ="Time", ylab= "Sales" ,col='red')
lines(train_pred_lnr$fitted,col='blue')
lines(test,col="red")
abline(v=2001, col="black", lwd=2)
text(2001.5,60000, "Validation ")
text(1998,60000, "Training ")

ta<-ts(test-train_pred_lnr$mean)
ac<-ts(c(train_pred_lnr$residuals,ta),start=start(train_pred_lnr$residuals),frequency=frequency(train_pred_lnr$residuals))
plot(ac, xlab ="Time", ylab= "Sales" , bty="l")
abline(v=2001, col="black", lwd=2)
text(2001.5,30000, "Validation ")
text(1998,30000, "Training ")

#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------------
#exponential trend and multiplicative  model

trend_exp<-tslm(train ~ trend+season,lambda=0 )
train_pred_exp <- forecast(trend_exp, h=length(test), level = 0)
#plot(train_pred_exp, xlab ="Time", ylab= "Sales" ,flty=2, bty="l",ylim=c(0,100000))
#lines(train_pred_exp$fitted)
#lines(test,col="blue")
plot(train_pred_exp, xlab ="Time", ylab= "Sales" ,col='red',ylim=c(0,100000))
lines(train_pred_exp$fitted,col='blue')
lines(test,col="red")
abline(v=2001, col="black", lwd=2)
text(2001.5,60000, "Validation ")
text(1998,60000, "Training ")

tm<-ts(test-train_pred_exp$mean)
ac<-ts(c(train_pred_exp$residuals,tm),start=start(train_pred_exp$residuals),frequency=frequency(train_pred_exp$residuals))
plot(ac, xlab ="Time", ylab= "Sales" , bty="l")
abline(v=2001, col="red", lwd=2)
text(2001.5,3000, "Validation ")
text(1998,3000, "Training ")




#' 
#' 
## ------------------------------------------------------------------------------------------------------------

#residual plot

plot(ta, xlab ="Time", ylab= "Sales" , bty="l")
lines(tm,col="blue")

accuracy(train_pred_lnr,test)
print("Expoenential trend mode")
accuracy(train_pred_exp,test)

summary(trend_lnr)
print("Expoenential trend mode")
summary(trend_exp)


#' 
## ------------------------------------------------------------------------------------------------------------

# building model
final_model<-tslm(data_ts ~ trend+season,lambda=0 )
final_model_pred <- forecast(final_model, h=1, level = 95)
print(final_model_pred$mean)
plot(final_model_pred, xlab ="Time", ylab= "Sales" ,flty=2, bty="l",ylim=c(0,100000))
lines(final_model_pred$fitted)

plot(final_model_pred$residuals, xlab ="Time", ylab= "Sales", bty="l")





#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------------
# plotting ACF and PACF plot
Acf(train_pred_exp$residuals, lag.max = 20)
Pacf(train_pred_exp$residuals, lag.max = 20)
kpss.test(train_pred_exp$residuals) ## Check stationarity
# running AR(2) model
model_res_arima <- Arima(train_pred_exp$residuals, order = c(2,0,0))
summary(model_res_arima)
forecast_error <- model_res_arima$fitted
diff <- train_pred_exp$residuals -forecast_error
combined <- cbind(train_pred_exp$residuals,forecast_error,diff )

#final train model data 
final_model_pred <- forecast(trend_exp, h=length(test), level = 0)
final_model_pred2<-forecast(model_res_arima,h=length(test),level=0)

Final_test_pred=final_model_pred$mean+final_model_pred2$mean
Final_test_pred

#' 
## ------------------------------------------------------------------------------------------------------------
# running AR(2) model
final_model_res_arima <- Arima(final_model_pred$residuals, order = c(2,0,0))

#final model data 
final_model_pred <- forecast(final_model, h=1, level = 0)
final_model_pred2<-forecast(final_model_res_arima,h=1,level=0)

Final_pred=final_model_pred$mean+final_model_pred2$mean
Final_pred



