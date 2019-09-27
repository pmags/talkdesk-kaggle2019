
# ======================================================================================= #
# Script Name : model_1                                                                                               
# Purpose     : simple model based on given features                                                                     
# Args        : 
# Date        : Mon Sep 23 09:46:39 2019    
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #

# import libraries 
library(tidyverse)
library(caret)
library(xts)
library(forecast)
library(lubridate)
library(reshape2)
library(quantmod)
library(TSA)
library(tsoutliers)
library(prophet)


# ======================================================================================= #
# Importing data and transform to time series ----                                  
# ======================================================================================= #

## XTS

# importing train data from csv
data <- read.csv("data/train.csv", header = T)

# Converts data into xts class for time series manipulation
xts_data <- as.xts(x = select(data,-interval), 
                   order.by = as.POSIXct(strptime(data$interval,"%Y-%m-%d %H:%M:%S"), 
                                         tz = "UTC"))

# Creating end points to be used on plots
ep_quarters <- endpoints(xts_data,on="quarters")
ep_months <- endpoints(xts_data,on="months") 
ep_weeks <- endpoints(xts_data,on="weeks") 
ep_days <- endpoints(xts_data,on="days") 
ep_hours <- endpoints(xts_data,on="hours") 

# ======================================================================================= #
# Modeling total calls using prophet ----                                         
# ======================================================================================= #

# data transformation necessacy
calls <- select(data, interval, total_calls) %>% 
  mutate(interval = as.POSIXct(strptime(interval,"%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  mutate(ds = interval, y = total_calls)

calls <- column_to_rownames(calls, var = "interval")

lam = BoxCox.lambda(calls$total_calls)
calls$y = BoxCox(calls$total_calls, lam)

m <-prophet(daily.seasonality = FALSE,
            weekly.seasonality = FALSE,
            yearly.seasonality = FALSE,
            n.changepoints = 24*8, 
            changepoint_prior_scale = 0.25)

m <- add_seasonality(m, name="monthly", period = 30.5, fourier.order = 20)
m <- add_seasonality(m, name="daily", period = 1, fourier.order = 20)
m <- add_seasonality(m, name="weekly", period = 7, fourier.order = 20)
m <- add_seasonality(m, name="semester", period = 365.25/2, fourier.order = 20,prior.scale = 20)
m <- fit.prophet(m,calls)

future <- make_future_dataframe(m, periods = 96*31, freq = 60*15)
forecast <- predict(m,future)
prophet_plot_components(m, forecast)

# xts_calls <- as.xts(forecast %>% select(yhat)%>% mutate(y_transf = exp(log(lam*yhat+1)/lam)),
#                     order.by = as.POSIXct(strptime(forecast$ds,"%Y-%m-%d %H:%M:%S"),
#                                           tz = "UTC"))

xts_calls <- as.xts(forecast %>% select(yhat)%>% mutate(y_transf = InvBoxCox(forecast$yhat,lam)),
                   order.by = as.POSIXct(strptime(forecast$ds,"%Y-%m-%d %H:%M:%S"),
                                           tz = "UTC"))

xts <- as.xts(calls %>% select(y),order.by = as.POSIXct(strptime(calls$ds,"%Y-%m-%d %H:%M:%S"), 
                                                        tz = "UTC"))

xts <- merge(xts, xts_calls,join = "inner") 

mape_calls <- xts %>% as.data.frame() %>%  
  mutate(t = abs(y-yhat)/y*100) %>%
  mutate(t = ifelse(is.infinite(t),0,t)) %>%  
  summarise(MAPE = mean(t,na.rm = TRUE))

# ======================================================================================= #
# Modeling average call duration ----                                         
# ======================================================================================= #

# data transformation necessacy
duration <- select(data, interval, total_calls,total_calls_duration) %>%
  mutate(avg_duration = ifelse(is.na(total_calls_duration/total_calls), 0, total_calls_duration/total_calls)) %>% 
  mutate(interval = as.POSIXct(strptime(interval,"%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  mutate(ds = interval, y = avg_duration)

duration <- column_to_rownames(duration, var = "interval")

lam = BoxCox.lambda(duration$avg_duration)
duration$y = BoxCox(duration$avg_duration, lam)

fit_duration <-prophet(daily.seasonality = FALSE,
                       weekly.seasonality = FALSE,
                       yearly.seasonality = FALSE,
                       n.changepoints = 24*4, 
                       changepoint_prior_scale = 0.3)

fit_duration <- add_seasonality(fit_duration, name="monthly", period = 30.5, fourier.order = 215)
fit_duration <- add_seasonality(fit_duration, name="daily", period = 1, fourier.order = 25)
fit_duration <- add_seasonality(fit_duration, name="weekly", period = 7, fourier.order = 215)
fit_duration <- add_seasonality(fit_duration, name="semester", period = 365.25/2, fourier.order = 25, prior.scale = 20)
fit_duration <- fit.prophet(fit_duration,duration)


future_duration <- make_future_dataframe(fit_duration, periods = 96*31, freq = 60*15)
forecast_dur <- predict(fit_duration,future_duration)
prophet_plot_components(fit_duration,forecast_dur)

xts_dur <- as.xts(forecast_dur %>% select(yhat)%>% mutate(y_transf = exp(log(lam*yhat+1)/lam)),
                  order.by = as.POSIXct(strptime(forecast_dur$ds,"%Y-%m-%d %H:%M:%S"),
                                        tz = "UTC"))

# cv_dur <- cross_validation(fit_duration,initial = 67104/1.3, period = 96*31*12,horizon = 96*31,units = "mins")
# performance_metrics(cv)

xts_or <- as.xts(duration %>% select(y),order.by = as.POSIXct(strptime(duration$ds,"%Y-%m-%d %H:%M:%S"), 
                                                              tz = "UTC"))

xts_or <- merge(xts_or, xts_dur,join = "inner") 

mape_dur <- xts_or %>% as.data.frame() %>% 
  mutate(t = abs(y-yhat)/y*100) %>%
  mutate(t = ifelse(is.infinite(t),0,t)) %>%  
  summarise(MAPE = mean(t,na.rm = TRUE))

# ======================================================================================= #
# Train agent model ----                                         
# ======================================================================================= #

# Assuming a linear polynomial relationship between total call duration and agent headcount

train_control <- trainControl(method = "repeatedcv", 
                              number = 3, 
                              repeats = 3, 
                              verboseIter = TRUE)

agent_model <- train(agent_headcount ~ total_calls_duration,
                     data = xts_data,
                     method = "gam",
                     trControl = train_control,
                     preProc = c("center","scale"))


# Inputs for forecast
calls_dec <- xts_calls["2018-12"]
duration_dec <- xts_dur["2018-12"]

december <- merge(calls_dec, duration_dec,join = "inner") %>% 
  as.data.frame() %>% 
  rename(total_calls = y_transf, avg_duration = y_transf.1) %>% 
  select(-yhat,-yhat.1) %>%  
  rownames_to_column() %>% 
  mutate(total_calls_duration = total_calls * avg_duration) %>% 
  column_to_rownames() %>% 
  as.xts()

pred <- predict(object = agent_model, newdata = december) %>% as.xts()

results <- merge(december,pred,join = "inner")

csv_result <- period.apply(results[,c("pred")], on = "hourly", INDEX = endpoints(results,on="hours"),FUN = mean) %>% 
  as.data.frame() %>% 
  rename(Predicted = pred) %>% 
  mutate(Id = seq.int(nrow(.)),
         Predicted = round(Predicted)) %>% 
  select(Id,Predicted)

rownames(csv_result) <- c()

write.table(csv_result,file = "submissions/25-09-2019_.csv",row.names = FALSE,quote = FALSE, sep = ";")

  # score: MAE = 33