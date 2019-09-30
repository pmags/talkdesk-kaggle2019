
# ======================================================================================= #
# Script Name : Total calls & call duration forecast models                                                                                           
# Purpose     : Train & test models that forecast for the last month on a daily basis the
#               total amount of calls and its duration
# Args        : 
# Date        : Thu Sep 26 19:04:20 2019   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #

# Import libraries
library(tidyverse)
library(xts)
library(forecast)
library(lubridate)
library(tsoutliers)
library(prophet)
library(caret)

set.seed(123)

# ======================================================================================= #
# Preparing total calls data ----                                         
# ======================================================================================= #

data <- read.csv("data/train.csv", header = T)

xts_data <-  as.xts(x = select(data,-interval), 
                    order.by = as.POSIXct(strptime(data$interval,"%Y-%m-%d %H:%M:%S"), tz = "UTC"))

# prepare data to be used with prophet model (time stanps = ds, data = y)
calls_total <- select(data, interval, total_calls) %>% 
  mutate(interval = as.POSIXct(strptime(interval,"%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  rename(ds = interval, y = total_calls)

# using tsclean to remove outliers
calls_total <- forecast::tsclean(as.xts(calls_total[-1],order.by = calls_total$ds)) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "ds") %>%
  mutate(ds = as.POSIXct(strptime(ds,"%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  # eliminating year end outliers
  # Prophet package use linear interpolation in the event of missing values
  mutate(y = ifelse(as.character(date(ds)) %in% c("2018-09-03","2018-09-04",
                                                  "2018-09-14","2018-09-15",
                                                  "2018-09-21","2018-09-22"), NA, y)) 

# ======================================================================================= #
# Modeling total number of calls ----                                         
# ======================================================================================= #

# Xmas holidays
xmas <- data_frame(
  holiday = "xmas & new year",
  ds = as.Date(c("2017-12-25","2017-12-31")),
  lower_window = -3,
  upper_window = 3
)

# fitting a model
# the series has Heteroscedasticity so we use a multiplicative model 
m_calls_total <- prophet(changepoint.prior.scale = 0.6,
                          seasonality.mode = "multiplicative",
                          weekly.seasonality = 25,
                          daily.seasonality = 25,
                          holidays = xmas) 

m_calls_total <- add_seasonality(m_calls_total,
                                 name = "monthly",
                               period = 30.5*6,
                               fourier.order = 25,
                               prior.scale = 0.6)

m_calls_total <- fit.prophet(m_calls_total, calls_total)

# dataframe with time to be estimated
future_calls_total <- make_future_dataframe(m_calls_total,
                                      periods = 96*31, # 31 days 
                                      freq = 60*15) # every 15 minutes

# Forecast the total number of calls for the specific period
fct_calls_total <- predict(m_calls_total, future_calls_total)
#fct_calls_total$y <- InvBoxCox(fct_calls_total$yhat, lam)

# calculating cross validation metrics
calls_cv <- cross_validation(m_calls_total,horizon = 90, units = "days")

# because there could be zero calls at a specific time we will add a small noise to calculate MAPE 
calls_cv <- calls_cv %>% mutate(y = y + 0.01, yhat = yhat + 0.01)

metrics_calls_total <- performance_metrics(calls_cv, rolling_window = 1) %>%  
  mutate(nrmse = rmse /diff((range(calls_cv$y))),
         nrmse_iqr =rmse / IQR(calls_cv$y))


# ======================================================================================= #
# Total handle time ----                                         
# ======================================================================================= #

calls_clean <- tsclean(xts_data$total_calls)
handle_clean <- tsclean(xts_data$total_handle_time)
model_data <- merge(calls_clean, handle_clean, join = "inner")

set.seed(123)
partition <- createDataPartition(model_data$total_handle_time, p = 0.8, list = FALSE)
train <- model_data[partition,]
test <- model_data[-partition,]

# test linear relationship
trainControl <- trainControl(method = "repeatedcv", 
                             number = 5,
                             repeats = 5,
                             verboseIter = TRUE)


time_handle_model <- caret::train(form = total_handle_time ~ total_calls, 
                            data = train,
                            method = "gbm",
                            trControl = trainControl,
                            metric = "RMSE",
                            preProcess = c("center","scale"))

fit_handle_time <- predict(time_handle_model, test)
metrics_handle <- postResample(pred = fit_handle_time, obs = test$total_handle_time)

# ======================================================================================= #
# Busy time time series data ----                                         
# ======================================================================================= #

# # prepare data to be used with prophet model (time stanps = ds, data = y)
# busy_time <- select(data, interval, busy_time) %>% 
#   mutate(interval = as.POSIXct(strptime(interval,"%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
#   rename(ds = interval, y = busy_time) %>% 
#   filter(date(ds) >= "2018-03-18")
# 
# # using tsclean to remove outliers
# busy_time <- forecast::tsclean(as.xts(busy_time[-1],order.by = busy_time$ds)) %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "ds") %>%
#   mutate(ds = as.POSIXct(strptime(ds,"%Y-%m-%d %H:%M:%S"),tz = "UTC"))


# ======================================================================================= #
# Modeling busy time ----                                         
# ======================================================================================= #

# # fitting a model
# # the series has Heteroscedasticity so we use a multiplicative model 
# m_busy_time <- prophet(busy_time,
#                        changepoint.prior.scale = 0.5,
#                          seasonality.mode = "multiplicative",
#                          weekly.seasonality = 15,
#                          daily.seasonality = 15)
# # dataframe with time to be estimated
# future_busy_time <- make_future_dataframe(m_busy_time,
#                                             periods = 96*31, # 31 days 
#                                             freq = 60*15) # every 15 minutes
# 
# # Forecast the total number of calls for the specific period
# fct_busy_time <- predict(m_busy_time, future_busy_time)
# 
# 
# # calculating cross validation metrics
# busy_cv <- cross_validation(m_busy_time,horizon = 30, units = "days")
# 
# # because there could be zero calls at a specific time we will add a small noise to calculate MAPE 
# busy_cv <- busy_cv %>% mutate(y = y + 0.01, yhat = yhat + 0.01)
# 
# metrics_busy_time <- performance_metrics(busy_cv, rolling_window = 1) %>%  
#   mutate(nrmse = rmse /diff((range(busy_cv$y))),
#          nrmse_iqr =rmse / IQR(busy_cv$y))

# TODO: O modelo tem resultados negativos que têm de ser corrigidos

# ======================================================================================= #
# train agents headcount model ----                                         
# ======================================================================================= #

# gbm
set.seed(123)
partition <- createDataPartition(data$agent_headcount, p = 0.8, list = FALSE)

# set trian and test partition
train <- data[partition,]
test <- data[-partition,]

# test linear relationship
trainControl <- trainControl(method = "repeatedcv", 
                             number = 5,
                             repeats = 5,
                             verboseIter = TRUE)

agent_model <- caret::train(form = agent_headcount ~ total_calls + total_handle_time, 
                      data = train,
                      method = "gbm",
                      trControl = trainControl,
                      metric = "RMSE",
                      preProcess = c("center","scale"))

fit_agent <- predict(agent_model, test)
metrics_agent <- postResample(pred = fit_agent,obs = test$agent_headcount)

# ======================================================================================= #
# Plots & metrics ----                                         
# ======================================================================================= #

# fitted values of total calls
submission_test <- fct_calls_total %>% select(ds,yhat) %>% rename( total_calls = yhat)
submission_test <- as.xts(submission_test[-1], order.by = submission_test$ds) 

# merge with observed agent headcount
submission_test <- merge(submission_test,xts_data$agent_headcount, join = "inner")

# fitted total handle time
submission_test$total_handle_time <- predict(time_handle_model, submission_test)

# fitted agent headcount
submission_test$agent_headcount_fitted <- predict(agent_model, submission_test)

# calculating mae
submission_test$error <- submission_test$agent_headcount - submission_test$agent_headcount_fitted

mae <- function(error)
{
  mean(abs(error), na.rm = TRUE)
}

plot(period.apply(submission_test[,"error"], 
                  INDEX = endpoints(submission_test,on="months") ,
                  FUN = mae),
     type = "l", 
     xlab = "MAE",
     main = "Monthly Mae evolution") 

mae(submission_test$error)

# time series decomposition
prophet_plot_components(m_calls_total,fct_calls_total)

# ======================================================================================= #
# Preparing submission ----                                         
# ======================================================================================= #

fct_calls_total <- fct_calls_total %>% select(ds,yhat) %>% rename( total_calls = yhat)
calls_dec <-as.xts(fct_calls_total[-1],order.by = fct_calls_total$ds)["2018-12"]
calls_dec$total_handle_time <-predict(time_handle_model, calls_dec)
calls_dec$agent_headcount <- predict(object = agent_model, newdata = calls_dec)

csv_submisison <- period.apply(calls_dec[,c("agent_headcount")], on = "hourly",
                               INDEX = endpoints(calls_dec,on="hours"),FUN = mean) %>%
  as.data.frame() %>% 
  rename(Predicted = agent_headcount) %>%
  mutate(Id = seq.int(nrow(.)),
         Predicted = round(Predicted)) %>%
  select(Id,Predicted)

rownames(csv_submisison) <- c()

# december <-calls_dec %>%
#   as.data.frame() %>%
#   rename(total_calls = yhat) %>%
#   rownames_to_column() %>%
#   mutate(date = as.POSIXct(strptime(rowname,"%Y-%m-%d %H:%M:%S"),tz = "UTC"))
# 
# pred <- predict(object = agent_model, newdata = december) %>% as.xts(order.by = december$date)
# 
# submission <- merge(as.xts(select(december, -date,-rowname), order.by = december$date),pred,join = "inner")
# 
# csv_submisison <- period.apply(submission[,c("pred")], on = "hourly",
#                            INDEX = endpoints(submission,on="hours"),FUN = mean) %>%
#   as.data.frame() %>%
#   rename(Predicted = pred) %>%
#   mutate(Id = seq.int(nrow(.)),
#          Predicted = round(Predicted)) %>%
#   select(Id,Predicted)



write.csv(csv_submisison,file = paste0("submissions/",date(now()),".csv"),
          row.names = FALSE,
          quote = FALSE)

# score: MAE = 22
