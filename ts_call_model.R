
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

# ======================================================================================= #
# Preparing data ----                                         
# ======================================================================================= #

data <- read.csv("data/train.csv", header = T)

# convert to a multiseason time series file
msts_data <- msts(select(data,-interval),
                  seasonal.periods = c(96, # daily
                                       96*7, # weekly
                                       96*31 # monthly
                                       ))

# prepare data to be used with prophet model
prophet_calls_total <- select(data, interval, total_calls) %>% 
  mutate(interval = as.POSIXct(strptime(interval,"%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  rename(ds = interval,y = total_calls)

prophet_calls_avg_duration <- select(data, interval, total_calls, total_calls_duration) %>%
  mutate(avg_dur_calls = ifelse(is.na(total_calls_duration/total_calls), 0, total_calls_duration/total_calls)) %>% 
  mutate(interval = as.POSIXct(strptime(interval,"%Y-%m-%d %H:%M:%S"),tz = "UTC")) %>% 
  mutate(ds = interval) %>% 
  rename(y = avg_dur_calls) %>% 
  column_to_rownames(var = "interval")


# ======================================================================================= #
# Modeling total number of calls ----                                         
# ======================================================================================= #

# fitting a model
m_number_calls <- prophet(prophet_calls_total)

future_calls <- make_future_dataframe(m_number_calls,
                                      periods = 96*31, # 31 days 
                                      freq = 60*15) # every 15 minutes

# Forecast the tota number of calls for the specific period
fct_calls <- predict(m_number_calls,future_calls)
prophet_plot_components(m_number_calls,fct_calls)

# fitted values
fit_calls <- as.xts(fct_calls$yhat, 
                    order.by = as.POSIXct(strptime(fct_calls$ds,"%Y-%m-%d %H:%M:%S"),
                                                          tz = "UTC"))

merge_calls <- merge(fit_calls,as.xts(prophet_calls_total[-1],order.by = prophet_calls_total$ds), join = "inner")
merge_calls$errors <- merge_calls$fit_calls - merge_calls$y 
merge_calls$mape <- (merge_calls$fit_calls - merge_calls$y)/merge_calls$y 
merge_calls$mape <- ifelse(is.infinite(merge_calls$mape),0,merge_calls$mape)

# TODO: É preciso criar uma matriz com métricas para os modelos

mape_calls <- merge_calls %>% 
  as.data.frame() %>% 
  summarise(MAPE = mean(mape,na.rm = TRUE))

prophet_plot_components(m_number_calls, fct_calls)

# TODO: Usar a função de limpeza de outliers do ts package










