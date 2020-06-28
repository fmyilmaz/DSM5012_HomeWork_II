
##----------------------------------------------------------------
##                       Loading Packages                       --
##----------------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(zoo) # S3 Infrastructure for Regular and Irregular Time Series (Z's Ordered Observations), CRAN v1.8-8
library(fpp2) # Data for "Forecasting: Principles and Practice" (2nd Edition), CRAN v2.3
library(forecast) # Forecasting Functions for Time Series and Linear Models, CRAN v8.12
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics, CRAN v2.3
library(stargazer) # Well-Formatted Regression and Summary Statistics Tables, CRAN v5.2.2
library(xtable) # Export Tables to LaTeX or HTML, CRAN v1.8-4

##----------------------------------------------------------------
##                         Loading Data                         --
##----------------------------------------------------------------

users_data <- read.csv('data/timeseries.csv')
users_data <- users_data[,-1]
users_date <- as.POSIXct(as.character(users_data$time, format="%Y-%m-%d %H:%M"))
users_data <- zoo(users_data[,c('users','newusers')], order.by = users_date) 

##----------------------------------------------------------------
##                   Plotting and Decomposing                   --
##----------------------------------------------------------------

autoplot(users_data,facets = Series ~ .) + ggtitle("User and New User Count by Hour") + 
  ylab("# of Users and New User") + xlab("")  + facet_free() + theme(legend.position = 'bottom')

gglagplot(users_data$users, lags = 12) + 
  ggtitle("# of Lags for Users")

gglagplot(users_data$newusers, lags = 12) + 
  ggtitle("# of Lags for New Users")

user_acf <- ggAcf(users_data$users, lag.max = 60) + ggtitle("ACF Plot of User")
newuser_acf <- ggAcf(users_data$newusers, lag.max = 60) + ggtitle("ACF Plot of New User")
grid.arrange(user_acf, newuser_acf)

user_pacf <- ggPacf(users_data$users, lag.max = 60) + ggtitle("PACF Plot of User")
newuser_pacf <- ggPacf(users_data$newusers, lag.max = 60) + ggtitle("PACF Plot of New User")
grid.arrange(user_pacf, newuser_pacf)

##---------------------------------------------------------------
##                     Data Pre-Processing                     --
##---------------------------------------------------------------

first_diff <- diff(users_data, 1)
diff_user_acf <- ggAcf(first_diff$users, lag.max = 60) + ggtitle("ACF Plot of User")
diff_newuser_acf <- ggAcf(first_diff$newusers, lag.max = 60) + ggtitle("ACF Plot of New User")
grid.arrange(diff_user_acf, diff_newuser_acf)

second_diff <- diff(first_diff,12)
diff_user_acf2 <- ggAcf(second_diff$users, lag.max = 60) + ggtitle("ACF Plot of User")
diff_newuser_acf2 <- ggAcf(second_diff$newusers, lag.max = 60) + ggtitle("ACF Plot of New User")
grid.arrange(diff_user_acf2, diff_newuser_acf2)

diff_user_pacf <- ggPacf(second_diff$users) + ggtitle("PACF Plot of User")
diff_newuser_pacf <- ggPacf(second_diff$newusers) + ggtitle("PACF Plot of New User")
grid.arrange(diff_user_pacf, diff_newuser_pacf)

##---------------------------------------------------------------
##                          Modeling                          --
##---------------------------------------------------------------

users_model <- arima(users_data$users, order = c(2,1,1), seasonal = list(order = c(2,1,1), period =12))
checkresiduals(users_model)
stargazer(users_model, title = 'SARIMA models for New Users',
          column.labels = c('ARIMA(2,1,1)(2,1,1)'), 
          covariate.labels = c('AR(1)','AR(2)','MA(1)','SAR(1)','SAR(2)','SMA(1)'))

newusers_model1 <- arima(users_data$newusers, order = c(1,1,2), seasonal = list(order = c(1,0,1), period =12))
checkresiduals(newusers_model1)
stargazer(newusers_model1, title = 'SARIMA models for New Users',
          column.labels = c('ARIMA(1,1,2)(1,0,1)'),
          covariate.labels = c('AR(1)','MA(1)','MA(2)','SAR(1)','SMA(1)'))



##---------------------------------------------------------------
##                         Forecasting                         --
##---------------------------------------------------------------


user_forecast <- forecast(users_model, h = 12)
autoplot(user_forecast)
xtable(data.frame(user_forecast))

newuser_forecast <- forecast(newusers_model1, h = 12)
autoplot(newuser_forecast)
xtable(data.frame(newuser_forecast))






