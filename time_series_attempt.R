library(tidyverse)
library(forecast)
library(zoo)
library(lubridate)

chile = read.csv('chile_2004_today.csv')

chile_prophet = chile %>% 
                select(time, mag) %>%
                mutate(time=date(time)) %>%
                arrange(time, -mag) %>%
                filter (!duplicated (time))

chile_zoo = read.zoo(chile_prophet)

tsdisplay(chile_zoo)

auto.arima(chile_zoo)

model1 = Arima(chile_zoo, order = c(5, 1, 0))

model1 %>% residuals() %>% tsdisplay(points = F, 
                                       main = paste('AIC:', round(model1$aic)))

library(prophet)

chile_prophet2 = chile_prophet %>% rename(ds=time, y=mag)

prophet_model = prophet(chile_prophet2)

prophet_future = make_future_dataframe(prophet_model, periods = 365)

prophet_forecast = predict(prophet_model, prophet_future)

plot(prophet_model, prophet_forecast)

prophet_plot_components(prophet_model, prophet_forecast)
