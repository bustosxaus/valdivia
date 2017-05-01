library(tidyverse)
library(forecast)
library(zoo)
library(lubridate)

#chile 2004-2017
earth = read.csv('datasets/chile_2004_today.csv')

chile0417 = earth %>%
                select(time, mag) %>%
                mutate(time=date(time)) %>%
                arrange(time, -mag) %>%
                filter (!duplicated (time))

chile0417_zoo = read.zoo(chile0417)

ggtsdisplay(chile0417_zoo, main='Chile 2004-2017')

pdf("plots/tsdisplay1.pdf", width = 10)
ggtsdisplay(chile0417_zoo, main='Chile 2004-2017')
dev.off()

auto.arima(chile0417_zoo)

model1 = Arima(chile0417_zoo, order = c(5, 1, 0))

model1 %>% residuals() %>% tsdisplay(points = F, 
                                     main = paste('AIC:', round(model1$aic)))

pdf("plots/model1.pdf", width = 10)
model1 %>% residuals() %>% tsdisplay(points = F, 
                                     main = paste('AIC:', round(model1$aic)))
dev.off()

#chile 5 years
earth = read.csv('datasets/chile_df.csv')

chile5 = earth %>%
    select(time, mag) %>%
    mutate(time=date(time)) %>%
    arrange(time, -mag) %>%
    filter (!duplicated (time))

chile5_zoo = read.zoo(chile5)

ggtsdisplay(chile5_zoo, main='Chile last 5 years')

pdf("plots/tsdisplay2.pdf", width = 10)
ggtsdisplay(chile5_zoo, main='Chile last 5 years')
dev.off()

auto.arima(chile5_zoo)

model2 = Arima(chile5_zoo, order = c(0, 0, 0))

model2 %>% residuals() %>% tsdisplay(points = F, 
                                     main = paste('AIC:', round(model2$aic)))

#filtering two years
earth = read.csv('datasets/chile_2004_today.csv')

chile_2year = earth %>% 
    select(time, mag) %>%
    mutate(time=date(time)) %>%
    arrange(time, -mag) %>%
    filter(!duplicated (time)) %>%
    filter(year(time) %in% c(2009, 2010))

chile2year_zoo = read.zoo(chile_2year)

ggtsdisplay(chile2year_zoo, main='Chile 2009 and 2010')

pdf("plots/tsdisplay3.pdf", width = 10)
ggtsdisplay(chile2year_zoo, main='Chile 2009 and 2010')
dev.off()

auto.arima(chile2year_zoo)

model3 = Arima(chile2year_zoo, order = c(1, 1, 1))

model3 %>% residuals() %>% ggtsdisplay(points = F, 
                                     main = paste('AIC:', round(model3$aic)))

pdf("plots/model3.pdf", width = 10)
model3 %>% residuals() %>% ggtsdisplay(points = F, 
                                       main = paste('AIC:', round(model3$aic)))
dev.off()

#california 2004-2017
earth = read.csv('datasets/cali_df.csv')

california5 = earth %>%
    select(time, mag) %>%
    mutate(time=date(time)) %>%
    arrange(time, -mag) %>%
    filter (!duplicated (time))

california5_zoo = read.zoo(california5)

tsdisplay(california5_zoo, main='California last 5 years')

auto.arima(california5_zoo)

model4 = Arima(california5_zoo, order = c(5, 1, 0))

model4 %>% residuals() %>% tsdisplay(points = F, 
                                     main = paste('AIC:', round(model4$aic)))




#playing with prophet 
library(prophet)

chile_prophet2 = chile_prophet %>% rename(ds=time, y=mag)

prophet_model = prophet(chile_prophet2)

prophet_future = make_future_dataframe(prophet_model, periods = 365)

prophet_forecast = predict(prophet_model, prophet_future)

plot(prophet_model, prophet_forecast)

prophet_plot_components(prophet_model, prophet_forecast)
