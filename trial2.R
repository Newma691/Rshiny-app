library(highfrequency)

library(quantmod)

library(ggplot2)
library(dplyr)
library(forecast)
library(TTR)
library(zoo)
library(DT)




library(tm)

a = getAlphaVantageData(
   symbols = "AAPL",
   interval = "5min",
   outputType = "xts",
   apiKey = "4ba7547a6dmshaa88a9af9a0d86bp1de134jsn3e304b4aa78c",
   doSleep = TRUE
   ) 
   
Arima(a$CLOSE)

#plot(1:nrow(a$CLOSE), a$CLOSE)
ggplot(data = a$CLOSE, aes(x = 1:nrow(a$CLOSE), y =  a$CLOSE)) + geom_line() + xlab('Time Period') + ggtitle(a$CLOSE) + theme_bw()
