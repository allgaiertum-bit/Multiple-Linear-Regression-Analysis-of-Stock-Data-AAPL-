library(readr)
library(tidyverse)
library(ggplot2)
library(patchwork)


#importing data 
aapl_master_enriched <- read_csv("Data/archive (2)/aapl_master_enriched.csv")
sap500 <- read_csv("Data/archive (3)/sap500.csv")
X_NASDAQ <- read_csv("Data/_NASDAQ.csv")

#Reducing data to sensible amount, Align data by date
aapl_master_enriched <- filter(aapl_master_enriched, date >= as.Date("2020-01-01") & date <= as.Date("2025-11-26"
))
aapl_master_enriched <- mutate(aapl_master_enriched, log_volume = log(volume))
sap500 <- filter(sap500, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"
))
nasdaq <- filter(X_NASDAQ, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"
))

table <- tibble(date = aapl_master_enriched$date, aapl_close = aapl_master_enriched$close, aapl_open = aapl_master_enriched$open, 
                aapl_volume =  aapl_master_enriched$log_volume, sp500_open = sap500$Open, nasdaq_open = nasdaq$Open,
                aapl_ema = aapl_master_enriched$ema_5)
view(table)

#Vorraussetzungen 

#Linearität 
 p1 <- ggplot(table, aes(x = aapl_open, y =  aapl_close))+geom_point()
 p2 <- ggplot(table, aes(x= (aapl_volume), y = aapl_close))+geom_point()
 p3 <- ggplot(table, aes(x= sp500_open, y = aapl_close))+geom_point() 
 p4 <- ggplot(table, aes(x= nasdaq_open, y = aapl_close))+geom_point()
 p5 <- ggplot(table, aes(x = aapl_ema, y = aapl_close)) + geom_point()
 (combined <-  p1 + p2 + p3 + p4 + p5)
 
 
