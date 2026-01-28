library(readr)
library(tidyverse)
library(ggplot2)

#importing data 
aapl_master_enriched <- read_csv("archive (2)/aapl_master_enriched.csv")
sap500 <- read_csv("archive (3)/sap500.csv")
view(aapl_master_enriched)
view(sap500)

#Reducing data to sensible amount, Align data by date
aapl_master_enriched <- filter(aapl_master_enriched, date >= as.Date("2020-01-01") & date <= as.Date("2026-01-23"
))
aapl_master_enriched2 <- filter(aapl_master_enriched, date >= as.Date("2026-01-01") & date <= as.Date("2026-01-23"
))
sap500 <- filter(sap500, Date >= as.Date("2020-01-01") & Date <= as.Date("2026-01-23"
))
table <- tibble(date = aapl_master_enriched$date, aapl_close = aapl_master_enriched$close, aapl_open = aapl_master_enriched$open, aapl_volume =  aapl_master_enriched$volume, sp500_open = sap500$Open, sp500_volume = sap500$Volume)
view(table)

#Vorraussetzungen 

#Linearität 
 ggplot(aapl_master_enriched, aes(x = return_1d, y = close))+geom_point()
 ggplot(table, aes(x = aapl_open, y =  aapl_close))+geom_point()
 ggplot(table, aes(x= log(aapl_volume), y = aapl_close))+geom_point()
 ggplot(table, aes(x= sp500_open, y = aapl_close))+geom_point() 
 ggplot(table, aes(x= log(sp500_volume), y = aapl_close))+geom_point() 