library(readr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lmtest)
library(ggfortify)
library(car)
library(quantmod)

#importing data from Kaggle
aapl_master_enriched <- read_csv("Data/archive (2)/aapl_master_enriched.csv")
sap500 <- read_csv("Data/archive (3)/sap500.csv")
X_NASDAQ <- read_csv("Data/_NASDAQ.csv")

#Download XLP daily
getSymbols("XLP", from = "2020-01-01", to = "2026-01-23", auto.assign = TRUE)
XLP <- XLP %>% 
  as.data.frame() %>%
  mutate(Date = as.Date(index(XLP))) %>%
  select(Date, XLP.Open) %>%
  rename(xlp_open = XLP.Open)

#Reducing data to sensible amount, Align data by date
XLP <- filter(XLP, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"))
aapl_master_enriched <- mutate(aapl_master_enriched, log_volume = log(volume))
aapl_master_enriched <- filter(aapl_master_enriched, date >= as.Date("2020-01-01") & date <= as.Date("2025-11-26"
))
aapl_master_enriched2 <- filter(aapl_master_enriched, date >= as.Date("2025-10-01") & date <= as.Date("2025-11-26"))
view(aapl_master_enriched)
sap500 <- filter(sap500, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"
))
sap5002 <- filter(sap500, Date >= as.Date("2025-10-01") & Date <= as.Date("2025-11-26"
))
nasdaq <- filter(X_NASDAQ, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"
))
nasdaq2 <- filter(X_NASDAQ, Date >= as.Date("2025-10-01") & Date <= as.Date("2025-11-26"
))


table <- tibble(date = aapl_master_enriched$date, xlp_open  = XLP$xlp_open, aapl_close = aapl_master_enriched$close, aapl_open = aapl_master_enriched$open, 
                aapl_volume =  aapl_master_enriched$log_volume, nasdaq_open = nasdaq$Open, snp500_open = sap500$Open,
                aapl_ema = aapl_master_enriched$ema_5)
table2 <- tibble(date = aapl_master_enriched2$date, aapl_close = aapl_master_enriched2$close, aapl_open = aapl_master_enriched2$open, 
                 aapl_volume =  aapl_master_enriched2$log_volume, nasdaq_open = nasdaq2$Open,
                 aapl_ema = aapl_master_enriched2$ema_5)


#HALLOOOOOO

#Building linear models and checking necessary requirements
modell <- lm(aapl_close~  aapl_volume + snp500_open + xlp_open + nasdaq_open , data=table )
modell2 <- lm(aapl_close~  aapl_open + aapl_volume + nasdaq_open, data=table2 )

#Linearität 
 p1 <- ggplot(table, aes(x = aapl_open, y =  aapl_close))+geom_point()
 p2 <- ggplot(table, aes(x= (aapl_volume), y = aapl_close))+geom_point()
 p4 <- ggplot(table, aes(x= nasdaq_open, y = aapl_close))+geom_point()
 p5 <- ggplot(table, aes(x = xlp_open , y = aapl_close)) + geom_point()
 combined <-  p1 + p2 + p4 + p5
 combined

#Multikollinearität und einflussreiche Fälle
vif(modell)
#Hohe Multikollinearität zwischen SNP 500 und Nasdaq -> Nullhypothese der Unabhängigkeit muss verworden werden - Neuer Ansatz ohne SNP 500
modell <- lm(aapl_close~  aapl_volume + xlp_open + nasdaq_open , data=table )
vif(modell)
vif(modell2)
 
#Normalverteilung der Residuen und Homoeskadizität
combined <- autoplot(modell,which = 2) + autoplot(modell,which= 1)
combined
combined2 <- autoplot(modell2, which = 2) + autoplot(modell2, which = 1)
combined2
bptest(modell)
bptest(modell2)

#Interpretation
coeftest(modell, vcoc= vcovHC(modell,type = "HC3"))
summary(modell)
summary(modell2)



 
 
 
