library(readr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lmtest)
library(ggfortify)
library(car)
library(quantmod)

#Importieren der Datensätze von Kaggle
aapl_master_enriched <- read_csv("Data/archive (2)/aapl_master_enriched.csv")
sap500 <- read_csv("Data/archive (3)/sap500.csv")
X_NASDAQ <- read_csv("Data/_NASDAQ.csv")

#Herunterladen von XLP täglich von Quantmod
getSymbols("XLP", from = "2020-01-01", to = "2026-01-23", auto.assign = TRUE)
XLP <- XLP %>% 
  as.data.frame() %>%
  mutate(Date = as.Date(index(XLP))) %>%
  select(Date, XLP.Open) %>%
  rename(xlp_open = XLP.Open)

#Reduzierung und Normung des Zeitraums, Aneinanderreihung der genutzten Daten in gemeinsamer Tabelle
XLP <- filter(XLP, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"))
aapl_master_enriched <- mutate(aapl_master_enriched, log_volume = log(volume))
aapl_master_enriched <- filter(aapl_master_enriched, date >= as.Date("2020-01-01") & date <= as.Date("2025-11-26"))
sap500 <- filter(sap500, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"))
nasdaq <- filter(X_NASDAQ, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"))

table <- tibble(date = aapl_master_enriched$date, aapl_close = aapl_master_enriched$close,
                aapl_volume =  aapl_master_enriched$log_volume, xlp_open  = XLP$xlp_open, nasdaq_open = nasdaq$Open, snp500_open = sap500$Open)

#Erstellung der selben Tabelle mit kleinerem Zeitrahmen um Einfluss von Datenmenge zu illustrieren
XLP2 <- filter(XLP, Date >= as.Date("2025-10-01") & Date <= as.Date("2025-11-26"))
aapl_master_enriched2 <- filter(aapl_master_enriched, date >= as.Date("2025-10-01") & date <= as.Date("2025-11-26"))
sap5002 <- filter(sap500, Date >= as.Date("2025-10-01") & Date <= as.Date("2025-11-26"))
nasdaq2 <- filter(X_NASDAQ, Date >= as.Date("2025-10-01") & Date <= as.Date("2025-11-26"))
table2 <- tibble(date = aapl_master_enriched2$date, aapl_close = aapl_master_enriched2$close, 
                 aapl_volume =  aapl_master_enriched2$log_volume, xlp_open  = XLP2$xlp_open, nasdaq_open = nasdaq2$Open,
                 snp500_open = sap5002$Open)

#Bau der linearen Modelle
modell <- lm(aapl_close~  aapl_volume + snp500_open + xlp_open + nasdaq_open , data=table )
modell2 <- lm(aapl_close~  aapl_volume + snp500_open + xlp_open +  nasdaq_open, data=table2 )

#Vorraussetzungen

#Linearität 
 p1 <- ggplot(table, aes(x= (aapl_volume), y = aapl_close))+geom_point()
 p2 <- ggplot(table, aes(x= nasdaq_open, y = aapl_close))+geom_point()
 p3 <- ggplot(table, aes(x = xlp_open , y = aapl_close)) + geom_point()
 p4 <- ggplot(table, aes(x = snp500_open , y = aapl_close)) + geom_point()
 combined <-  p1 + p2 + p3 + p4
 combined

#Multikollinearität und einflussreiche Fälle
vif(modell)
#Hohe Multikollinearität zwischen SNP 500 und Nasdaq -> Nullhypothese der Unabhängigkeit muss verworden werden - Neuer Ansatz ohne SNP 500
modell <- lm(aapl_close~  aapl_volume + xlp_open + nasdaq_open , data=table )
vif(modell)
modell2 <- lm(aapl_close~  aapl_volume + xlp_open + nasdaq_open , data=table2 )
vif(modell2)

#Normalverteilung der Residuen und Homoeskadizität
combined <- autoplot(modell,which = 2) + autoplot(modell,which= 1)
combined
combined2 <- autoplot(modell2, which = 2) + autoplot(modell2, which = 1)
combined2
bptest(modell)
bptest(modell2)

#Interpretation - Nullhypothese: Der Koeffizient hat keinen signifikanten Einfluss auf die abhängige Variable
coeftest(modell, vcoc= vcovHC(modell,type = "HC3"))
summary(modell)
summary(modell2)



 
 
 
