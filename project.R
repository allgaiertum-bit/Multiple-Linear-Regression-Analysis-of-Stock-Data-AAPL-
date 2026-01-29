library(readr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lmtest)
library(ggfortify)
library(jtools)
library(car)
library(quantmod)

#Importieren der Datensätze von Kaggle
aapl_master_enriched <- read_csv("Data/archive (2)/aapl_master_enriched.csv")
X_NASDAQ <- read_csv("Data/_NASDAQ.csv")

#Herunterladen der Daten des XLP von Yahoo Finance über Quantmod
getSymbols("XLP", from = "2020-01-01", to = "2025-11-27", auto.assign = TRUE)
XLP <- XLP %>% 
  as.data.frame() %>%
  mutate(Date = as.Date(index(XLP))) %>%
  select(Date, XLP.Open) %>%
  rename(xlp_open = XLP.Open)
VIX <- getSymbols("^VIX", src = "yahoo", auto.assign = FALSE)
VIX <- VIX %>%
  as.data.frame() %>%
  mutate(Date = as.Date(index(VIX))) %>%
  select(Date, VIX.Close) %>%
  rename(vix_close = VIX.Close)

#Reduzierung und Normung des Zeitraums, Aneinanderreihung der genutzten Daten in gemeinsamer Tabelle
VIX <- filter(VIX, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"))
aapl_master_enriched <- mutate(aapl_master_enriched, log_volume = log(volume))
aapl_master_enriched <- filter(aapl_master_enriched, date >= as.Date("2020-01-01") & date <= as.Date("2025-11-26"))
nasdaq <- filter(X_NASDAQ, Date >= as.Date("2020-01-01") & Date <= as.Date("2025-11-26"))

table <- tibble(date = aapl_master_enriched$date, aapl_close = aapl_master_enriched$close,
                aapl_volume =  aapl_master_enriched$log_volume, xlp_open  = XLP$xlp_open, nasdaq_open = nasdaq$Open,
                vix_close = VIX$vix_close)


#Test der Vorraussetzungen

#Linearität 
p1 <- ggplot(table, aes(x= aapl_volume, y = aapl_close))+geom_point()
p2 <- ggplot(table, aes(x= nasdaq_open, y = aapl_close))+geom_point()
p3 <- ggplot(table, aes(x = xlp_open , y = aapl_close)) + geom_point()
p4 <- ggplot(table, aes(x = log(vix_close), y = aapl_close))+geom_point()
combined <-  p1 + p2 + p3 + p4
combined

#Bau des linearen Modells

modell <- lm(aapl_close~  aapl_volume + xlp_open + nasdaq_open , data=table )

#Normalverteilung der Residuen und Homoeskadizität
hist(residuals(modell), col = "steelblue") 
plot(modell,which= 1)
bptest(modell)
#Da der p-Wert weit unter dem Signifikanzniveau von 0.05 liegt muss die Nullhypothese der Homoeskadizität verworfen werden
#Testen der Nullhypothese mit kleinerem Datensatz
table2 <- filter(table, date >= as.Date("2025-05-26") & date <= as.Date("2025-11-26"))
view(table2)
modell2 <- lm(aapl_close~  aapl_volume + xlp_open +  nasdaq_open, data=table2 ) 
bptest(modell2)


#Multikollinearität
vif(modell2)

#Interpretation - t Test mit Nullhypothese: Der Koeffizient hat keinen signifikanten Einfluss auf die abhängige Variable
summary(modell2)
summary(modell)
effect_plot(modell2, pred = nasdaq_open, interval = TRUE, plot.points = TRUE)



 
 
 
