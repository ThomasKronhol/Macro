# First assignment, Macroeconometrics 
#update.packages()
#Collecting data from Fred, using their API
#install.packages("tinytex")
#install.packages("ggplot2", "dplyr")
library(fredr)
library(ggplot2)
library(dplyr)
library(purrr)
library(vars)
library(xts)
library(gridExtra)
library(tseries)
library(tidyverse)
library(palmerpenguins)
library(quarto)
library(tinytex)
#Setting up API key from FRED
fredr_set_key("54706a95d44824ac499f1012d9b3a401")

start_date <- as.Date("1990-01-01")
end_date <- as.Date("2022-12-01")

# Define a vector of series IDs
#timeseries <- c("INDPRO", "CPIAUCSL", "UMCSENT", "TOTCI", "USSTHPI", "NFCI") incl. house prices
timeseries <- c("INDPRO", "CPIAUCSL", "UMCSENT", "TOTCI", "NFCI")
data <- list()

# Retrieve the data for the series and date range ::::: change m to q for quarterly
for (name in timeseries) {
  fred_data <- fredr(series_id = name, observation_start = start_date, observation_end = end_date, frequency = "m")
  data[[name]] <- fred_data
}

#Deleting columns not required 
indp <- data[["INDPRO"]]
indp$ln_value <- log(indp$value)
indp_ <- indp[, c("date","ln_value")]
indp_$date <- as.Date(indp_$date, format = "%d-%m-%Y")
tindp <- xts(indp_$ln_value, order.by = indp_$date)

cpi <- data[["CPIAUCSL"]]
cpi$ln_value <- log(cpi$value)
cpi_ <- cpi[, c("date","ln_value")] 
tcpi <- xts(cpi_$ln_value, order.by = indp_$date)

expec <- data[["UMCSENT"]]
expec_ <- expec[, c("date","value")]
texpec <- xts(expec_$value, order.by = indp_$date)

lend <- data[["TOTCI"]]
lend$ln_value <- log(lend$value)
lend_ <- lend[, c("date","ln_value")]
tlend <- xts(lend_$ln_value, order.by = indp_$date)

#hp <- data[["USSTHPI"]]
#hp$ln_value <- log(hp$value)
#hp_ <- hp[, c("date","ln_value")]
#thp <- xts(hp_$ln_value, order.by = indp_$date)

fci <- data[["NFCI"]]
fci_ <- fci[, c("date","value")]
tfci <- xts(fci_$value, order.by = indp_$date)

#Merging the series into vector Y
#Y = na.omit(merge(tindp, tcpi, texpec, tlend, thp, tfci))
#colnames(Y)<- c("indu", "cpi", "exp", "lend","hp","fci")
Y = na.omit(merge(tindp, tcpi, texpec, tlend, tfci))
colnames(Y)<- c("indu", "cpi", "exp", "lend","fci")
head(Y)

#Plotting the series in levels
ip = ggplot(data = Y[,1], aes(x = index(Y[,1]), y = Y[,1])) +
  geom_line(color = "black") +
  labs(title = "Industrial production", x = "Year", y = "Ln(Indp)") +
  theme_minimal()

inf = ggplot(data = Y[,2], aes(x = index(Y[,2]), y = Y[,2])) +
  geom_line(color = "black") +
  labs(title = "Consumer price index", x = "Year", y = "Ln(CPI)") +
  theme_minimal()

ex = ggplot(data = Y[,3], aes(x = index(Y[,3]), y = Y[,3])) +
  geom_line(color = "black") +
  labs(title = "Consumer Expectations", x = "Year", y = "Index") +
  theme_minimal()
    
le = ggplot(data = Y[,4], aes(x = index(Y[,4]), y = Y[,4])) +
  geom_line(color = "black") +
  labs(title = "Lending", x = "Year", y = "Ln(Lending)") +
  theme_minimal()

#hou = ggplot(data = Y[,5], aes(x = index(Y[,5]), y = Y[,5])) +
#  geom_line(color = "black") +
#  labs(title = "House price index", x = "Year", y = "Ln(HP)") +
#  theme_minimal()

fincon = ggplot(data = Y[,5], aes(x = index(Y[,5]), y = Y[,5])) +
  geom_line(color = "black") +
  labs(title = "National Financial Condition index", x = "Year", y = "Index") +
  theme_minimal()

#grid.arrange(ip, inf, ex, le, hou, fincon, nrow = 3, ncol = 2)
grid.arrange(ip, inf, ex, le, fincon, nrow = 3, ncol = 2)

#Doing an ACF test :::::::: change to 6 if HP incl.
adf_ <- list()
for (i in 1:5) {
  adf_result = adf.test(Y[,i])
  adf_[[i]] <- adf_result
}
head(adf_)

# Testing for cointegration rank
vecm_Y = ca.jo(Y, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary(vecm_Y) 

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#first differences of the variables 
dY <- diff(Y, differences = 1)
dY <- dY[13:nrow(dY),]
colnames(dY)<- c("dindu", "dcpi", "dexp", "dlend","dfci")
#colnames(dY)<- c("dindu", "dcpi", "dexp", "dlend","dhp","dfci")
adf__ <- list()
for (i in 1:5) {
  adf__result = adf.test(dY[,i])
  adf__[[i]] <- adf__result
}
head(adf__)

vecm_dY = ca.jo(dY, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary(vecm_dY) 

#Plotting the series in FD
dip = ggplot(data = dY[,1], aes(x = index(dY[,1]), y = dY[,1])) +
  geom_line(color = "black") +
  labs(title = "Industrial production", x = "Year", y = "pct.") +
  theme_minimal()

dinf = ggplot(data = dY[,2], aes(x = index(dY[,2]), y = dY[,2])) +
  geom_line(color = "black") +
  labs(title = "Consumer price index", x = "Year", y = "pct.") +
  theme_minimal()

dex = ggplot(data = dY[,3], aes(x = index(dY[,3]), y = dY[,3])) +
  geom_line(color = "black") +
  labs(title = "Consumer Expectations", x = "Year", y = "pct. points") +
  theme_minimal()

dle = ggplot(data = dY[,4], aes(x = index(dY[,4]), y = dY[,4])) +
  geom_line(color = "black") +
  labs(title = "Lending", x = "Year", y = "pct.") +
  theme_minimal()

#dhou = ggplot(data = dY[,5], aes(x = index(dY[,5]), y = dY[,5])) +
#  geom_line(color = "black") +
#  labs(title = "House price index", x = "Year", y = "pct.") +
#  theme_minimal()

dfincon = ggplot(data = dY[,5], aes(x = index(dY[,5]), y = dY[,5])) +
  geom_line(color = "black") +
  labs(title = "National Financial Condition index", x = "Year", y = "pct. point") +
  theme_minimal()

grid.arrange(dip, dinf, dex, dle, dfincon, nrow = 3, ncol = 2)
#grid.arrange(dip, dinf, dex, dle, dhou, dfincon, nrow = 3, ncol = 2)
