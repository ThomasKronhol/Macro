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

start_date <- as.Date("1987-01-01")
end_date <- as.Date("2023-01-01")

# Define a vector of series IDs
timeseries <- c("INDPRO", "CPIAUCSL", "UMCSENT", "TOTCI", "CSUSHPISA", "NFCI") #incl. house prices
#timeseries <- c("INDPRO", "CPIAUCSL", "UMCSENT", "TOTCI", "NFCI")
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

hp <- data[["CSUSHPISA"]]
hp$ln_value <- log(hp$value)
hp_ <- hp[, c("date","ln_value")]
thp <- xts(hp_$ln_value, order.by = indp_$date)

fci <- data[["NFCI"]]
fci_ <- fci[, c("date","value")]
tfci <- xts(fci_$value, order.by = indp_$date)

#Merging the series into vector Y
y = na.omit(merge(tindp, tcpi, texpec, tlend, thp, tfci))
colnames(y)<- c("indu", "cpi", "exp", "lend","hp","fci")
#Y = na.omit(merge(tindp, tcpi, texpec, tlend, tfci))
#colnames(Y)<- c("indu", "cpi", "exp", "lend","fci")
head(y)

y = as.ts(y)
#Plotting the series in levels
# ip = ggplot(data = y[,1], aes(x = index(y[,1]), y = y[,1])) +
#   geom_line(color = "black") +
#   labs(title = "Industrial production", x = "Year", y = "Ln(Indp)") +
#   theme_minimal()
# 
# inf = ggplot(data = Y[,2], aes(x = index(Y[,2]), y = Y[,2])) +
#   geom_line(color = "black") +
#   labs(title = "Consumer price index", x = "Year", y = "Ln(CPI)") +
#   theme_minimal()
# 
# ex = ggplot(data = Y[,3], aes(x = index(Y[,3]), y = Y[,3])) +
#   geom_line(color = "black") +
#   labs(title = "Consumer Expectations", x = "Year", y = "Index") +
#   theme_minimal()
#     
# le = ggplot(data = Y[,4], aes(x = index(Y[,4]), y = Y[,4])) +
#   geom_line(color = "black") +
#   labs(title = "Lending", x = "Year", y = "Ln(Lending)") +
#   theme_minimal()
# 
# hou = ggplot(data = Y[,5], aes(x = index(Y[,5]), y = Y[,5])) +
#   geom_line(color = "black") +
#   labs(title = "House price index", x = "Year", y = "Ln(HP)") +
#   theme_minimal()
# 
# fincon = ggplot(data = Y[,6], aes(x = index(Y[,6]), y = Y[,6])) +
#   geom_line(color = "black") +
#   labs(title = "National Financial Condition index", x = "Year", y = "Index") +
#   theme_minimal()
# 
# #grid.arrange(ip, inf, ex, le, hou, fincon, nrow = 3, ncol = 2)
# grid.arrange(ip, inf, ex, le, hou, fincon, nrow = 3, ncol = 2)
# 
# # Plotting Autocorrelation functions
# #par(mfrow=c(3,2))
# #a_ip = acf(Y[,1], lag.max = 20, main = "ACF Plot, Industrial production", ylab = "Autocorrelation", type = "correlation")
# #a_cpi = acf(Y[,2], lag.max = 20, main = "ACF Plot, Consumer price index", ylab = "Autocorrelation", type = "correlation")
# #a_exp = acf(Y[,3], lag.max = 20, main = "ACF Plot, Consumer expectations", ylab = "Autocorrelation", type = "correlation")
# #a_le = acf(Y[,4], lag.max = 20, main = "ACF Plot, Lending", ylab = "Autocorrelation", type = "correlation")
# #a_hou = acf(Y[,6], lag.max = 20, main = "ACF Plot, House price index", ylab = "Autocorrelation", type = "correlation")
# #a_fci = acf(Y[,5], lag.max = 20, main = "ACF Plot, NFCI", ylab = "Autocorrelation", type = "correlation")
# 
# # Plotting Partial Autocorrelation functions
# #par(mfrow=c(3,2))
# #a_ip = pacf(Y[,1], lag.max = 5, main = "PACF Plot, Industrial production", ylab = "Partial Autocorrelation")
# #a_cpi = pacf(Y[,2], lag.max = 20, main = "PACF Plot, Consumer price index", ylab = "Partial Autocorrelation")
# #a_exp = pacf(Y[,3], lag.max = 20, main = "PACF Plot, Consumer expectations", ylab = "Partial Autocorrelation")
# #a_le = pacf(Y[,4], lag.max = 20, main = "PACF Plot, Lending", ylab = "Partial Autocorrelation")
# #a_hou = pacf(Y[,5], lag.max = 20, main = "PACF Plot, House price index", ylab = "Partial Autocorrelation")
# #a_fci = pacf(Y[,6], lag.max = 20, main = "PACF Plot, NFCI", ylab = "Partial Autocorrelation")
# 
# 
# #Doing an ADF test :::::::: change to 6 if HP incl.
# max_lag = 12
# adf_ <- list()
# for (i in 1:6) {
#   adf_result = adf.test(Y[,i], k = max_lag)
#   adf_[[i]] <- adf_result
# }
# head(adf_)
# 
# # View the ADF test results
# summary(adf_result)
# 
# adf_table <- data.frame(Test_Statistic = numeric(length(adf_)), 
#                         p_value = numeric(length(adf_)), 
#                         Lags_Used = numeric(length(adf_)))
# 
# # Fill in the data frame with the test results
# for (i in 1:length(adf_)) {
#   adf_table[i, "Test_Statistic"] = round(adf_[[i]]$statistic,3)
#   adf_table[i, "p_value"] = round(adf_[[i]]$p.value,3)
#   adf_table[i, "Lags_Used"] = round(adf_[[i]]$parameter,3)
# }
# 
# # Print the data frame
# rownames(adf_table)<- c("Industrial Production", "Consumer Price Index", "Consumer Expectation", "Lending","House prices","NFCI")
# colnames(adf_table)<- c("Test statistic", "P-value", "Lags")
# print(adf_table)
# 
# # Testing for cointegration rank
# vecm_Y = ca.jo(Y, type = "trace", ecdet = "const", K = 12, spec = "transitory")
# summary(vecm_Y) 
# 

# setup for estimation MLE
############################################################
N       = 6
p       = 12
S       = 50000
h       = 8

# create Y and X
############################################################
Y       = ts(y[13:433,], start=c(1987,1), frequency=12)
X       = matrix(1,nrow(Y),1)
for (i in 1:p){
  X     = cbind(X,y[13:433-i,])
}

t0          = proc.time() # read processor time

# MLE
############################################################
A.hat       = solve(t(X)%*%X)%*%t(X)%*%Y
Sigma.hat   = t(Y-X%*%A.hat)%*%(Y-X%*%A.hat)/nrow(Y)
# round(A.hat,3)
# round(Sigma.hat,3)
# round(cov2cor(Sigma.hat),3)

# prior distribution
############################################################
kappa.1     = 1^2
kappa.2     = 100
kappa.3     = 1
A.prior     = matrix(0,nrow(A.hat),ncol(A.hat))
A.prior[2:7,] = kappa.3*diag(N)
V.prior     = diag(c(kappa.2,kappa.1*((1:p)^(-2))%x%rep(1,N)))
S.prior     = diag(diag(Sigma.hat))
nu.prior    = N+1

# normal-inverse Wishard posterior parameters
############################################################
V.bar.inv   = t(X)%*%X + diag(1/diag(V.prior))
V.bar       = solve(V.bar.inv)
A.bar       = V.bar%*%(t(X)%*%Y + diag(1/diag(V.prior))%*%A.prior)
nu.bar      = nrow(Y) + nu.prior
S.bar       = S.prior + t(Y)%*%Y + t(A.prior)%*%diag(1/diag(V.prior))%*%A.prior - t(A.bar)%*%V.bar.inv%*%A.bar
S.bar.inv   = solve(S.bar)

# posterior draws 
############################################################
Sigma.posterior   = rWishart(S, df=nu.bar, Sigma=S.bar.inv)
Sigma.posterior   = apply(Sigma.posterior,3,solve)
Sigma.posterior   = array(Sigma.posterior,c(N,N,S))
A.posterior       = array(rnorm(prod(c(dim(A.bar),S))),c(dim(A.bar),S))
B.posterior       = array(NA,c(N,N,S))
L                 = t(chol(V.bar))
for (s in 1:S){
  cholSigma.s     = chol(Sigma.posterior[,,s])
  B.posterior[,,s]= t(cholSigma.s)
  A.posterior[,,s]= A.bar + L%*%A.posterior[,,s]%*%cholSigma.s
}

round(apply(A.posterior,1:2,mean),4)
round(apply(B.posterior,1:2,mean),4)

## Generating artificial data



