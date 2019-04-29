rm(list=ls(all=T)) 
library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)

# Variable <- read_excel("~/8.semester projekt/Variable.xlsx")
data <- read_excel("Variable.xlsx")
data <- as.data.frame(data)
data$RO <- (data$WTI/(data$`CPI US`/100))
data$lRO <- log(data$RO)
data$l_diff_RO <- diff.xts(data$lRO, differences = 1)
data$OP <- log(data$`World oil prod`) - lag.xts(log(data$`World oil prod`),k = 1) 
data$OI <- (data$`OECD Pet inv`/data$`US Pet inv`)*data$`US crude inv`
data$dOI <- diff.xts(data$OI,differences = 1)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"

data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["1973-01/2018-06"]

lRO = data.xts$lRO["1973-02/2018-06"]
lRO.ts <- ts(lRO,frequency=12,start=c(1973, 2), end=c(2018, 6))

# Replikation -------------------------------------------------------------

#Arima testing
lROsub = subset(lRO.ts, end = 226)
checkresiduals(lROsub)

testlROt <- ur.df(lROsub,type="trend",lags = 12, selectlags = "AIC")
testlROd <- ur.df(lROsub,type="drift", lags = 12, selectlags = "AIC")
testlROn <- ur.df(lRO,type="none", lags = 12, selectlags = "AIC")
summary(testlROt)
summary(testlROd)
summary(testlROn)

lROsub %>% diff() %>% ggtsdisplay(main="")
autofit = auto.arima(lROsub)
summary(autofit)
checkresiduals(autofit)

#12 lag forecast
v = c(NA)
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(12,0,0))
  recursive = predict(arimaf, n.ahead=1)
  fcst=recursive$pred[1]
  v[i+1]=fcst
}

test = subset(lRO.ts[, "lRO"],start=227,end = 439)
RW = lRO.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))
arimaf.ts = ts(v[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))

#autoplot(subset(lRO.ts, end = 438)) +
autoplot(test, series = "Real Price") +
  autolayer(arimaf.ts, series="arima") +
  #autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()


#Forecast Accuracy
ac.rwf = accuracy(RWf.ts, test)
ac.arf =accuracy(arimaf.ts, test)
PE_AR=arimaf.ts-test
MSPE_AR = mean((PE_AR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_AR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

# 1 lag (AIC) forecast
v = c(NA)
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(1,0,0))
  recursive = predict(arimaf, n.ahead=1)
  fcst=recursive$pred[1]
  v[i+1]=fcst
}

test = subset(lRO.ts[, "lRO"],start=227,end = 439)
RW = lRO.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))
arimaf.ts = ts(v[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))

#Forecast Accuracy
ac.rwf = accuracy(RWf.ts, test)
ac.arf =accuracy(arimaf.ts, test)
PE_AR=arimaf.ts-test
MSPE_AR = mean((PE_AR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_AR/MSPE_RW #fraction of random walk - should be <1 to beat the RW


# ARMA(1,1) forecast
v = c(NA)
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(1,0,1))
  recursive = predict(arimaf, n.ahead=1)
  fcst=recursive$pred[1]
  v[i+1]=fcst
}

test = subset(lRO.ts[, "lRO"],start=227,end = 439)
RW = lRO.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))
arimaf.ts = ts(v[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))

#Forecast Accuracy
ac.rwf = accuracy(RWf.ts, test)
ac.arf =accuracy(arimaf.ts, test)
PE_AR=arimaf.ts-test
MSPE_AR = mean((PE_AR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_AR/MSPE_RW #fraction of random walk - should be <1 to beat the RW




