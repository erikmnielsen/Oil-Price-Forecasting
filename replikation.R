rm(list=ls(all=T)) 
library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)
#library(dplyr)
#library(timeSeries)
library(aTSA)
#library(rugarch)
#library(fBasics)
#library(fGarch)
#library(zoo)
#library(reshape2)
#library(mvmeta)
#library(tidyr)
#library(ggplot2)
#library(gridExtra)
#library(dynlm)
#library(tsDyn)
#library(tseries)
#library(MTS)
#library(portes)


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

plot.xts(data.xts$`World oil prod`)

par(mfrow=c(1,1))
plot(data.xts$WTI)
plot(data.xts$RO)
plot(data.xts$lRO)

# Replication h=1

data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

v = c(NA)
for (i in 0:213) {
  train = VAR.ts[1:(226+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=1)
  fcst=recursive$fcst$lRO[,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=227,end = 439)
RW = VAR.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))
VARf.ts = ts(v[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))

autoplot(subset(VAR.ts[, "lRO"], end = 438)) +
  #autoplot(test, series = "Real Price") +
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()


#FORECAST ACCURACY
ac.rwf = accuracy(RWf.ts, test)
ac.varf =accuracy(VARf.ts, test)


PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW

plot(ts(PE_VAR^2))
lines(PE_RW^2, col = "red")

# Replication h=3

data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

v = c(NA)
for (i in 0:213) {
  train = VAR.ts[1:(226+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=3)
  fcst=recursive$fcst$lRO[3,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=229,end = 439)
RW = VAR.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:211], frequency=12, start=c(1992, 2), end=c(2009, 8))
VARf.ts = ts(v[1:211], frequency=12, start=c(1992, 2), end=c(2009, 8))

test
RWf.ts

autoplot(subset(VAR.ts[, "lRO"], end = 438)) +
  #autoplot(test, series = "Real Price") +
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()


#FORECAST ACCURACY
ac.rwf = accuracy(RWf.ts, test)
ac.varf =accuracy(VARf.ts, test)


PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW

plot(ts(PE_VAR^2))
lines(PE_RW^2, col = "red")

# Replication h=6
data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

v = c(NA)
for (i in 0:213) {
  train = VAR.ts[1:(226+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=6)
  fcst=recursive$fcst$lRO[6,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=232,end = 439)
RW = VAR.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:208], frequency=12, start=c(1992, 5), end=c(2009, 8))
VARf.ts = ts(v[1:208], frequency=12, start=c(1992, 5), end=c(2009, 8))

test
RWf.ts

autoplot(subset(VAR.ts[, "lRO"], end = 438)) +
  #autoplot(test, series = "Real Price") +
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()


#FORECAST ACCURACY
ac.rwf = accuracy(RWf.ts, test)
ac.varf =accuracy(VARf.ts, test)


PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW

plot(ts(PE_VAR^2))
lines(PE_RW^2, col = "red")

# Replication h=9
data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

v = c(NA)
for (i in 0:213) {
  train = VAR.ts[1:(226+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=9)
  fcst=recursive$fcst$lRO[9,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=235,end = 439)
RW = VAR.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:205], frequency=12, start=c(1992, 8), end=c(2009, 8))
VARf.ts = ts(v[1:205], frequency=12, start=c(1992, 8), end=c(2009, 8))

test
RWf.ts

autoplot(subset(VAR.ts[, "lRO"], end = 438)) +
  #autoplot(test, series = "Real Price") +
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()


#FORECAST ACCURACY
ac.rwf = accuracy(RWf.ts, test)
ac.varf =accuracy(VARf.ts, test)


PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW

plot(ts(PE_VAR^2))
lines(PE_RW^2, col = "red")




# Replication h=12
data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

v = c(NA)
for (i in 0:213) {
  train = VAR.ts[1:(226+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=12)
  fcst=recursive$fcst$lRO[12,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=238,end = 439)
RW = VAR.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:202], frequency=12, start=c(1992, 11), end=c(2009, 8))
VARf.ts = ts(v[1:202], frequency=12, start=c(1992, 11), end=c(2009, 8))

test
RWf.ts

autoplot(subset(VAR.ts[, "lRO"], end = 438)) +
  #autoplot(test, series = "Real Price") +
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()


#FORECAST ACCURACY
ac.rwf = accuracy(RWf.ts, test)
ac.varf =accuracy(VARf.ts, test)


PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW

plot(ts(PE_VAR^2))
lines(PE_RW^2, col = "red")