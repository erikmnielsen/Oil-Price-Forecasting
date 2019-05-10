rm(list=ls(all=T)) 
library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)
library(aTSA)

# Variable <- read_excel("~/8.semester projekt/VariableHam.xlsx")
data <- read_excel("VariableHam.xlsx")
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

#Hamilton kontra Kilian
plot.xts(data.xts$`World REA Index`, col = "red")
lines(data.xts$Hamilton, col = "blue")
lines(data.xts$Kilian, col="yellow")

#Variable
WTI = data.xts$WTI
WOP = na.omit(data.xts$OP)
PetInv = data.xts$OI
dPetInv = na.omit(data.xts$dOI)
REA = data.xts$`World REA Index`
Ham = data.xts$Hamilton
RO = data.xts$RO
lRO = data.xts$lRO

#Augmented Dickey Fuller test

#WTI
testt <- ur.df(WTI,type="trend",lags = 12)
testd <- ur.df(WTI,type="drift", lags = 12)
testn <- ur.df(WTI,type="none", lags = 12)
summary(testt)
summary(testd)
summary(testn)

#World oil production
testWOPt <- ur.df(WOP,type="trend",lags = 12)
testWOPd <- ur.df(WOP,type="drift", lags = 12)
testWOPn <- ur.df(WOP, type="none", lags = 12)
summary(testWOPt)

#RO
testROt <- ur.df(RO,type="trend",lags = 6)
testROd <- ur.df(RO,type="drift", lags = 12)
testROn <- ur.df(RO,type="none", lags = 12)
summary(testROt)
summary(testROd)
summary(testROn)

testlROt <- ur.df(lRO,type="trend",lags = 1)
testlROd <- ur.df(lRO,type="drift", lags = 1)
testlROn <- ur.df(lRO,type="none", lags = 1)
summary(testlROt)
summary(testlROd) #ved et enkelt lag er denne stationær
summary(testlROn)

#Oil inventory
testOIt <- ur.df(PetInv,type="trend",lags = 30, selectlags = "AIC")
testOId <- ur.df(PetInv,type="drift", lags = 12)
testOIn <- ur.df(PetInv,type="none", lags = 30, selectlags = "AIC")
summary(testOIt)
summary(testOId)
summary(testOIn)

testdOIt <- ur.df(dPetInv,type="trend",lags = 24, selectlags = "AIC")
testdOId <- ur.df(dPetInv,type="drift", lags = 30, selectlags = "AIC")
testdOIn <- ur.df(dPetInv,type="none", lags = 30, selectlags = "AIC")

summary(testdOIt)
summary(testdOId)
summary(testdOIn)

#Hamilton variable
testHamt <- ur.df(Ham,type="trend",lags = 24, selectlags = "AIC")
testHamd <- ur.df(Ham,type="drift", lags = 24, selectlags = "AIC")
testHamn <- ur.df(Ham,type="none", lags = 24, selectlags = "AIC")

summary(testHamt)
summary(testHamd) #stationær
summary(testHamn)

# VAR FORECASTS ALL 4 VARIABLES
data <- read_excel("VariableHam.xlsx")
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

# Forecast lRO ------------------------------------------------------------
data.VAR <- data.xts[-1,c("Hamilton","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))
v=c(NA)

# h=1, MSPE=0.9681 -------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=1)
  fcst=recursive$fcst$lRO[,"fcst"] #OBS
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=365,end = 545)
RW = VAR.ts[-c(1:363), "lRO"]
RWf.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
VARf.ts = ts(v[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))

#autoplot(subset(VAR.ts[, "lRO"], end = 545)) +
autoplot(test) + #evt: series = "Real Price"
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Forecasts:")) +
  theme_economist() +
  scale_color_economist()


#Forecast Accuracy
ac.rwf = accuracy(RWf.ts, test)
ac.varf =accuracy(VARf.ts, test)

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

plot(ts(PE_VAR^2))
lines(ts(PE_RW^2), col = "red")

# h=3, MSPE=1.0495 ---------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=3)
  fcst=recursive$fcst$lRO[3,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=367,end = 545)
RW = VAR.ts[-c(1:363), "lRO"]
RWf.ts = ts(RW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))
VARf.ts = ts(v[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

# h=6, MSPE=1.1389 ---------------------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=6)
  fcst=recursive$fcst$lRO[6,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=369,end = 545)
RW = VAR.ts[-c(1:363), "lRO"]
RWf.ts = ts(RW[1:177], frequency=12, start=c(2003, 10), end=c(2018, 6))
VARf.ts = ts(v[1:177], frequency=12, start=c(2003, 10), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

# h=9, MSPE=1.1201 ---------------------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=9)
  fcst=recursive$fcst$lRO[9,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=371,end = 545)
RW = VAR.ts[-c(1:363), "lRO"]
RWf.ts = ts(RW[1:175], frequency=12, start=c(2003, 12), end=c(2018, 6))
VARf.ts = ts(v[1:175], frequency=12, start=c(2003, 12), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

# h=12, MSPE=1.1716 ---------------------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=12)
  fcst=recursive$fcst$lRO[12,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=373,end = 545)
RW = VAR.ts[-c(1:363), "lRO"]
RWf.ts = ts(RW[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))
VARf.ts = ts(v[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW


# Forecast l_diff_RO ------------------------------------------------------
data.VAR <- data.xts[-1,c("Hamilton","dOI","l_diff_RO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))
v=c(NA)

# h=1, MSPE=0.6747 -------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=1)
  fcst=recursive$fcst$l_diff_RO[,"fcst"] #OBS
  v[i+1]=fcst
}

test = subset(VAR.ts[, "l_diff_RO"],start=365,end = 545)
RW = VAR.ts[-c(1:363), "l_diff_RO"]
RWf.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
VARf.ts = ts(v[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW


# h=3, MSPE=0.5201 ---------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=3)
  fcst=recursive$fcst$l_diff_RO[3,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "l_diff_RO"],start=367,end = 545)
RW = VAR.ts[-c(1:363), "l_diff_RO"]
RWf.ts = ts(RW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))
VARf.ts = ts(v[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

# h=6, MSPE=0.5271 ---------------------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=6)
  fcst=recursive$fcst$l_diff_RO[6,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "l_diff_RO"],start=369,end = 545)
RW = VAR.ts[-c(1:363), "l_diff_RO"]
RWf.ts = ts(RW[1:177], frequency=12, start=c(2003, 10), end=c(2018, 6))
VARf.ts = ts(v[1:177], frequency=12, start=c(2003, 10), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

# h=9, MSPE=0.5108 ---------------------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=9)
  fcst=recursive$fcst$l_diff_RO[9,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "l_diff_RO"],start=371,end = 545)
RW = VAR.ts[-c(1:363), "l_diff_RO"]
RWf.ts = ts(RW[1:175], frequency=12, start=c(2003, 12), end=c(2018, 6))
VARf.ts = ts(v[1:175], frequency=12, start=c(2003, 12), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW  

# h=12 MSPE=0.4825 ---------------------------------------------------------------------
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=12)
  fcst=recursive$fcst$l_diff_RO[12,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "l_diff_RO"],start=373,end = 545)
RW = VAR.ts[-c(1:363), "l_diff_RO"]
RWf.ts = ts(RW[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))
VARf.ts = ts(v[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))

PE_VAR=VARf.ts-test
MSPE_VAR = mean((PE_VAR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_VAR/MSPE_RW #fraction of random walk - should be <1 to beat the RW

