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

# test --------------------------------------------------------------------

WTI = data.xts$WTI
d_WTI<-diff(WTI)

a <- ggAcf(WTI)
b <- ggPacf(WTI)
c <- ggAcf(d_WTI)
d <- ggPacf(d_WTI)
grid.arrange(a,b,c,d,nrow=2)

par(mfrow=c(2,1))
plot(WTI)
plot(d_WTI)

fit.lin <- lm(WTI ~ 1)
checkresiduals(fit.lin)

fit.lin2 <- lm(d_WTI ~ 1)
checkresiduals(fit.lin2)
plot(ts(fit.lin2$residuals^2))

testt <- ur.df(WTI,type="trend",lags = 12)
testd <- ur.df(WTI,type="drift", lags = 12)
testn <- ur.df(WTI,type="none", lags = 12)
summary(testt)

Box.test(testn@res,lag=6, type="Lj")

summary(ur.df(data.xts$`World oil prod`,type="trend",lags = 10, selectlags = "AIC"))
summary(ur.df(data.xts$`World oil prod`,type="drift",lags = 10, selectlags = "AIC"))
summary(ur.df(data.xts$`World oil prod`,type="none",lags = 10, selectlags = "AIC"))


WOP = na.omit(data.xts$OP)
PetInv = data.xts$OI
dPetInv = na.omit(data.xts$dOI)
REA = data.xts$`World REA Index`
RO = data.xts$RO
lRO = data.xts$lRO

testROt <- ur.df(RO,type="trend",lags = 10, selectlags = "AIC") #Vælger 2 lags
testROt <- ur.df(RO,type="trend",lags = 6)
testROd <- ur.df(RO,type="drift", lags = 10, selectlags = "AIC")
testROd <- ur.df(RO,type="drift", lags = 6)


ddRO = diff(RO)
testROn <- ur.df(ddRO,type="drift", lags = 10, selectlags = "AIC")
summary(testROt)
summary(testROd) #ved 6 lags er der unit root
summary(testROn)


fit.lin2 <- lm(RO ~ 1)
checkresiduals(fit.lin2)
adf.test(RO)

Box.test(testROn@res,lag=1, type="Lj")

testlROt <- ur.df(lRO,type="trend",lags = 10, selectlags = "AIC")
testlROd <- ur.df(lRO,type="drift", lags = 10, selectlags = "AIC")
testlROn <- ur.df(lRO,type="none", lags = 10, selectlags = "AIC")
summary(testlROt)
summary(testlROd)
summary(testlROn)

testWOPt <- ur.df(WOP,type="trend",lags = 10, selectlags = "AIC")
testWOPd <- ur.df(WOP,type="drift", lags = 10, selectlags = "AIC")
testWOPn <- ur.df(WOP,type="none", lags = 10, selectlags = "AIC")

summary(testWOPt)
summary(testWOPd)
summary(testWOPn)
plot(WOP)


testOIt <- ur.df(PetInv,type="trend",lags = 30, selectlags = "AIC")
testOId <- ur.df(PetInv,type="drift", lags = 30, selectlags = "AIC")
testOIn <- ur.df(PetInv,type="none", lags = 30, selectlags = "AIC")

test<-adf.test(PetInv, k = 30)
test

summary(testOIt)
summary(testOId)
summary(testOIn)

plot(PetInv)

testdOIt <- ur.df(dPetInv,type="trend",lags = 30, selectlags = "AIC")
testdOId <- ur.df(dPetInv,type="drift", lags = 30, selectlags = "AIC")
testdOIn <- ur.df(dPetInv,type="none", lags = 30, selectlags = "AIC")

summary(testdOIt)
summary(testdOId)
summary(testdOIn)

plot(dPetInv)

testREAt <- ur.df(REA,type="trend",lags = 10, selectlags = "AIC")
testREAd <- ur.df(REA,type="drift", lags = 10, selectlags = "AIC")
testREAn <- ur.df(REA,type="none", lags = 10, selectlags = "AIC")

summary(testREAt)
summary(testREAd)
summary(testREAn)

plot(REA)




# Arima -------------------------------------------------------------------

lROsub = subset(lRO.ts, end = 226)
autoplot(lROsub)

testlROt <- ur.df(lROsub,type="trend",lags = 12, selectlags = "AIC")
testlROd <- ur.df(lROsub,type="drift", lags = 12, selectlags = "AIC")
testlROn <- ur.df(lRO,type="none", lags = 12, selectlags = "AIC")
summary(testlROt)
summary(testlROd)
summary(testlROn)

lROsub %>% diff() %>% ggtsdisplay(main="")

lRO = data.xts$lRO["1973-02/2018-06"]
lRO.ts <- ts(lRO,frequency=12,start=c(1973, 2), end=c(2018, 6))
autoplot(lRO.ts)

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
  

#Arima testing

lROsub = subset(lRO.ts, end = 226)

autofit = auto.arima(lROsub)
summary(autofit)
checkresiduals(autofit)
pred1 <- predict(autofit, n.ahead= 1)

fit = Arima(lROsub, order=c(12,0,0), include.mean = T)
summary(fit)
checkresiduals(fit)
pred2 <- predict(fit, n.ahead= 1)

plot(fit, type = c("ar"), xlab = "Real", ylab = "Imaginary")
attributes(autoplot(fit))
autoplot(fit)$data
autoplot(forecast(fit))


# VAR testing---------------------------------------------------------------------

#data.fin=merge(data.xts,d_WTI,..............,join='inner')
#data.xts$``

data.VAR <- data.xts[-1,c("World REA Index","dOI","lRO","OP")]

#data.VAR["1973-01/2003-06"]

VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

VARselect(data.VAR,lag.max =  12)$selection
VAR <- VAR(data.VAR, p=12)
VAR2 <- VAR(VAR.ts, p=12)

serial.test(VAR, lags.pt=48, type="PT.asymptotic") 
serial.test(VAR, lags.pt=80, type = "PT.adjusted")
serial.test(VAR, lags.bg =12, type = "BG")

x <- fevd(VAR, n.ahead = 5)
x


mq(VAR.ts)
res <- residuals(VAR)
VAR.ts = ts(VAR$datamat)
LjungBox(res, lags = 2)
Acoef(VAR) ### [KxK] matrix med koeff


forecast(VARf, h=1)

f.var = forecast(VAR2, h=1)
f.var$forecast$lRO


# VAR FORECASTS ALL 4 VARIABLES -------------------------------------------

data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
data.VAR <- data.xts[-1,c("Kilian","dOI","l_diff_RO","OP")]
data.VAR <- data.xts[-1,c("World REA Index","dOI","lRO","OP")]
VARselect(data.VAR,lag.max =  12)$selection
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))
v=c(NA)

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


# 4 Variables, h=3 ---------------------------------------------------------

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




# Replication -------------------------------------------------------------

data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

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



