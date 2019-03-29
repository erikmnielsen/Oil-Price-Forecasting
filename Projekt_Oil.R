library(readxl)
library(forecast)
library(dplyr)
library(timeSeries)
library(vars)
library(aTSA)
library(xts)
library(urca)
library(rugarch)
library(fBasics)
library(fGarch)
library(zoo)
library(reshape2)
library(mvmeta)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(dynlm)
library(tsDyn)
library(tseries)
library(fpp2)
library(vars)
library(MTS)


# Variable <- read_excel("~/8.semester projekt/Variable.xlsx")
data <- read_excel("Variable.xlsx")
data <- as.data.frame(data)
data$RO <- (data$WTI/(data$`CPI US`/100))
data$lRO <- log(data$RO)
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

View(data.xts)
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

testt <- ur.df(WTI,type="trend",lags = 10, selectlags = "AIC")
testd <- ur.df(WTI,type="drift", lags = 10, selectlags = "AIC")
testn <- ur.df(WTI,type="none", lags = 10, selectlags = "AIC")
summary(testn)

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

testROt <- ur.df(RO,type="trend",lags = 10, selectlags = "AIC")
testROd <- ur.df(RO,type="drift", lags = 10, selectlags = "AIC")
testROn <- ur.df(RO,type="none", lags = 10, selectlags = "AIC")
summary(testROt)
summary(testROd)
summary(testROn)

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

data.fin=merge(data.xts,d_WTI,..............,join='inner')
data.xts$``
data.VAR <- data.xts[-1,c("World REA Index","dOI","lRO","OP")]


# VAR ---------------------------------------------------------------------
VARselect(data.VAR,lag.max =  36)        
VAR <- VAR(data.VAR, p=12) 
VAR

acf(VAR$y)


?serial.test
serial.test(VAR, lags.pt=40)
?mq
mq(VAR.ma)
VAR.ma = as.data.frame(VAR$datamat)

Acoef(VAR) ### [KxK] matrix med koeff

var1 <- VAR(uschange[,1:2], p=1, type="const")

