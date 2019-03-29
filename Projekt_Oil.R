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


# Variable <- read_excel("~/8.semester projekt/Variable.xlsx")
data <- read_excel("Variable.xlsx")
data <- as.data.frame(data)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"

View(data)
data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["1973-01/2018-06"]
plot(data.xts$WTI)

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






WOP = data.xts$`World oil prod`
CPI = data.xts$`CPI US`
PetInv = data.xts$`OECD Pet inv`
REA = data.xts$`World REA Index`

testt <- ur.df(WOP,type="trend",lags = 10, selectlags = "AIC")
testd <- ur.df(WOP,type="drift", lags = 10, selectlags = "AIC")
testn <- ur.df(WOP,type="none", lags = 10, selectlags = "AIC")

testt <- ur.df(CPI,type="trend",lags = 10, selectlags = "AIC")
testd <- ur.df(CPI,type="drift", lags = 10, selectlags = "AIC")
testn <- ur.df(CPI,type="none", lags = 10, selectlags = "AIC")

testt <- ur.df(PetInv,type="trend",lags = 10, selectlags = "AIC")
testd <- ur.df(PetInv,type="drift", lags = 10, selectlags = "AIC")
testn <- ur.df(PetInv,type="none", lags = 10, selectlags = "AIC")

testt <- ur.df(REA,type="trend",lags = 10, selectlags = "AIC")
testd <- ur.df(REA,type="drift", lags = 10, selectlags = "AIC")
testn <- ur.df(REA,type="none", lags = 10, selectlags = "AIC")


data.fin=merge(data.xts,d_WTI,..............,join='inner')



# VAR ---------------------------------------------------------------------
VARselect(data.xts)        
VAR <- VAR(data.xts, p=6) 
VAR

serial.test(VAR, lags.pt=10, type="PT.asymptotic")

Acoef(VAR) ### [KxK] matrix med koeff

var1 <- VAR(uschange[,1:2], p=1, type="const")

