# Databehandling ----------------------------------------------------------
rm(list=ls(all=T)) 
library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)
library(fDMA)
library(xts)
library(dynlm)

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

#Variable
WTI = data.xts$WTI
WOP = na.omit(data.xts$OP)
PetInv = data.xts$OI
dPetInv = na.omit(data.xts$dOI)
REA = data.xts$`World REA Index`
Ham = data.xts$Hamilton
RO = data.xts$RO
lRO = data.xts$lRO
OI = data.xts$OI
Kilian = data.xts$Kilian
HamOECD = data.xts$HamiltonOECD

data.xts
# World oil production ----------------------------------------------------
testt = (ur.df(WOP,type="trend",lags = 11))
summary(testt) #stationær

x = ts(WOP)
diff.x = ts(lag(x,1) - x)
n = length(diff.x)
lags = 1
temps = seq_along(diff.x)
summary(dynlm(diff.x ~1 + temps + lag(x,-1) + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8) + lag(diff.x,-9) + lag(diff.x,-10) + lag(diff.x,-11))) #stationær

# lRO ----------------------------------------------------------------------
summary(ur.df(lRO,type="trend",lags = 6)) #ikke-stationær

x = ts(lRO)
diff.x = ts(lag(x,1) - x)
n = length(diff.x)
temps = seq_along(diff.x)
summary(dynlm(diff.x ~1 + temps + lag(x,-1) + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6)))

summary(ur.df(lRO,type="drift", lags = 6)) #på grænsen til stationær

summary(dynlm(diff.x ~1 + lag(x,-1) + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6)))

summary(ur.df(lRO,type="none", lags = 6))

summary(dynlm(diff.x ~0 + lag(x,-1) + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6)))


# Oil inventory -----------------------------------------------------------
summary(ur.df(OI,type="trend",lags = 12)) #ikke-stationær

plot(OI)

x = ts(OI)
diff.x = ts(lag(x,1) - x)
tt = seq_along(diff.x)
summary(dynlm(diff.x ~1 + tt + lag(x,-1) + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8) + lag(diff.x,-9) + lag(diff.x,-10) + lag(diff.x,-11) + lag(diff.x,-12)))

summary(ur.df(OI, type="drift", lags = 12))
summary(dynlm(diff.x ~1 + lag(x,-1) + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8) + lag(diff.x,-9) + lag(diff.x,-10) + lag(diff.x,-11) + lag(diff.x,-12)))

summary(dynlm(diff.x ~1 + tt + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8) + lag(diff.x,-9) + lag(diff.x,-10) + lag(diff.x,-11) + lag(diff.x,-12)))
summary(dynlm(diff.x ~1 + tt + lag(x,-1) + lag(diff.x,-1) + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8) + lag(diff.x,-9) + lag(diff.x,-10) + lag(diff.x,-11) + lag(diff.x,-12)))

summary(ur.df(OI,type="none", lags = 12))

summary(dynlm(diff.x ~1 + lag(x,-1) + lag(diff.x,-1)))

# Hamilton variable -------------------------------------------------------
summary(ur.df(Ham,type="trend",lags = 11)) #stationær

x = ts(Ham)
diff.x = ts(lag(x,1) - x)
tt = seq_along(diff.x)
summary(dynlm(diff.x ~1 + tt + lag(x,-1) + lag(diff.x,-1) + + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8) + lag(diff.x,-9) + lag(diff.x,-10) + lag(diff.x,-11)))

# Kilian ------------------------------------------------------------------
summary(ur.df(Kilian,type="trend",lags = 11))
#Her er resultatet -2,99, hvor nulhypotesen ikke kan forkastes

x = ts(Kilian)
diff.x = ts(lag(x,1) - x)
tt = seq_along(diff.x)
summary(dynlm(diff.x ~1 + tt + lag(x,-1) + lag(diff.x,-1) + + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8) + lag(diff.x,-9) + lag(diff.x,-10) + lag(diff.x,-11)))
# stationær

# HamiltonOECD ------------------------------------------------------------------
summary(ur.df(HamOECD,type="trend",lags = 8)) #stationær

x = ts(HamOECD)
diff.x = ts(lag(x,1) - x)
tt = seq_along(diff.x)
summary(dynlm(diff.x ~1 + tt + lag(x,-1) + lag(diff.x,-1) + + lag(diff.x,-2) + lag(diff.x,-3) + lag(diff.x,-4) + lag(diff.x,-5) + lag(diff.x,-6) + + lag(diff.x,-7) + lag(diff.x,-8)))
# stationær



