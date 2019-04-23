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
#library(aTSA)
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




# Arima -------------------------------------------------------------------

WTI["1973-01/2003-06"]
plot(WTI)

WTI.ts <- ts(WTI,frequency=12,start=c(1973, 1), end=c(2003, 6))
autoplot(WTI.ts)

auto.arima(WTI)

fit <- arima(WTI.ts, order = c(1,1,0), include.mean=T)
fit2 <- arima(WTI, order = c(0,1,2), include.mean=T)
#fit3 <- arima(WTI, order = c(3,1,0), include.mean=T)
#fit4 <- arima(WTI, order = c(4,1,0), include.mean=T)
#fit5 <- arima(WTI, order = c(5,1,0), include.mean=T)

summary(fit)
summary(fit2)

checkresiduals(fit)
checkresiduals(fit2)

plot(fit, type = c("ar"), xlab = "Real", ylab = "Imaginary")
attributes(autoplot(fit))
autoplot(fit)$data

autoplot(forecast(fit))


#prediction for 2006Q1
pred1 <- predict(fit, n.ahead= 1)

#prediction for 2006Q2
den2 <- den["/2006-3"]
fit2 <- arima(den2$dlden, order = c(2,0,0), include.mean=T)
pred2 <- predict(fit2, n.ahead = 1)







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
data.VAR <- data.xts[-1,c("World REA Index","dOI","lRO","OP")]
VARselect(data.VAR,lag.max =  12)$selection
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))


for (i in 1:182) {
  train = VAR.ts[1:363+i, ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=3)
  fcst=recursive$fcst$lRO[3,"fcst"]
  v[i]=fcst
}
test = subset(VAR.ts[, "lRO"],start=365,end = 545)
RW = VAR.ts[-c(1:363), "lRO"]
RWf.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
VARf.ts = ts(v[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))

autoplot(subset(VAR.ts[, "lRO"], end = 545)) +
#autoplot(test) + #evt: series = "Real Price"
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

#Resultater (opdateres)
##12 lags, h=1: MSPE=1.135761
##24 lags, h=1: MSPE=1.493964

plot(ts(PE_VAR^2))
lines(PE_RW^2, col = "red")

#andet plot
df = data.frame(date = as.Date(rownames(VAR.ts)),
                RealOilPrice  = VAR.ts[,"lRO"],
                fitted=c(rep(NA, 3), fitted(VAR)[ ,1], rep(NA, 24)),
                forecast = c(rep(NA,132), recursive[, 1]))

dfm = melt(df,id.vars = "date")
ggplot(data = dfm) + geom_line(aes(x = date, y = value, color = variable))+
  geom_hline(yintercept = mean(train[ ,1]), linetype = 2,color = "yellow")


# Replication -------------------------------------------------------------
data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]

for (i in 0:213) {
  train = VAR.ts[1:(226+i), ]
  VARf <- VAR(train, p=12)
  recursive = predict(VARf, n.ahead=1)
  fcst=recursive$fcst$lRO[,"fcst"]
  v[i+1]=fcst
}

test = subset(VAR.ts[, "lRO"],start=227,end = 439)
RW = VAR.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:212], frequency=12, start=c(1991, 12), end=c(2009, 8))
VARf.ts = ts(v[1:212], frequency=12, start=c(1991, 12), end=c(2009, 8))

#autoplot(subset(VAR.ts[, "lRO"], end = 438)) +
autoplot(test, series = "Real Price") +
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


# kode --------------------------------------------------------------------



get_statistics <- function(ts_df, h=1, start=c(1973, 1), end=c(2018, 6), est_periods_OOS = 20) {
  #1. Historical mean model
  #avg   <- mean(window(ts_df, start, end), na.rm=TRUE)
  #IS_error_N <- (window(ts_df, start, end) - avg)
  #2. OLS model
  reg <- VAR
  IS_error_A <- reg$residuals
  ### 
  ####OOS ANALYSIS
  OOS_error_N <- numeric(end - start - est_periods_OOS) 
  OOS_error_A <- numeric(end - start - est_periods_OOS)
  #Only use information that is available up to the time at which the forecast is made
  j <- 0
  for (i in (start + est_periods_OOS):(end-1)) {
    j <- j + 1
    #Get the actual ERP that you want to predict
    actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
    #1. Historical mean model
    OOS_error_N[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
    #2. OLS model
    reg_OOS <- dyn$lm(eval(parse(text=dep)) ~ lag(eval(parse(text=indep)), -1), 
                      data=window(ts_df, start, i))
    #Compute_error
    df <- data.frame(x=as.numeric(window(ts_df, i, i)[, indep]))
    names(df) <- indep
    pred_ERP   <- predict.lm(reg_OOS, newdata=df)
    OOS_error_A[j] <-  pred_ERP - actual_ERP
  }
  #Compute statistics 
  MSE_N <- mean(OOS_error_N^2)
  MSE_A <- mean(OOS_error_A^2)
  T <- length(!is.na(ts_df[, dep]))
  OOS_R2  <- 1 - MSE_A/MSE_N
  #Is the -1 enough (maybe -2 needed because of lag)?
  OOS_oR2 <- OOS_R2 - (1-OOS_R2)*(reg$df.residual)/(T - 1) 
  dRMSE <- sqrt(MSE_N) - sqrt(MSE_A)
  ##
  #### CREATE PLOT
  IS  <- cumsum(IS_error_N[2:length(IS_error_N)]^2)-cumsum(IS_error_A^2)
  OOS <- cumsum(OOS_error_N^2)-cumsum(OOS_error_A^2)
  df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end), 
                    IS=IS[(1 + est_periods_OOS):length(IS)], 
                    OOS=OOS) #Because you lose one observation due to the lag
  #Shift IS errors vertically, so that the IS line begins 
  # at zero on the date of first OOS prediction. (see Goyal/Welch (2008, p. 1465))
  df$IS <- df$IS - df$IS[1] 
  df  <- melt(df, id.var="x") 
  plotGG <- ggplot(df) + 
    geom_line(aes(x=x, y=value,color=variable)) + 
    geom_rect(data=data.frame(),#Needed by ggplot2, otherwise not transparent
              aes(xmin=1973, xmax=1975,ymin=-0.2,ymax=0.2), 
              fill='red',
              alpha=0.1) + 
    geom_line(aes(x=x, y=value,color=variable)) + 
    geom_rect(data=data.frame(),#Needed by ggplot2, otherwise not transparent
              aes(xmin=1998, xmax=2000,ymin=-0.2,ymax=0.2), 
              fill='brown',
              alpha=0.1) + 
    geom_rect(data=data.frame(),#Needed by ggplot2, otherwise not transparent
              aes(xmin=2007, xmax=2009,ymin=-0.2,ymax=0.2), 
              fill='yellow',
              alpha=0.1) + 
    geom_rect(data=data.frame(),#Needed by ggplot2, otherwise not transparent
              aes(xmin=1929, xmax=1931,ymin=-0.2,ymax=0.2), 
              fill='purple',
              alpha=0.1) + 
    scale_y_continuous('Cumulative SSE Difference', limits=c(-0.23, 0.23)) + 
    scale_x_continuous('Year')
  ##
  oos.sequence <- {(actual_ERP - avg)^2 - 
      (actual_ERP - reg_OOS$fitted)^2 + 
      (avg - reg_OOS$fitted)^2}
  mu <- mean(oos.sequence)
  
  
  return(list(IS_error_N = IS_error_N,
              IS_error_A = reg$residuals,
              OOS_error_N = OOS_error_N,
              OOS_error_A = OOS_error_A,
              IS_R2 = summary(reg)$r.squared, 
              IS_aR2 = summary(reg)$adj.r.squared, 
              OOS_R2  = OOS_R2,
              OOS_oR2 = OOS_oR2,
              dfres = reg$df.residual,
              dRMSE = dRMSE,
              MSE_A = MSE_A,
              MSE_N = MSE_N,
              dep = mean(reg_OOS$fitted-avg),
              mu = mu,
              
              plotGG = plotGG))
}




