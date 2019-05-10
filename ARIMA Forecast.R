rm(list=ls(all=T)) 
library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)
library(fDMA)


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

# h=1 forecasts -----------------------------------------------------------
m_h1=matrix(nrow=182, ncol=10)

# AR(12) 
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(12,0,0))
  m_h1[i+1,1]=predict(arimaf, n.ahead=1)$pred[1] #recursive forecast
}

# AR(24) 
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(24,0,0))
  m_h1[i+1,2]=predict(arimaf, n.ahead=1)$pred[1] #recursive forecast
}

# AR() (SIC)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,0,0))
  m_h1[i+1,3]=predict(arimaf, n.ahead=1)$pred[1]
}

# AR(1) (AIC)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,0,0))
  m_h1[i+1,4]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARMA(1,1)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,0,1))
  m_h1[i+1,5]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARIMA(11,1,0)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(11,1,0))
  m_h1[i+1,6]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARIMA(23,1,0)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(23,1,0))
  m_h1[i+1,7]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARIMA(,1,0) SIC
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,1,0))
  m_h1[i+1,8]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARIMA(1,1,0) AIC
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,1,0))
  m_h1[i+1,9]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARIMA(0,1,1)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(0,1,1))
  m_h1[i+1,10]=predict(arimaf, n.ahead=1)$pred[1]
}


# h=3 forecasts -----------------------------------------------------------
m_h3=matrix(nrow=182, ncol=10)

#AR(12)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(12,0,0))
  m_h3[i+1,1]=predict(arimaf, n.ahead=3)$pred[3]
}

#AR(24)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(24,0,0))
  m_h3[i+1,2]=predict(arimaf, n.ahead=3)$pred[3]
}

# AR() (SIC)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,0,0))
  m_h3[i+1,3]=predict(arimaf, n.ahead=3)$pred[3]
}

# AR(1) (AIC)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,0,0))
  m_h3[i+1,4]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARMA(1,1)
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,0,1))
  m_h3[i+1,5]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARIMA(11,1,0) 
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(11,1,0))
  m_h3[i+1,6]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARIMA(23,1,0) 
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(23,1,0))
  m_h3[i+1,7]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARIMA(,1,0) SIC
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,1,0))
  m_h3[i+1,8]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARIMA(1,1,0) AIC
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(1,1,0))
  m_h3[i+1,9]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARIMA(0,1,1) 
for (i in 0:181) {
  train = lRO.ts[1:(364+i), ]
  arimaf = Arima(train, order=c(0,1,1))
  m_h3[i+1,10]=predict(arimaf, n.ahead=3)$pred[3]
}


# Forecast Accuracy -----------------------------------------------------------------------
test_h1 = subset(lRO.ts[, "lRO"],start=365,end = 545)
test_h3 = subset(lRO.ts[, "lRO"],start=367,end = 545)
RW = lRO.ts[-c(1:363), "lRO"]
RWf_h1.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
RWf_h3.ts = ts(RW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))

m=matrix(nrow=10, ncol=10)

for (i in 1:10) {
  arimaf.ts = ts(m_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
  m[i,1]=mean((arimaf.ts-test_h1)^2)/mean((RWf_h1.ts-test_h1)^2)
  m[i,2]=hit.ratio(y=test_h1,y.hat=arimaf.ts,d=FALSE)
}

for (i in 1:10) {
  arimaf.ts = ts(m_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
  m[i,3]=mean((arimaf.ts-test_h3)^2)/mean((RWf_h3.ts-test_h3)^2)
  m[i,4]=hit.ratio(y=test_h3,y.hat=arimaf.ts,d=FALSE)
}

m #fraction of random walk - should be <1 to beat the RW




arimaf.ts = ts(m_h1[1:213,1], frequency=12, start=c(1991, 12), end=c(2009, 8))
#autoplot(subset(lRO.ts, end = 438)) +
autoplot(test_h1, series = "Real Price") +
  autolayer(arimaf.ts, series="arima") +
  #autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()






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


# Replicating, h=1 forecasts -----------------------------------------------------------
m_h1=matrix(nrow=214, ncol=5)

#AR(12), h=1 
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(12,0,0))
  m_h1[i+1,1]=predict(arimaf, n.ahead=1)$pred[1] #recursive forecast
}

# AR(1), h=1 (AIC)
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(1,0,0))
  m_h1[i+1,2]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARMA(1,1), h=1
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(1,0,1))
  m_h1[i+1,3]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARIMA(11,1,0), h=1
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(11,1,0))
  m_h1[i+1,4]=predict(arimaf, n.ahead=1)$pred[1]
}

# ARIMA(0,1,1), h=1
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(0,1,1))
  m_h1[i+1,5]=predict(arimaf, n.ahead=1)$pred[1]
}


# Replicating, h=3 forecasts -----------------------------------------------------------
m_h3=matrix(nrow=214, ncol=5)

#AR(12) MSPE=0.9769135
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(12,0,0))
  m_h3[i+1,1]=predict(arimaf, n.ahead=3)$pred[3]
}

# AR(1) (AIC), MSPE=0.9940179
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(1,0,0))
  recursive=predict(arimaf, n.ahead=3)
  fcst=recursive$pred[3]
  m_h3[i+1,2]=fcst
  #m_h3[i,2]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARMA(1,1), MSPE=0.9623326
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(1,0,1))
  m_h3[i+1,3]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARIMA(11,1,0) MSPE=0.9825416
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(11,1,0))
  m_h3[i+1,4]=predict(arimaf, n.ahead=3)$pred[3]
}

# ARIMA(0,1,1) MSPE=0.9707211
for (i in 0:213) {
  train = lRO.ts[1:(226+i), ]
  arimaf = Arima(train, order=c(0,1,1))
  m_h3[i+1,5]=predict(arimaf, n.ahead=3)$pred[3]
}


# Replicating, Forecast Accuracy -----------------------------------------------------------------------
test_h1 = subset(lRO.ts[, "lRO"],start=227,end = 439)
test_h3 = subset(lRO.ts[, "lRO"],start=229,end = 439)
RW = lRO.ts[-c(1:225), "lRO"]
RWf_h1.ts = ts(RW[1:213], frequency=12, start=c(1991, 12), end=c(2009, 8))
RWf_h3.ts = ts(RW[1:211], frequency=12, start=c(1992, 2), end=c(2009, 8))

m=matrix(nrow=5, ncol=4)

for (i in 1:5) {
  arimaf.ts = ts(m_h1[1:213,i], frequency=12, start=c(1991, 12), end=c(2009, 8))
  m[i,1]=mean((arimaf.ts-test_h1)^2)/mean((RWf_h1.ts-test_h1)^2)
  m[i,2]=hit.ratio(y=test_h1,y.hat=arimaf.ts,d=FALSE)
}

for (i in 1:5) {
  arimaf.ts = ts(m_h3[1:211,i], frequency=12, start=c(1992, 2), end=c(2009, 8))
  m[i,3]=mean((arimaf.ts-test_h3)^2)/mean((RWf_h3.ts-test_h3)^2)
  m[i,4]=hit.ratio(y=test_h3,y.hat=arimaf.ts,d=FALSE)
}

m #fraction of random walk - should be <1 to beat the RW




arimaf.ts = ts(m_h1[1:213,1], frequency=12, start=c(1991, 12), end=c(2009, 8))
  #autoplot(subset(lRO.ts, end = 438)) +
  autoplot(test_h1, series = "Real Price") +
  autolayer(arimaf.ts, series="arima") +
  #autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Series:")) +
  theme_economist() +
  scale_color_economist()


# NN Forecast -----------------------------------------------------------------------

test = subset(lRO.ts[, "lRO"],start=227,end = 256)
RW = lRO.ts[-c(1:225), "lRO"]
RWf.ts = ts(RW[1:30], frequency=12, start=c(1991, 12), end=c(1994, 5))

fit = nnetar(lROsub)
fNN = (forecast(fit, h=30, bootstrap = T, npaths = 10))
autoplot(forecast(fit, h=30))

PE_AR=fNN$mean-test
MSPE_AR = mean((PE_AR)^2)
PE_RW = RWf.ts-test
MSPE_RW = mean((PE_RW)^2)
MSPE_AR/MSPE_RW #fraction of random walk - should be <1 to beat the RW






