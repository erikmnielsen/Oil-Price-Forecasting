rm(list=ls(all=T)) 

library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)
library(fDMA)

# VAR FORECASTS ALL 4 VARIABLES


# Data Preparation --------------------------------------------------------

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

data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.dVAR <- data.xts[-1,c("Kilian","dOI","l_diff_RO","OP")] 
dVAR.ts <- ts(data.dVAR,frequency=12,start=c(1973, 2), end=c(2018, 6))


# h=1 -------------------------------------------
m_h1=matrix(nrow=182, ncol=5)
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h1[i+1,1]=predict(VARf, n.ahead=1)$fcst$lRO[,"fcst"]
} #12 lag, MSPE=1.004544 
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h1[i+1,2]=predict(VARf, n.ahead=1)$fcst$lRO[,"fcst"]
} #24 lag
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h1[i+1,3]=predict(VARf, n.ahead=1)$fcst$l_diff_RO[,"fcst"]
} #12 lag diff, MSPE=0.7104026 
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h1[i+1,4]=predict(VARf, n.ahead=1)$fcst$l_diff_RO[,"fcst"]
} #24 lag diff
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=1)
  m_h1[i+1,5]=predict(VARf, n.ahead=1)$fcst$lRO[,"fcst"]
} #1 lag

# h=3 ---------------------------------------------------------
m_h3=matrix(nrow=182, ncol=4)
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h3[i+1,1]=predict(VARf, n.ahead=3)$fcst$lRO[3,"fcst"]
} #MSPE=1.070662
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h3[i+1,2]=predict(VARf, n.ahead=3)$fcst$lRO[3,"fcst"]
}
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h3[i+1,3]=predict(VARf, n.ahead=3)$fcst$l_diff_RO[3,"fcst"]
}
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h3[i+1,4]=predict(VARf, n.ahead=3)$fcst$l_diff_RO[3,"fcst"]
}

# h=6 ---------------------------------------------------------------------
m_h6=matrix(nrow=182, ncol=4)
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h6[i+1,1]=predict(VARf, n.ahead=6)$fcst$lRO[6,"fcst"]
} # MSPE=1.185503
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h6[i+1,2]=predict(VARf, n.ahead=6)$fcst$lRO[6,"fcst"]
}
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h6[i+1,3]=predict(VARf, n.ahead=6)$fcst$l_diff_RO[6,"fcst"]
}
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h6[i+1,4]=predict(VARf, n.ahead=6)$fcst$l_diff_RO[6,"fcst"]
}

# h=9 ---------------------------------------------------------------------
m_h9=matrix(nrow=182, ncol=4)
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h9[i+1,1]=predict(VARf, n.ahead=9)$fcst$lRO[9,"fcst"]
} # MSPE=1.242557
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h9[i+1,2]=predict(VARf, n.ahead=9)$fcst$lRO[9,"fcst"]
} # MSPE=
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h9[i+1,3]=predict(VARf, n.ahead=9)$fcst$l_diff_RO[9,"fcst"]
}
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h9[i+1,4]=predict(VARf, n.ahead=9)$fcst$l_diff_RO[9,"fcst"]
}

# h=12 ---------------------------------------------------------------------
m_h12=matrix(nrow=182, ncol=4)
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h12[i+1,1]=predict(VARf, n.ahead=12)$fcst$lRO[12,"fcst"]
} # MSPE=1.301601
for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h12[i+1,2]=predict(VARf, n.ahead=12)$fcst$lRO[12,"fcst"]
}
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=12)
  m_h12[i+1,3]=predict(VARf, n.ahead=12)$fcst$l_diff_RO[12,"fcst"]
}
for (i in 0:181) {
  train = dVAR.ts[1:(364+i), ]
  VARf <- VAR(train, p=24)
  m_h12[i+1,4]=predict(VARf, n.ahead=12)$fcst$l_diff_RO[12,"fcst"]
}

# Forecast Accuracy -----------------------------------------------------------------------
{
test_h1 = subset(VAR.ts[, "lRO"],start=365,end = 545)
test_dh1 = subset(dVAR.ts[, "l_diff_RO"],start=365,end = 545)
test_h3 = subset(VAR.ts[, "lRO"],start=367,end = 545)
test_dh3 = subset(dVAR.ts[, "l_diff_RO"],start=367,end = 545)
test_h6 = subset(VAR.ts[, "lRO"],start=369,end = 545)
test_dh6 = subset(dVAR.ts[, "l_diff_RO"],start=369,end = 545)
test_h9 = subset(VAR.ts[, "lRO"],start=371,end = 545)
test_dh9 = subset(dVAR.ts[, "l_diff_RO"],start=371,end = 545)
test_h12 = subset(VAR.ts[, "lRO"],start=373,end = 545)
test_dh12 = subset(dVAR.ts[, "l_diff_RO"],start=373,end = 545)

RW = VAR.ts[-c(1:363), "lRO"]
dRW = dVAR.ts[-c(1:363), "l_diff_RO"]

RWf_h1.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
dRWf_h1.ts = ts(dRW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
RWf_h3.ts = ts(RW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))
dRWf_h3.ts = ts(dRW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))
RWf_h6.ts = ts(RW[1:177], frequency=12, start=c(2003, 10), end=c(2018, 6))
dRWf_h6.ts = ts(dRW[1:177], frequency=12, start=c(2003, 10), end=c(2018, 6))
RWf_h9.ts = ts(RW[1:175], frequency=12, start=c(2003, 12), end=c(2018, 6))
dRWf_h9.ts = ts(dRW[1:175], frequency=12, start=c(2003, 12), end=c(2018, 6))
RWf_h12.ts = ts(RW[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))
dRWf_h12.ts = ts(dRW[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))
}

m=matrix(nrow=45, ncol=10)
{}
for (i in 1:5) {
  VARf.ts = ts(m_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
  if(i<3){
    m[i,1]=mean((VARf.ts-test_h1)^2)/mean((RWf_h1.ts-test_h1)^2) 
    m[i,2]=hit.ratio(y=test_h1,y.hat=VARf.ts,d=FALSE)
  } else{
    m[i,1]=mean((VARf.ts-test_dh1)^2)/mean((dRWf_h1.ts-test_dh1)^2)
    m[i,2]=hit.ratio(y=test_dh1,y.hat=VARf.ts,d=FALSE)
  }}
for (i in 1:5) {
  VARf.ts = ts(m_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
  if(i<3){
    m[i,3]=mean((VARf.ts-test_h3)^2)/mean((RWf_h3.ts-test_h3)^2)
    m[i,4]=hit.ratio(y=test_h3,y.hat=VARf.ts,d=FALSE) 
  } else{
    m[i,3]=mean((VARf.ts-test_dh3)^2)/mean((dRWf_h3.ts-test_dh3)^2)
    m[i,4]=hit.ratio(y=test_dh3,y.hat=VARf.ts,d=FALSE)
  }}
for (i in 1:5) {
  VARf.ts = ts(m_h6[1:177,i], frequency=12, start=c(2003, 10), end=c(2018, 6))
  if(i<3){
    m[i,5]=mean((VARf.ts-test_h6)^2)/mean((RWf_h6.ts-test_h6)^2)
    m[i,6]=hit.ratio(y=test_h6,y.hat=VARf.ts,d=FALSE)
  } else{
    m[i,5]=mean((VARf.ts-test_dh6)^2)/mean((dRWf_h6.ts-test_dh6)^2)
    m[i,6]=hit.ratio(y=test_dh6,y.hat=VARf.ts,d=FALSE)
  }}
for (i in 1:5) {
  VARf.ts = ts(m_h9[1:175,i], frequency=12, start=c(2003, 12), end=c(2018, 6))
  if(i<3){
    m[i,7]=mean((VARf.ts-test_h9)^2)/mean((RWf_h9.ts-test_h9)^2)
    m[i,8]=hit.ratio(y=test_h9,y.hat=VARf.ts,d=FALSE)
  } else{
    m[i,7]=mean((VARf.ts-test_dh9)^2)/mean((dRWf_h9.ts-test_dh9)^2)
    m[i,8]=hit.ratio(y=test_dh9,y.hat=VARf.ts,d=FALSE)
  }}
for (i in 1:5) {
  VARf.ts = ts(m_h12[1:173,i], frequency=12, start=c(2004, 2), end=c(2018, 6))
  if(i<3){
    m[i,9]=mean((VARf.ts-test_h12)^2)/mean((RWf_h12.ts-test_h12)^2)
    m[i,10]=hit.ratio(y=test_h12,y.hat=VARf.ts,d=FALSE) 
  } else{
    m[i,9]=mean((VARf.ts-test_dh12)^2)/mean((dRWf_h12.ts-test_dh12)^2)
    m[i,10]=hit.ratio(y=test_dh12,y.hat=VARf.ts,d=FALSE)
  }}

m


#autoplot(subset(VAR.ts[, "lRO"], end = 545)) +
autoplot(test) + #evt: series = "Real Price"
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Forecasts:")) +
  theme_economist() +
  scale_color_economist()


