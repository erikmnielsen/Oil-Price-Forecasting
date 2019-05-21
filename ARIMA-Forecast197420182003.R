rm(list=ls(all=T)) 
library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)
library(fDMA)
library(quantmod)
library(grid)
library(gridExtra)

eriksPT_test=function(Actual, Forecast){
  
  obsx=as.numeric(Actual)
  fcst=as.numeric(Forecast)
  
  delta_obsx=as.matrix(cbind(ifelse(obsx-Lag(obsx)>0,1,0)[-1])) #calc change of actual (delta)
  delta_fcst=as.matrix(cbind(ifelse(fcst-Lag(fcst)>0,1,0)[-1])) #calc change of forecast (delta)
  TrFa=ifelse(delta_obsx-delta_fcst==0, 1, 0)
  
  n=nrow(delta_obsx)
  Pyz=mean(TrFa)
  Py=mean(delta_obsx)
  Pz=mean(delta_fcst)
  p=Py%*%Pz+(1-Py)%*%(1-Pz)
  v=(p%*%(1-p))/n
  w=(((2%*%Pz-1)^2%*%Py%*%(1-Py))/n)+(((2%*%Py-1)^2%*%Pz%*%(1-Pz))/n)+((4%*%Py%*%Pz%*%(1-Py)%*%(1-Pz))/n^2)
  
  Sn=((p%*%(1-p))/n)^(-1/2)*(Pyz-p) #sidste ligning s.461 i Timmermann
  Sn2=(Pyz-p)/sqrt(((Pyz%*%(1-Pyz))/n)-w) #ligning 6 i Timmermann
  PT=(Pyz-p)/sqrt(v-w) #http://www.real-statistics.com/time-series-analysis/forecasting-accuracy/pesaran-timmermann-test/
  pv=1-pnorm(PT)
  summary=c(PT,pv)
  names(summary)=c("PT statistic","p.value")
  summary
  #print(Sn)
  #print(Sn2)
}
MDirAcc <- function(Actual, Forecast, lag=1) {
  return( mean(sign(diff(Actual, lag=lag))==sign(diff(Forecast, lag=lag))) )
}


# Variable <- read_excel("~/8.semester projekt/Variable.xlsx")
data <- read_excel("VariableHam74.xlsx")
data <- as.data.frame(data)
data$RO <- (data$WTI/(data$`CPI US`/100))
data$lRO <- log(data$RO)
data$l_diff_RO <- diff.xts(data$lRO, differences = 1)
data$OP <- log(data$`World oil prod`) - lag.xts(log(data$`World oil prod`),k = 1) 
data$OI <- (data$`OECD Pet inv`/data$`US Pet inv`)*data$`US crude inv`
data$dOI <- diff.xts(data$OI,differences = 1)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"

data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["1974-1/2018-06"]

lRO = data.xts$lRO["1974-02/2018-06"]
lRO.ts <- ts(lRO,frequency=12,start=c(1974, 2), end=c(2018, 6))
dlRO = data.xts$l_diff_RO["1974-02/2018-06"]
dlRO.ts <- ts(dlRO, frequency=12,start=c(1974, 2), end=c(2018, 6))

data.xts$observationer = 1:nrow(data.xts) 
length(lRO)

View(data.xts)


#total er lig 534, 200306 er lig med 354, forskel er lig med 180

nc=10
m_h1=matrix(nrow=182, ncol=nc)
m_h3=matrix(nrow=182, ncol=nc)
m_h6=matrix(nrow=182, ncol=nc)
m_h9=matrix(nrow=182, ncol=nc)
m_h12=matrix(nrow=182, ncol=nc)

for (j in 1:nc){
  for (i in 0:181) {
    train = lRO.ts[1:(352+i), ]
    
    if (j==1){arimaf = Arima(train, order=c(1,0,0))}
    if (j==2){arimaf = Arima(train, order=c(2,0,0))}
    if (j==3){arimaf = Arima(train, order=c(12,0,0))}
    if (j==4){arimaf = Arima(train, order=c(24,0,0), method="CSS")}
    if (j==5){arimaf = Arima(train, order=c(1,0,1))}
    if (j==6){arimaf = Arima(train, order=c(2,0,1))}
    if (j==7){arimaf = Arima(train, order=c(1,1,0))}
    if (j==8){arimaf = Arima(train, order=c(11,1,0))}
    if (j==9){arimaf = Arima(train, order=c(23,1,0), method="CSS")}
    if (j==10){arimaf = Arima(train, order=c(0,1,1))}
   
    m_h1[i+1,j]=predict(arimaf, n.ahead=1)$pred[1]
    m_h3[i+1,j]=predict(arimaf, n.ahead=3)$pred[3]
    m_h6[i+1,j]=predict(arimaf, n.ahead=6)$pred[6]
    m_h9[i+1,j]=predict(arimaf, n.ahead=9)$pred[9]
    m_h12[i+1,j]=predict(arimaf, n.ahead=12)$pred[12]
  }
}

# Forecast Accuracy -----------------------------------------------------------------------
test_h1 = subset(lRO.ts[, "lRO"],start=353,end = 533)
test_h3 = subset(lRO.ts[, "lRO"],start=355,end = 533)
test_h6 = subset(lRO.ts[, "lRO"],start=358,end = 533)
test_h9 = subset(lRO.ts[, "lRO"],start=361,end = 533)
test_h12 = subset(lRO.ts[, "lRO"],start=364,end = 533)
test_dh1 = subset(dlRO.ts[, "l_diff_RO"],start=553,end = 533)
test_dh3 = subset(dlRO.ts[, "l_diff_RO"],start=355,end = 533)
test_dh6 = subset(dlRO.ts[, "l_diff_RO"],start=358,end = 533)
test_dh9 = subset(dlRO.ts[, "l_diff_RO"],start=361,end = 533)
test_dh12 = subset(dlRO.ts[, "l_diff_RO"],start=364,end = 533)


length(dlRO.ts)

RW = lRO.ts[-c(1:351), "lRO"]
RWf_h1.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
RWf_h3.ts = ts(RW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))
RWf_h6.ts = ts(RW[1:176], frequency=12, start=c(2003, 10), end=c(2018, 6))
RWf_h9.ts = ts(RW[1:173], frequency=12, start=c(2003, 12), end=c(2018, 6))
RWf_h12.ts = ts(RW[1:170], frequency=12, start=c(2004, 2), end=c(2018, 6))


m=matrix(nrow=nc, ncol=20)
pw=2
pt=2
x=11

for (i in 1:nc) {
  arimaf.ts = ts(m_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
  if(i<x){
    PE.ARIMA=arimaf.ts-test_h1
    PE.RW=RWf_h1.ts-test_h1
    m[i,1]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,2]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=1, power=pw)$p.value
    m[i,3]=MDirAcc(test_h1, arimaf.ts)
    m[i,4]=eriksPT_test(test_h1, arimaf.ts)[pt]
  } else{
    PE.dARIMA=arimaf.ts-test_dh1
    PE.dRW=test_dh1
    m[i,1]=mean((PE.dARIMA)^2)/mean((PE.dRW)^2)
    m[i,2]=dm.test(PE.dARIMA, PE.dRW, alternative = "less", h=1, power=pw)$p.value
    m[i,3]=MDirAcc(test_dh1, arimaf.ts)
    m[i,4]=eriksPT_test(test_dh1, arimaf.ts)[pt]
  }
}
for (i in 1:nc) {
  arimaf.ts = ts(m_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
  if(i<x){
    PE.ARIMA=arimaf.ts-test_h3
    PE.RW=RWf_h3.ts-test_h3
    m[i,5]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,6]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=3, power=pw)$p.value
    #m[i,7]=hit.ratio(y=test_h3,y.hat=arimaf.ts,d=FALSE)
    m[i,7]=MDirAcc(test_h3, arimaf.ts)
    m[i,8]=eriksPT_test(test_h3, arimaf.ts)[pt]
  } else{
    PE.dARIMA=arimaf.ts-test_dh3
    PE.dRW=test_dh3
    m[i,5]=mean((PE.dARIMA)^2)/mean((PE.dRW)^2)
    m[i,6]=dm.test(PE.dARIMA, PE.dRW, alternative = "less", h=3, power=pw)$p.value
    #m[i,7]=hit.ratio(y=test_dh3,y.hat=arimaf.ts,d=FALSE)
    m[i,7]=MDirAcc(test_dh3, arimaf.ts)
    m[i,8]=eriksPT_test(test_dh3, arimaf.ts)[pt]
  }
}
for (i in 1:nc) {
  arimaf.ts = ts(m_h6[1:176,i], frequency=12, start=c(2003, 11), end=c(2018, 6))
  if(i<x){
    PE.ARIMA=arimaf.ts-test_h6
    PE.RW=RWf_h6.ts-test_h6
    m[i,9]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,10]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=6, power=pw)$p.value
    #m[i,11]=hit.ratio(y=test_h6,y.hat=arimaf.ts,d=FALSE)
    m[i,11]=MDirAcc(test_h6, arimaf.ts)
    m[i,12]=eriksPT_test(test_h6, arimaf.ts)[pt]
  } else{
    PE.dARIMA=arimaf.ts-test_dh6
    PE.dRW=test_dh6
    m[i,9]=mean((PE.dARIMA^2))/ mean((PE.dRW)^2)
    m[i,10]=dm.test(PE.dARIMA, PE.dRW, alternative = "less", h=6, power=pw)$p.value
    m[i,11]=MDirAcc(test_dh6, arimaf.ts)
    m[i,12]=eriksPT_test(test_dh6, arimaf.ts)[pt]
  }
}
for (i in 1:nc) {
  arimaf.ts = ts(m_h9[1:173,i], frequency=12, start=c(2004, 2), end=c(2018, 6))
  if(i<x){
    PE.ARIMA=arimaf.ts-test_h9
    PE.RW=RWf_h9.ts-test_h9
    m[i,13]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,14]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=9, power=pw)$p.value
    m[i,15]=MDirAcc(test_h9, arimaf.ts)
    m[i,16]=eriksPT_test(test_h9, arimaf.ts)[pt]
  } else{
    PE.dARIMA=arimaf.ts-test_dh9
    PE.dRW=test_dh9
    m[i,13]=mean((PE.dARIMA^2))/ mean((PE.dRW)^2)
    m[i,14]=dm.test(PE.dARIMA, PE.dRW, alternative = "less", h=9, power=pw)$p.value
    m[i,15]=MDirAcc(test_dh9, arimaf.ts)
    m[i,16]=eriksPT_test(test_dh9, arimaf.ts)[2]
  }
}
for (i in 1:nc) {
  arimaf.ts = ts(m_h12[1:170,i], frequency=12, start=c(2004, 5), end=c(2018, 6))
  if(i<x){
    PE.ARIMA=arimaf.ts-test_h12
    PE.RW=RWf_h12.ts-test_h12
    m[i,17]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,18]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=12, power=pw)$p.value
    m[i,19]=MDirAcc(test_h12, arimaf.ts)
    m[i,20]=eriksPT_test(test_h12, arimaf.ts)[pt]
  } else{
    PE.dARIMA=arimaf.ts-test_dh12
    PE.dRW=test_dh12
    m[i,17]=mean((PE.dARIMA^2))/ mean((PE.dRW)^2)
    m[i,18]=dm.test(PE.dARIMA, PE.dRW, alternative = "less", h=12, power=pw)$p.value
    m[i,19]=MDirAcc(test_dh12, arimaf.ts)
    m[i,20]=eriksPT_test(test_dh12, arimaf.ts)[pt]
  }
}




colnames(m) = c("h=1","pv","SR","pv","h=3","pv","SR","pv","h=6","pv","SR","pv","h=9","pv","SR","pv","h=12","pv","SR","pv")
#p = c(1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,12,24)
#m = cbind(p, m)

m.df = as.data.frame(m, row.names = c("AR(1)", "AR(2)", "AR(12)", "AR(24)", "ARMA(1,1)", "ARMA(2,1)",
                                      "ARIMA(1,1,0)", "ARIMA(11,1,0)", "ARIMA(23,1,0)", 
                                      "ARIMA(0,1,1)"))
m.df =round(m.df, digits = 3)
grid.table(m.df)







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






