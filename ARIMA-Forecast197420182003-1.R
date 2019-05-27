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

pacf(RO)
auto.arima(RO)

# Variable <- read_excel("~/8.semester projekt/Variable.xlsx")
data <- read_excel("VariableHam74.xlsx")
data <- as.data.frame(data)
data$RO <- (data$WTI/(data$`CPI US`/100))
#data$lRO <- log(data$RO)
data$diff_RO <- diff.xts(data$RO, differences = 1)
data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["1974-1/2018-06"]

fit = Arima(RO, order=c(1,0,0), method = "CSS")
autoplot(fit)$data


RO = data.xts$RO
RO.ts <- ts(RO,frequency=12,start=c(1974, 2), end=c(2018, 6))
#lRO = data.xts$lRO["1974-02/2018-06"]
#lRO.ts <- ts(lRO,frequency=12,start=c(1974, 2), end=c(2018, 6))
diffRO = data.xts$diff_RO
dRO.ts <- ts(diffRO, frequency=12,start=c(1974, 2), end=c(2018, 6))

data.xts$observationer = 1:nrow(data.xts) 

#total er lig 534, 200306 er lig med 354, forskel er lig med 180

nc=12
m_h1=matrix(nrow=182, ncol=nc)
m_h3=matrix(nrow=182, ncol=nc)
m_h6=matrix(nrow=182, ncol=nc)
m_h9=matrix(nrow=182, ncol=nc)
m_h12=matrix(nrow=182, ncol=nc)

for (j in 1:nc){
  for (i in 0:181) {
    train = RO.ts[1:(352+i), ]
    
    if (j==1){arimaf = Arima(train, order=c(1,0,0), method = "CSS")}
    if (j==2){arimaf = Arima(train, order=c(2,0,0), method = "CSS")}
    if (j==3){arimaf = Arima(train, order=c(12,0,0), method = "CSS")}
    if (j==4){arimaf = Arima(train, order=c(24,0,0), method = "CSS")}
    if (j==5){arimaf = Arima(train, order=c(1,0,1), method = "CSS")}
    if (j==6){arimaf = Arima(train, order=c(2,0,1), method = "CSS")}
    if (j==7){arimaf = Arima(train, order=c(2,1,2), method = "CSS")}
    if (j==8){arimaf = Arima(train, order=c(1,1,0), method = "CSS")}
    if (j==9){arimaf = Arima(train, order=c(2,1,0), method = "CSS")}
    if (j==10){arimaf = Arima(train, order=c(11,1,0), method = "CSS")}
    if (j==11){arimaf = Arima(train, order=c(23,1,0), method = "CSS")}
    if (j==12){arimaf = Arima(train, order=c(0,1,1), method = "CSS")}
   
    m_h1[i+1,j]=predict(arimaf, n.ahead=1)$pred[1]
    m_h3[i+1,j]=predict(arimaf, n.ahead=3)$pred[3]
    m_h6[i+1,j]=predict(arimaf, n.ahead=6)$pred[6]
    m_h9[i+1,j]=predict(arimaf, n.ahead=9)$pred[9]
    m_h12[i+1,j]=predict(arimaf, n.ahead=12)$pred[12]
  }}


arimaf = Arima(train, order=c(23,1,0), method = "CSS")

m_h1
m_h3
m_h6


# Forecast Accuracy -----------------------------------------------------------------------
test_h1 = subset(RO.ts[, "RO"],start=353,end = 533)
test_h3 = subset(RO.ts[, "RO"],start=355,end = 533)
test_h6 = subset(RO.ts[, "RO"],start=358,end = 533)
test_h9 = subset(RO.ts[, "RO"],start=361,end = 533)
test_h12 = subset(RO.ts[, "RO"],start=364,end = 533)
test_dh1 = subset(dRO.ts[, "diff_RO"],start=353,end = 533)
test_dh3 = subset(dRO.ts[, "diff_RO"],start=355,end = 533)
test_dh6 = subset(dRO.ts[, "diff_RO"],start=358,end = 533)
test_dh9 = subset(dRO.ts[, "diff_RO"],start=361,end = 533)
test_dh12 = subset(dRO.ts[, "diff_RO"],start=364,end = 533)
test_dh1


RW = RO.ts[-c(1:351), "RO"]



RWf_h1.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
RWf_h3.ts = ts(RW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))
RWf_h6.ts = ts(RW[1:176], frequency=12, start=c(2003, 11), end=c(2018, 6))
RWf_h9.ts = ts(RW[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))
RWf_h12.ts = ts(RW[1:170], frequency=12, start=c(2004, 5), end=c(2018, 6))

m=matrix(nrow=nc, ncol=20)
pw=2
pt=2

for (i in 1:nc) {
  arimaf.ts1 = ts(m_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
    PE.ARIMA=arimaf.ts1-test_h1
    PE.RW=RWf_h1.ts-test_h1
    m[i,1]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,2]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=1, power=pw)$p.value
    m[i,3]=hit.ratio(test_h1, arimaf.ts1)
    m[i,4]=eriksPT_test(test_h1, arimaf.ts1)[pt]
}
for (i in 1:nc) {
  arimaf.ts3 = ts(m_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
    PE.ARIMA=arimaf.ts3-test_h3
    PE.RW=RWf_h3.ts-test_h3
    m[i,5]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,6]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=3, power=pw)$p.value
    #m[i,7]=hit.ratio(y=test_h3,y.hat=arimaf.ts,d=FALSE)
    m[i,7]=hit.ratio(test_h3, arimaf.ts3)
    m[i,8]=eriksPT_test(test_h3, arimaf.ts3)[pt]
}


auto.arima(RO.ts)

for (i in 1:nc) {
  arimaf.ts6 = ts(m_h6[1:176,i], frequency=12, start=c(2003, 11), end=c(2018, 6))
    PE.ARIMA=arimaf.ts6-test_h6
    PE.RW=RWf_h6.ts-test_h6
    m[i,9]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,10]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=6, power=2)$p.value
    m[i,11]=hit.ratio(test_h6, arimaf.ts6)
    m[i,12]=eriksPT_test(test_h6, arimaf.ts6)[pt]
}



for (i in 1:nc) {
  arimaf.ts9 = ts(m_h9[1:173,i], frequency=12, start=c(2004, 2), end=c(2018, 6))
    PE.ARIMA=arimaf.ts9-test_h9
    PE.RW=RWf_h9.ts-test_h9
    m[i,13]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,14]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=9, power=pw)$p.value
    m[i,15]=hit.ratio(test_h9, arimaf.ts9)
    m[i,16]=eriksPT_test(test_h9, arimaf.ts9)[pt]
  }

for (i in 1:nc) {
  arimaf.ts = ts(m_h12[1:170,i], frequency=12, start=c(2004, 5), end=c(2018, 6))
    PE.ARIMA=arimaf.ts-test_h12
    PE.RW=RWf_h12.ts-test_h12
    m[i,17]=mean((PE.ARIMA)^2)/mean((PE.RW)^2)
    m[i,18]=dm.test(PE.ARIMA, PE.RW, alternative = "less", h=12, power=pw)$p.value
    m[i,19]=hit.ratio(test_h12, arimaf.ts)
    m[i,20]=eriksPT_test(test_h12, arimaf.ts)[pt]
  }

colnames(m) = c("h=1","pv","SR","pv","h=3","pv","SR","pv","h=6","pv","SR","pv","h=9","pv","SR","pv","h=12","pv","SR","pv")
#p = c(1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,1,2,12,24,12,24,1,2,12,24)
#m = cbind(p, m)

m.df = as.data.frame(m, row.names = c("AR(1)", "AR(2)", "AR(12)", "AR(24)", "ARMA(1,1)", "ARMA(2,1)", "ARMA(2,2)" ,
                                      "ARIMA(1,1,0)", "ARIMA(2,1,1)" , "ARIMA(11,1,0)", "ARIMA(23,1,0)", 
                                      "ARIMA(0,1,1)"))
m.df =round(m.df, digits = 3)
grid.table(m.df)


m #fraction of random walk - should be <1 to beat the RW
