rm(list=ls(all=T))

library(xts)
library(fpp2)
library(vars)
library(readxl)
library(urca)
library(forecast)
library(ggthemes)
library(fDMA)
library(gridExtra)
library(grid)
library(rugarch)
library(quantmod)
library(tidyr)

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

nwPT_test=function(Actual,Forecast){
  
  yt=Actual #assign actual to yt to make code shorter  
  xt=Forecast #assign forecast to xt...  
  
  delta_yt=as.matrix(cbind(ifelse(yt-Lag(yt)>0,1,0)[-1])) #calc change of yt (delta)
  delta_xt=as.matrix(cbind(ifelse(xt-Lag(xt)>0,1,0)[-1])) #calc change of xt(delta)
  nT=length(delta_yt) #number of Time periods
  Yt=cbind(delta_yt[-1]) #Yt=(y2,...,yT) 
  Xt=cbind(delta_xt[-1]) #Xt=(x2,...,xT)
  Yt2=as.vector(rbind(delta_yt[-nT])) #Yt2=(y1,...,yT-1)
  Xt2=as.vector(rbind(delta_xt[-nT])) #Xt2=(x1,...,xT-1)
  teta=rep(1,nT-1) #T-1 vector of ones
  I=diag(nT-1) #Identity matrix
  W=cbind(teta,Yt2,Xt2) #W matrix as in formula
  
  Mw=I-(W%*%((t(W)%*%W)^(-1))%*%t(W))#calcualting Mw as in formula
  
  #calculating elements S as in formula
  Syy.w=((nT-1)^(-1))*t(Yt)%*%Mw%*%Yt
  Sxx.w=((nT-1)^(-1))*t(Xt)%*%Mw%*%Xt
  Sxy.w=((nT-1)^(-1))*t(Xt)%*%Mw%*%Yt
  Syx.w=((nT-1)^(-1))*t(Yt)%*%Mw%*%Xt
  
  PT=(nT-1)*(Syy.w^(-1)*Syx.w*Sxx.w^(-1)*Sxy.w)#finally calculating PT 
  
  p.value=1-pchisq(PT,df=1)#calculating p-value
  #some code to make it looks nicer
  summary=c(PT,p.value)
  names(summary)=c("PT statistic","p.value")
  summary
} #bruges ikke



# Data Preparation --------------------------------------------------------

data <- read_excel("VariableHam74.xlsx")
data <- as.data.frame(data)
#data$CPI <- log(data$`CPI US`) - lag.xts(log(data$`CPI US`),k = 1)
#data$CPI_d <- diff.xts(data$lRO, differences = 1)
data$RO <- (data$WTI/(data$`CPI US`/100))
data$lRO <- log(data$RO)
data$l_diff_RO <- diff.xts(data$lRO, differences = 1)
data$OP <- log(data$`World oil prod`) - lag.xts(log(data$`World oil prod`),k = 1) 
data$OI <- (data$`OECD Pet inv`/data$`US Pet inv`)*data$`US crude inv`
data$dOI <- diff.xts(data$OI,differences = 1)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"

#data$l_prod <- log(data$`World oil prod`)
#data$l_d_prod <- diff.xts(data$l_prod,differences = 1)
#data$OP <- diff.xts(data$`World oil prod`,differences = 1)
#data$l_prod100 <- 100*(data$l_prod)
#data$OP <- diff.xts(data$l_prod100,differences = 1)

data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["1974-01/2018-06"]

# VAR modeller

nc=20
x=19

autoplot(VAR.ts)
subset(VAR.ts[, "lRO"],start=1,end = 364)

#Killian
{
  m_h1=matrix(nrow=182, ncol=nc)
  m_h3=matrix(nrow=182, ncol=nc)
  m_h6=matrix(nrow=182, ncol=nc)
  m_h9=matrix(nrow=182, ncol=nc)
  m_h12=matrix(nrow=182, ncol=nc)
  
  VAR.ts <- ts(data.xts[-1,c("Kilian","dOI","lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR2.ts <- ts(data.xts[-1,c("Kilian","lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  VAR3.ts <- ts(data.xts[-1,c("Kilian","lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  VAR4.ts <- ts(data.xts[-1,c("dOI","lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR5.ts <- ts(data.xts[-1,c("lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR6.ts <- ts(data.xts[-1,c("Kilian", "dOI", "lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  dVAR.ts <- ts(data.xts[-1,c("Kilian","dOI","l_diff_RO","OP")] ,frequency=12,start=c(1974, 2), end=c(2018, 6))
}
for (j in 1:nc){
  for (i in 0:181) {
    train = VAR.ts[1:(364-12+i), ]
    train2 = VAR2.ts[1:(364-12+i), ]
    train3 = VAR3.ts[1:(364-12+i), ]
    train4 = VAR4.ts[1:(364-12+i), ]
    train5 = VAR5.ts[1:(364-12+i), ]
    train6 = VAR6.ts[1:(364-12+i), ]
    traind = dVAR.ts[1:(364-12+i), ]
    
    if (j==1){VARf <- VAR(train, p=1)}
    if (j==2){VARf <- VAR(train, p=12)}
    if (j==3){VARf <- VAR(train, p=24)}
    
    if (j==4){VARf <- VAR(train2, p=1)}
    if (j==5){VARf <- VAR(train2, p=12)}
    if (j==6){VARf <- VAR(train2, p=24)}
    
    if (j==7){VARf <- VAR(train3, p=1)}
    if (j==8){VARf <- VAR(train3, p=12)}
    if (j==9){VARf <- VAR(train3, p=24)}
    
    if (j==10){VARf <- VAR(train4, p=1)}
    if (j==11){VARf <- VAR(train4, p=12)}
    if (j==12){VARf <- VAR(train4, p=24)}
    
    if (j==13){VARf <- VAR(train5, p=1)}
    if (j==14){VARf <- VAR(train5, p=12)}
    if (j==15){VARf <- VAR(train5, p=24)}
    
    if (j==16){VARf <- VAR(train6, p=1)}
    if (j==17){VARf <- VAR(train6, p=12)}
    if (j==18){VARf <- VAR(train6, p=24)}
    
    if (j==19){VARf <- VAR(traind, p=12)}
    if (j==20){VARf <- VAR(traind, p=24)}
    
    if (j<x){m_h1[i+1,j]=predict(VARf, n.ahead=1)$fcst$lRO[,"fcst"]}
    if (j<x){m_h3[i+1,j]=predict(VARf, n.ahead=3)$fcst$lRO[3,"fcst"]}
    if (j<x){m_h6[i+1,j]=predict(VARf, n.ahead=6)$fcst$lRO[6,"fcst"]}
    if (j<x){m_h9[i+1,j]=predict(VARf, n.ahead=9)$fcst$lRO[9,"fcst"]}
    if (j<x){m_h12[i+1,j]=predict(VARf, n.ahead=12)$fcst$lRO[12,"fcst"]}
    
    if (j>x-1){m_h1[i+1,j]=predict(VARf, n.ahead=1)$fcst$l_diff_RO[,"fcst"]}
    if (j>x-1){m_h3[i+1,j]=predict(VARf, n.ahead=3)$fcst$l_diff_RO[3,"fcst"]}
    if (j>x-1){m_h6[i+1,j]=predict(VARf, n.ahead=6)$fcst$l_diff_RO[6,"fcst"]}
    if (j>x-1){m_h9[i+1,j]=predict(VARf, n.ahead=9)$fcst$l_diff_RO[9,"fcst"]}
    if (j>x-1){m_h12[i+1,j]=predict(VARf, n.ahead=12)$fcst$l_diff_RO[12,"fcst"]}
  }
} 

#Hamilton
{
  mH_h1=matrix(nrow=182, ncol=nc)
  mH_h3=matrix(nrow=182, ncol=nc)
  mH_h6=matrix(nrow=182, ncol=nc)
  mH_h9=matrix(nrow=182, ncol=nc)
  mH_h12=matrix(nrow=182, ncol=nc)
  
  VAR.ts <- ts(data.xts[-1,c("Hamilton","dOI","lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR2.ts <- ts(data.xts[-1,c("Hamilton","lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  VAR3.ts <- ts(data.xts[-1,c("Hamilton","lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  VAR4.ts <- ts(data.xts[-1,c("dOI","lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR5.ts <- ts(data.xts[-1,c("lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR6.ts <- ts(data.xts[-1,c("Hamilton", "dOI", "lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  dVAR.ts <- ts(data.xts[-1,c("Hamilton","dOI","l_diff_RO","OP")] ,frequency=12,start=c(1974, 2), end=c(2018, 6))
}
for (j in 1:nc){
  for (i in 0:181) {
    train = VAR.ts[1:(364-12+i), ]
    train2 = VAR2.ts[1:(364-12+i), ]
    train3 = VAR3.ts[1:(364-12+i), ]
    train4 = VAR4.ts[1:(364-12+i), ]
    train5 = VAR5.ts[1:(364-12+i), ]
    train6 = VAR6.ts[1:(364-12+i), ]
    traind = dVAR.ts[1:(364-12+i), ]
    
    if (j==1){VARf <- VAR(train, p=1)}
    if (j==2){VARf <- VAR(train, p=12)}
    if (j==3){VARf <- VAR(train, p=24)}
    
    if (j==4){VARf <- VAR(train2, p=1)}
    if (j==5){VARf <- VAR(train2, p=12)}
    if (j==6){VARf <- VAR(train2, p=24)}
    
    if (j==7){VARf <- VAR(train3, p=1)}
    if (j==8){VARf <- VAR(train3, p=12)}
    if (j==9){VARf <- VAR(train3, p=24)}
    
    if (j==10){VARf <- VAR(train4, p=1)}
    if (j==11){VARf <- VAR(train4, p=12)}
    if (j==12){VARf <- VAR(train4, p=24)}
    
    if (j==13){VARf <- VAR(train5, p=1)}
    if (j==14){VARf <- VAR(train5, p=12)}
    if (j==15){VARf <- VAR(train5, p=24)}
    
    if (j==16){VARf <- VAR(train6, p=1)}
    if (j==17){VARf <- VAR(train6, p=12)}
    if (j==18){VARf <- VAR(train6, p=24)}
    
    if (j==19){VARf <- VAR(traind, p=12)}
    if (j==20){VARf <- VAR(traind, p=24)}
    
    if (j<x){mH_h1[i+1,j]=predict(VARf, n.ahead=1)$fcst$lRO[,"fcst"]}
    if (j<x){mH_h3[i+1,j]=predict(VARf, n.ahead=3)$fcst$lRO[3,"fcst"]}
    if (j<x){mH_h6[i+1,j]=predict(VARf, n.ahead=6)$fcst$lRO[6,"fcst"]}
    if (j<x){mH_h9[i+1,j]=predict(VARf, n.ahead=9)$fcst$lRO[9,"fcst"]}
    if (j<x){mH_h12[i+1,j]=predict(VARf, n.ahead=12)$fcst$lRO[12,"fcst"]}
    
    if (j>x-1){mH_h1[i+1,j]=predict(VARf, n.ahead=1)$fcst$l_diff_RO[,"fcst"]}
    if (j>x-1){mH_h3[i+1,j]=predict(VARf, n.ahead=3)$fcst$l_diff_RO[3,"fcst"]}
    if (j>x-1){mH_h6[i+1,j]=predict(VARf, n.ahead=6)$fcst$l_diff_RO[6,"fcst"]}
    if (j>x-1){mH_h9[i+1,j]=predict(VARf, n.ahead=9)$fcst$l_diff_RO[9,"fcst"]}
    if (j>x-1){mH_h12[i+1,j]=predict(VARf, n.ahead=12)$fcst$l_diff_RO[12,"fcst"]}
  }
} 

#Hamilton OECD
{
  mO_h1=matrix(nrow=182, ncol=nc)
  mO_h3=matrix(nrow=182, ncol=nc)
  mO_h6=matrix(nrow=182, ncol=nc)
  mO_h9=matrix(nrow=182, ncol=nc)
  mO_h12=matrix(nrow=182, ncol=nc)
  
  VAR.ts <- ts(data.xts[-1,c("HamiltonOECD","dOI","lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR2.ts <- ts(data.xts[-1,c("HamiltonOECD","lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  VAR3.ts <- ts(data.xts[-1,c("HamiltonOECD","lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  VAR4.ts <- ts(data.xts[-1,c("dOI","lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR5.ts <- ts(data.xts[-1,c("lRO","OP")],frequency=12,start=c(1974, 2), end=c(2018, 6))
  VAR6.ts <- ts(data.xts[-1,c("HamiltonOECD", "dOI", "lRO")],frequency=12,start=c(1974, 2), end=c(2018, 6)) 
  dVAR.ts <- ts(data.xts[-1,c("HamiltonOECD","dOI","l_diff_RO","OP")] ,frequency=12,start=c(1974, 2), end=c(2018, 6))
}
for (j in 1:nc){
  for (i in 0:181) {
    train = VAR.ts[1:(364-12+i), ]
    train2 = VAR2.ts[1:(364-12+i), ]
    train3 = VAR3.ts[1:(364-12+i), ]
    train4 = VAR4.ts[1:(364-12+i), ]
    train5 = VAR5.ts[1:(364-12+i), ]
    train6 = VAR6.ts[1:(364-12+i), ]
    traind = dVAR.ts[1:(364-12+i), ]
    
    if (j==1){VARf <- VAR(train, p=1)}
    if (j==2){VARf <- VAR(train, p=12)}
    if (j==3){VARf <- VAR(train, p=24)}
    
    if (j==4){VARf <- VAR(train2, p=1)}
    if (j==5){VARf <- VAR(train2, p=12)}
    if (j==6){VARf <- VAR(train2, p=24)}
    
    if (j==7){VARf <- VAR(train3, p=1)}
    if (j==8){VARf <- VAR(train3, p=12)}
    if (j==9){VARf <- VAR(train3, p=24)}
    
    if (j==10){VARf <- VAR(train4, p=1)}
    if (j==11){VARf <- VAR(train4, p=12)}
    if (j==12){VARf <- VAR(train4, p=24)}
    
    if (j==13){VARf <- VAR(train5, p=1)}
    if (j==14){VARf <- VAR(train5, p=12)}
    if (j==15){VARf <- VAR(train5, p=24)}
    
    if (j==16){VARf <- VAR(train6, p=1)}
    if (j==17){VARf <- VAR(train6, p=12)}
    if (j==18){VARf <- VAR(train6, p=24)}
    
    if (j==19){VARf <- VAR(traind, p=12)}
    if (j==20){VARf <- VAR(traind, p=24)}
    
    if (j<x){mO_h1[i+1,j]=predict(VARf, n.ahead=1)$fcst$lRO[,"fcst"]}
    if (j<x){mO_h3[i+1,j]=predict(VARf, n.ahead=3)$fcst$lRO[3,"fcst"]}
    if (j<x){mO_h6[i+1,j]=predict(VARf, n.ahead=6)$fcst$lRO[6,"fcst"]}
    if (j<x){mO_h9[i+1,j]=predict(VARf, n.ahead=9)$fcst$lRO[9,"fcst"]}
    if (j<x){mO_h12[i+1,j]=predict(VARf, n.ahead=12)$fcst$lRO[12,"fcst"]}
    
    if (j>x-1){mO_h1[i+1,j]=predict(VARf, n.ahead=1)$fcst$l_diff_RO[,"fcst"]}
    if (j>x-1){mO_h3[i+1,j]=predict(VARf, n.ahead=3)$fcst$l_diff_RO[3,"fcst"]}
    if (j>x-1){mO_h6[i+1,j]=predict(VARf, n.ahead=6)$fcst$l_diff_RO[6,"fcst"]}
    if (j>x-1){mO_h9[i+1,j]=predict(VARf, n.ahead=9)$fcst$l_diff_RO[9,"fcst"]}
    if (j>x-1){mO_h12[i+1,j]=predict(VARf, n.ahead=12)$fcst$l_diff_RO[12,"fcst"]}
  }
} 

# Forecast Evaluation -----------------------------------------------------------------------
{
  test_h1 = subset(VAR.ts[, "lRO"],start=365-12,end = 545-12)
  test_dh1 = subset(dVAR.ts[, "l_diff_RO"],start=365-12,end = 545-12)
  test_h3 = subset(VAR.ts[, "lRO"],start=367-12,end = 545-12)
  test_dh3 = subset(dVAR.ts[, "l_diff_RO"],start=367-12,end = 545-12)
  test_h6 = subset(VAR.ts[, "lRO"],start=370-12,end = 545-12)
  test_dh6 = subset(dVAR.ts[, "l_diff_RO"],start=370-12,end = 545-12)
  test_h9 = subset(VAR.ts[, "lRO"],start=373-12,end = 545-12)
  test_dh9 = subset(dVAR.ts[, "l_diff_RO"],start=373-12,end = 545-12)
  test_h12 = subset(VAR.ts[, "lRO"],start=376-12,end = 545-12)
  test_dh12 = subset(dVAR.ts[, "l_diff_RO"],start=376-12,end = 545-12)
  
  RW = VAR.ts[-c(1:351), "lRO"]
  
  RWf_h1.ts = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
  RWf_h3.ts = ts(RW[1:179], frequency=12, start=c(2003, 8), end=c(2018, 6))
  RWf_h6.ts = ts(RW[1:176], frequency=12, start=c(2003, 11), end=c(2018, 6))
  RWf_h9.ts = ts(RW[1:173], frequency=12, start=c(2004, 2), end=c(2018, 6))
  RWf_h12.ts = ts(RW[1:170], frequency=12, start=c(2004, 5), end=c(2018, 6))
}

m=matrix(nrow=nc, ncol=20)
pw=2
pt=2


for (i in 1:nc) {
  VARf.ts = ts(m_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h1
    PE.RW=RWf_h1.ts-test_h1
    m[i,1]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m[i,2]=dm.test(PE.VAR, PE.RW, alternative = "less", h=1, power=pw)$p.value
    m[i,3]=MDirAcc(test_h1, VARf.ts)
    m[i,4]=eriksPT_test(test_h1, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh1
    PE.dRW=test_dh1
    m[i,1]=mean((PE.dVAR)^2)/mean((PE.dRW)^2)
    m[i,2]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=1, power=pw)$p.value
    m[i,3]=MDirAcc(test_dh1, VARf.ts)
    m[i,4]=eriksPT_test(test_dh1, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h3
    PE.RW=RWf_h3.ts-test_h3
    m[i,5]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m[i,6]=dm.test(PE.VAR, PE.RW, alternative = "less", h=3, power=pw)$p.value
    m[i,7]=MDirAcc(test_h3, VARf.ts)
    m[i,8]=eriksPT_test(test_h3, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh3
    PE.dRW=test_dh3
    m[i,5]=mean((PE.dVAR)^2)/mean((PE.dRW)^2)
    m[i,6]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=3, power=pw)$p.value
    m[i,7]=MDirAcc(test_dh3, VARf.ts)
    m[i,8]=eriksPT_test(test_dh3, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h6[1:176,i], frequency=12, start=c(2003, 11), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h6
    PE.RW=RWf_h6.ts-test_h6
    m[i,9]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m[i,10]=dm.test(PE.VAR, PE.RW, alternative = "less", h=6, power=pw)$p.value
    m[i,11]=MDirAcc(test_h6, VARf.ts)
    m[i,12]=eriksPT_test(test_h6, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh6
    PE.dRW=test_dh6
    m[i,9]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m[i,10]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=6, power=pw)$p.value
    m[i,11]=MDirAcc(test_dh6, VARf.ts)
    m[i,12]=eriksPT_test(test_dh6, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h9[1:173,i], frequency=12, start=c(2004, 2), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h9
    PE.RW=RWf_h9.ts-test_h9
    m[i,13]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m[i,14]=dm.test(PE.VAR, PE.RW, alternative = "less", h=9, power=pw)$p.value
    m[i,15]=MDirAcc(test_h9, VARf.ts)
    m[i,16]=eriksPT_test(test_h9, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh9
    PE.dRW=test_dh9
    m[i,13]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m[i,14]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=9, power=pw)$p.value
    m[i,15]=MDirAcc(test_dh9, VARf.ts)
    m[i,16]=eriksPT_test(test_dh9, VARf.ts)[2]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h12[1:170,i], frequency=12, start=c(2004, 5), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h12
    PE.RW=RWf_h12.ts-test_h12
    m[i,17]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m[i,18]=dm.test(PE.VAR, PE.RW, alternative = "less", h=12, power=pw)$p.value
    m[i,19]=MDirAcc(test_h12, VARf.ts)
    m[i,20]=eriksPT_test(test_h12, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh12
    PE.dRW=test_dh12
    m[i,17]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m[i,18]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=12, power=pw)$p.value
    m[i,19]=MDirAcc(test_dh12, VARf.ts)
    m[i,20]=eriksPT_test(test_dh12, VARf.ts)[pt]
  }
}


colnames(m) = c("h=1","pv","SR","pv","h=3","pv","SR","pv","h=6","pv","SR","pv","h=9","pv","SR","pv","h=12","pv","SR","pv")
p = c(1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,12,24)
m = cbind(p, m)

m.df = as.data.frame(m, row.names = c("Killian1","Killian2","Killian3",
                                      "÷ Inv1", "÷ Inv2", "÷ Inv3",
                                      "÷ Inv & Prod1", "÷ Inv & Prod2", "÷ Inv & Prod3",
                                      "÷ Act & Prod1", "÷ Act & Prod2", "÷ Act & Prod3",
                                      "÷ Act & Inv1", "÷ Act & Inv2", "÷ Act & Inv3",
                                      "÷ Prod1", "÷ Prod2", "÷ Prod3",
                                      "Diff1", "Diff2"))
m.df =round(m.df, digits = 3)
#grid.table(m.df)
m.df


#HAMILTON
m2=matrix(nrow=nc, ncol=20)
pw=2
pt=2


for (i in 1:nc) {
  VARf.ts = ts(mH_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h1
    PE.RW=RWf_h1.ts-test_h1
    m2[i,1]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m2[i,2]=dm.test(PE.VAR, PE.RW, alternative = "less", h=1, power=pw)$p.value
    m2[i,3]=MDirAcc(test_h1, VARf.ts)
    m2[i,4]=eriksPT_test(test_h1, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh1
    PE.dRW=test_dh1
    m2[i,1]=mean((PE.dVAR)^2)/mean((PE.dRW)^2)
    m2[i,2]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=1, power=pw)$p.value
    m2[i,3]=MDirAcc(test_dh1, VARf.ts)
    m2[i,4]=eriksPT_test(test_dh1, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mH_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h3
    PE.RW=RWf_h3.ts-test_h3
    m2[i,5]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m2[i,6]=dm.test(PE.VAR, PE.RW, alternative = "less", h=3, power=pw)$p.value
    m2[i,7]=MDirAcc(test_h3, VARf.ts)
    m2[i,8]=eriksPT_test(test_h3, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh3
    PE.dRW=test_dh3
    m2[i,5]=mean((PE.dVAR)^2)/mean((PE.dRW)^2)
    m2[i,6]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=3, power=pw)$p.value
    m2[i,7]=MDirAcc(test_dh3, VARf.ts)
    m2[i,8]=eriksPT_test(test_dh3, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mH_h6[1:176,i], frequency=12, start=c(2003, 11), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h6
    PE.RW=RWf_h6.ts-test_h6
    m2[i,9]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m2[i,10]=dm.test(PE.VAR, PE.RW, alternative = "less", h=6, power=pw)$p.value
    m2[i,11]=MDirAcc(test_h6, VARf.ts)
    m2[i,12]=eriksPT_test(test_h6, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh6
    PE.dRW=test_dh6
    m2[i,9]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m2[i,10]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=6, power=pw)$p.value
    m2[i,11]=MDirAcc(test_dh6, VARf.ts)
    m2[i,12]=eriksPT_test(test_dh6, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mH_h9[1:173,i], frequency=12, start=c(2004, 2), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h9
    PE.RW=RWf_h9.ts-test_h9
    m2[i,13]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m2[i,14]=dm.test(PE.VAR, PE.RW, alternative = "less", h=9, power=pw)$p.value
    m2[i,15]=MDirAcc(test_h9, VARf.ts)
    m2[i,16]=eriksPT_test(test_h9, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh9
    PE.dRW=test_dh9
    m2[i,13]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m2[i,14]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=9, power=pw)$p.value
    m2[i,15]=MDirAcc(test_dh9, VARf.ts)
    m2[i,16]=eriksPT_test(test_dh9, VARf.ts)[2]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mH_h12[1:170,i], frequency=12, start=c(2004, 5), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h12
    PE.RW=RWf_h12.ts-test_h12
    m2[i,17]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m2[i,18]=dm.test(PE.VAR, PE.RW, alternative = "less", h=12, power=pw)$p.value
    m2[i,19]=MDirAcc(test_h12, VARf.ts)
    m2[i,20]=eriksPT_test(test_h12, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh12
    PE.dRW=test_dh12
    m2[i,17]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m2[i,18]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=12, power=pw)$p.value
    m2[i,19]=MDirAcc(test_dh12, VARf.ts)
    m2[i,20]=eriksPT_test(test_dh12, VARf.ts)[pt]
  }
}


colnames(m2) = c("h=1","pv","SR","pv","h=3","pv","SR","pv","h=6","pv","SR","pv","h=9","pv","SR","pv","h=12","pv","SR","pv")
p = c(1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,12,24)
m2 = cbind(p, m2)

m2.df = as.data.frame(m2, row.names = c("Hamilton1","Hamilton2","Hamilton3",
                                      "÷ Inv1", "÷ Inv2", "÷ Inv3",
                                      "÷ Inv & Prod1", "÷ Inv & Prod2", "÷ Inv & Prod3",
                                      "÷ Act & Prod1", "÷ Act & Prod2", "÷ Act & Prod3",
                                      "÷ Act & Inv1", "÷ Act & Inv2", "÷ Act & Inv3",
                                      "÷ Prod1", "÷ Prod2", "÷ Prod3",
                                      "Diff1", "Diff2"))
m2.df =round(m2.df, digits = 3)
#grid.table(m2.df)
m2.df



#HAMILTON OECD
m3=matrix(nrow=nc, ncol=20)
pw=2
pt=2


for (i in 1:nc) {
  VARf.ts = ts(mO_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h1
    PE.RW=RWf_h1.ts-test_h1
    m3[i,1]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m3[i,2]=dm.test(PE.VAR, PE.RW, alternative = "less", h=1, power=pw)$p.value
    m3[i,3]=MDirAcc(test_h1, VARf.ts)
    m3[i,4]=eriksPT_test(test_h1, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh1
    PE.dRW=test_dh1
    m3[i,1]=mean((PE.dVAR)^2)/mean((PE.dRW)^2)
    m3[i,2]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=1, power=pw)$p.value
    m3[i,3]=MDirAcc(test_dh1, VARf.ts)
    m3[i,4]=eriksPT_test(test_dh1, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mO_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h3
    PE.RW=RWf_h3.ts-test_h3
    m3[i,5]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m3[i,6]=dm.test(PE.VAR, PE.RW, alternative = "less", h=3, power=pw)$p.value
    m3[i,7]=MDirAcc(test_h3, VARf.ts)
    m3[i,8]=eriksPT_test(test_h3, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh3
    PE.dRW=test_dh3
    m3[i,5]=mean((PE.dVAR)^2)/mean((PE.dRW)^2)
    m3[i,6]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=3, power=pw)$p.value
    m3[i,7]=MDirAcc(test_dh3, VARf.ts)
    m3[i,8]=eriksPT_test(test_dh3, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mO_h6[1:176,i], frequency=12, start=c(2003, 11), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h6
    PE.RW=RWf_h6.ts-test_h6
    m3[i,9]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m3[i,10]=dm.test(PE.VAR, PE.RW, alternative = "less", h=6, power=pw)$p.value
    m3[i,11]=MDirAcc(test_h6, VARf.ts)
    m3[i,12]=eriksPT_test(test_h6, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh6
    PE.dRW=test_dh6
    m3[i,9]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m3[i,10]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=6, power=pw)$p.value
    m3[i,11]=MDirAcc(test_dh6, VARf.ts)
    m3[i,12]=eriksPT_test(test_dh6, VARf.ts)[pt]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mO_h9[1:173,i], frequency=12, start=c(2004, 2), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h9
    PE.RW=RWf_h9.ts-test_h9
    m3[i,13]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m3[i,14]=dm.test(PE.VAR, PE.RW, alternative = "less", h=9, power=pw)$p.value
    m3[i,15]=MDirAcc(test_h9, VARf.ts)
    m3[i,16]=eriksPT_test(test_h9, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh9
    PE.dRW=test_dh9
    m3[i,13]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m3[i,14]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=9, power=pw)$p.value
    m3[i,15]=MDirAcc(test_dh9, VARf.ts)
    m3[i,16]=eriksPT_test(test_dh9, VARf.ts)[2]
  }
}
for (i in 1:nc) {
  VARf.ts = ts(mO_h12[1:170,i], frequency=12, start=c(2004, 5), end=c(2018, 6))
  if(i<x){
    PE.VAR=VARf.ts-test_h12
    PE.RW=RWf_h12.ts-test_h12
    m3[i,17]=mean((PE.VAR)^2)/mean((PE.RW)^2)
    m3[i,18]=dm.test(PE.VAR, PE.RW, alternative = "less", h=12, power=pw)$p.value
    m3[i,19]=MDirAcc(test_h12, VARf.ts)
    m3[i,20]=eriksPT_test(test_h12, VARf.ts)[pt]
  } else{
    PE.dVAR=VARf.ts-test_dh12
    PE.dRW=test_dh12
    m3[i,17]=mean((PE.dVAR^2))/ mean((PE.dRW)^2)
    m3[i,18]=dm.test(PE.dVAR, PE.dRW, alternative = "less", h=12, power=pw)$p.value
    m3[i,19]=MDirAcc(test_dh12, VARf.ts)
    m3[i,20]=eriksPT_test(test_dh12, VARf.ts)[pt]
  }
}


colnames(m3) = c("h=1","pv","SR","pv","h=3","pv","SR","pv","h=6","pv","SR","pv","h=9","pv","SR","pv","h=12","pv","SR","pv")
p = c(1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,12,24)
m3 = cbind(p, m3)

m3.df = as.data.frame(m3, row.names = c("HamiltonOECD1","HamiltonOECD2","HamiltonOECD3",
                                      "÷ Inv1", "÷ Inv2", "÷ Inv3",
                                      "÷ Inv & Prod1", "÷ Inv & Prod2", "÷ Inv & Prod3",
                                      "÷ Act & Prod1", "÷ Act & Prod2", "÷ Act & Prod3",
                                      "÷ Act & Inv1", "÷ Act & Inv2", "÷ Act & Inv3",
                                      "÷ Prod1", "÷ Prod2", "÷ Prod3",
                                      "Diff1", "Diff2"))
m3.df =round(m3.df, digits = 3)
#grid.table(m3.df)
m3.df








#autoplot(subset(VAR.ts[, "lRO"], end = 545)) +
autoplot(test) + #evt: series = "Real Price"
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Forecasts:")) +
  theme_economist() +
  scale_color_economist()



