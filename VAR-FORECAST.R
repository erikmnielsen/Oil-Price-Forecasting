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

# VAR FORECASTS ALL 4 VARIABLES

# Data Preparation --------------------------------------------------------

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

# VAR modeller
{
data.VAR <- data.xts[-1,c("Kilian","dOI","lRO","OP")]
VAR.ts <- ts(data.VAR,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.VAR2 <- data.xts[-1,c("Hamilton","dOI","lRO","OP")]
VAR2.ts <- ts(data.VAR2,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.VAR3 <- data.xts[-1,c("Hamilton","lRO","OP")]
VAR3.ts <- ts(data.VAR3,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.VAR4 <- data.xts[-1,c("Hamilton","lRO")]
VAR4.ts <- ts(data.VAR4,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.VAR5 <- data.xts[-1,c("dOI","lRO")]
VAR5.ts <- ts(data.VAR5,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.VAR6 <- data.xts[-1,c("lRO","OP")]
VAR6.ts <- ts(data.VAR6,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.VAR7 <- data.xts[-1,c("Hamilton","dOI","lRO")]
VAR7.ts <- ts(data.VAR7,frequency=12,start=c(1973, 2), end=c(2018, 6))

data.dVAR <- data.xts[-1,c("Kilian","dOI","l_diff_RO","OP")] 
dVAR.ts <- ts(data.dVAR,frequency=12,start=c(1973, 2), end=c(2018, 6))
}

nc=23
x=22
m_h1=matrix(nrow=182, ncol=nc)
m_h3=matrix(nrow=182, ncol=nc)
m_h6=matrix(nrow=182, ncol=nc)
m_h9=matrix(nrow=182, ncol=nc)
m_h12=matrix(nrow=182, ncol=nc)

for (j in 1:nc){
  for (i in 0:181) {
    train = VAR.ts[1:(364+i), ]
    train2 = VAR2.ts[1:(364+i), ]
    train3 = VAR3.ts[1:(364+i), ]
    train4 = VAR4.ts[1:(364+i), ]
    train5 = VAR5.ts[1:(364+i), ]
    train6 = VAR6.ts[1:(364+i), ]
    train7 = VAR7.ts[1:(364+i), ]
    traind = dVAR.ts[1:(364+i), ]
    
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
    
    if (j==19){VARf <- VAR(train7, p=1)}
    if (j==20){VARf <- VAR(train7, p=12)}
    if (j==21){VARf <- VAR(train7, p=24)}
    
    if (j==22){VARf <- VAR(traind, p=12)}
    if (j==23){VARf <- VAR(traind, p=24)}
    
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

m=matrix(nrow=nc, ncol=10)

for (i in 1:nc) {
  VARf.ts = ts(m_h1[1:181,i], frequency=12, start=c(2003, 6), end=c(2018, 6))
  if(i<x){
    m[i,1]=mean((VARf.ts-test_h1)^2)/mean((RWf_h1.ts-test_h1)^2) 
    m[i,2]=hit.ratio(y=test_h1,y.hat=VARf.ts,d=FALSE)
  } else{
    m[i,1]=mean((VARf.ts-test_dh1)^2)/mean((dRWf_h1.ts-test_dh1)^2)
    m[i,2]=hit.ratio(y=test_dh1,y.hat=VARf.ts,d=FALSE)
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h3[1:179,i], frequency=12, start=c(2003, 8), end=c(2018, 6))
  if(i<x){
    m[i,3]=mean((VARf.ts-test_h3)^2)/mean((RWf_h3.ts-test_h3)^2)
    m[i,4]=hit.ratio(y=test_h3,y.hat=VARf.ts,d=FALSE) 
  } else{
    m[i,3]=mean((VARf.ts-test_dh3)^2)/mean((dRWf_h3.ts-test_dh3)^2)
    m[i,4]=hit.ratio(y=test_dh3,y.hat=VARf.ts,d=FALSE)
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h6[1:177,i], frequency=12, start=c(2003, 10), end=c(2018, 6))
  if(i<x){
    m[i,5]=mean((VARf.ts-test_h6)^2)/mean((RWf_h6.ts-test_h6)^2)
    m[i,6]=hit.ratio(y=test_h6,y.hat=VARf.ts,d=FALSE)
  } else{
    m[i,5]=mean((VARf.ts-test_dh6)^2)/mean((dRWf_h6.ts-test_dh6)^2)
    m[i,6]=hit.ratio(y=test_dh6,y.hat=VARf.ts,d=FALSE)
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h9[1:175,i], frequency=12, start=c(2003, 12), end=c(2018, 6))
  if(i<x){
    m[i,7]=mean((VARf.ts-test_h9)^2)/mean((RWf_h9.ts-test_h9)^2)
    m[i,8]=hit.ratio(y=test_h9,y.hat=VARf.ts,d=FALSE)
  } else{
    m[i,7]=mean((VARf.ts-test_dh9)^2)/mean((dRWf_h9.ts-test_dh9)^2)
    m[i,8]=hit.ratio(y=test_dh9,y.hat=VARf.ts,d=FALSE)
  }
}
for (i in 1:nc) {
  VARf.ts = ts(m_h12[1:173,i], frequency=12, start=c(2004, 2), end=c(2018, 6))
  if(i<x){
    m[i,9]=mean((VARf.ts-test_h12)^2)/mean((RWf_h12.ts-test_h12)^2)
    m[i,10]=hit.ratio(y=test_h12,y.hat=VARf.ts,d=FALSE) 
  } else{
    m[i,9]=mean((VARf.ts-test_dh12)^2)/mean((dRWf_h12.ts-test_dh12)^2)
    m[i,10]=hit.ratio(y=test_dh12,y.hat=VARf.ts,d=FALSE)
  }
}

colnames(m) = c("h=1","SR","h=3","SR","h=6","SR","h=9","SR","h=12","SR")
p = c(1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,1,12,24,1,12)
m = cbind(p, m)

m.df = as.data.frame(m, row.names = c("Killian1","Killian2","Killian3",
                                        "Hamilton1","Hamilton2","Hamilton3",
                                        "÷ Inv1", "÷ Inv2", "÷ Inv3",
                                        "÷ Inv & Prod1", "÷ Inv & Prod2", "÷ Inv & Prod3",
                                        "÷ Act & Prod1", "÷ Act & Prod2", "÷ Act & Prod3",
                                        "÷ Act & Inv1", "÷ Act & Inv2", "÷ Act & Inv3",
                                        "÷ Prod1", "÷ Prod2", "÷ Prod3",
                                        "Diff1", "Diff2"), 
                     colnames(1,2,3,4,5,6,7,8,9,10))
m.df =round(m.df, digits = 3)
grid.table(m.df)
m.df

#autoplot(subset(VAR.ts[, "lRO"], end = 545)) +
autoplot(test) + #evt: series = "Real Price"
  autolayer(VARf.ts, series="VARf") +
  autolayer(RWf.ts, series="RWf") +
  xlab("Time") + ylab("Log Price") +
  ggtitle("Forecasting real price of WTI") +
  guides(colour=guide_legend(title="Forecasts:")) +
  theme_economist() +
  scale_color_economist()


