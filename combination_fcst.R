train <- window(auscafe, end=c(2012,9))
h <- length(auscafe) - length(train)
ETS <- forecast(ets(train), h=h)
ETS[["mean"]]

ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),
                  h=h)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5


VAR.ts2 <- ts(data.xts[-1,c("Hamilton","dOI","lRO","OP")],frequency=12,start=c(1973, 2), end=c(2018, 6))
VAR.ts <- ts(data.xts[-1,c("dOI","lRO")],frequency=12,start=c(1973, 2), end=c(2018, 6))
train = window(VAR.ts, end=c(2003,5))
test = window(VAR.ts[,"lRO"], start=c(2003,6))
RW = VAR.ts[-c(1:363), "lRO"]
RW = ts(RW[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))

rec=m_h1[1:181,2]
mean((rec-test)^2)/mean((RW-test)^2)

VARf = VAR(train, p=12)

f=forecast(VARf, h=1)
f=forecast(model, h=1)
f$forecast$lRO$mean

predict(VARf, n.ahead=h)$fcst$lRO[,"fcst"]

mean((f-test)^2)/mean((RW-test)^2)

RO = data.xts$lRO["1973-02/2018-06"]
RO.ts <- ts(RO,frequency=12,start=c(1973, 2), end=c(2018, 6))
train=window(RO.ts, end=c(2003,5))


nc=5
x=4
h= nrow(VAR.ts) - nrow(train)
m_comb=matrix(nrow=182, ncol=nc)

for (j in 1:nc){
  for (i in 0:181) {
    train = RO.ts[1:(364+i), ]
    train2 = VAR.ts[1:(364+i), ]
    
    if (j==1){model = Arima(train, order=c(2,0,0))}
    if (j==2){model = auto.arima(train, lambda=0, biasadj=TRUE)}
    if (j==3){model = tbats(train)}
    if (j==4){model = ets(train)}
    if (j==5){model = nnetar(train)}
    
    m_comb[i+1,j]=forecast(model, h=1)$mean
  }
}

m=matrix(nrow=182, ncol=3)

for (i in 0:181) {
  train = VAR.ts[1:(364+i), ]
  train2 = VAR.ts2[1:(364+i), ]
  train3 = RO.ts[1:(364+i), ]
  m[i+1,1]=predict(VAR(train2, p=12), n.ahead=1)$fcst$lRO[,"fcst"]
  m[i+1,2]=predict(VAR(train, p=12), n.ahead=1)$fcst$lRO[,"fcst"]
  m[i+1,3]=meanf(train3, h=1)$mean
}

m_comb=cbind(m_comb[,1:5],m[1:181,1:3])


train = window(VAR.ts, end=c(2003,5))
meanf(train)

as.vector(test)
m_comb=m_comb[1:181,1:5]

cbind(m_comb,as.vector(test))

mean((m_comb[,1]-test)^2)/mean((RW-test)^2)
mean((m_comb[,2]-test)^2)/mean((RW-test)^2)
mean((m_comb[,3]-test)^2)/mean((RW-test)^2)
mean((m_comb[,4]-test)^2)/mean((RW-test)^2)
mean((m_comb[,5]-test)^2)/mean((RW-test)^2)
mean((m_comb[,6]-test)^2)/mean((RW-test)^2)
mean((m_comb[,7]-test)^2)/mean((RW-test)^2)
mean((m_comb[,8]-test)^2)/mean((RW-test)^2)

rec=m_h1[1:181,11]

combi=(m_comb[,1]+m_comb[,2]+m_comb[,3]+m_comb[,4]+m_comb[,5]+m_comb[,6]+m_comb[,7])/7 
combi=(m_comb[,1]+m_comb[,2]+m_comb[,3]+m_comb[,4]+m_comb[,5]+m_comb[,7])/6
combi=(m_comb[,1]+m_comb[,2]+m_comb[,3]+m_comb[,5]+m_comb[,6])/5
combi=(m_comb[,1]+m_comb[,3]+m_comb[,5]+m_comb[,7])/4
combi=(m_comb[,1]+m_comb[,3]+m_comb[,5]+m_comb[,6]+m_comb[,7])/5
combi=(m_comb[,1]+m_comb[,7])/2
mean((combi-test)^2)/mean((RW-test)^2)

c(ar2= accuracy(m_comb[,1], test)["Test set","RMSE"],
  auto = accuracy(m_comb[,2], test)["Test set","RMSE"],
  tbats = accuracy(m_comb[,3], test)["Test set","RMSE"],
  ets = accuracy(m_comb[,4], test)["Test set","RMSE"],
  nnetar = accuracy(m_comb[,5], test)["Test set","RMSE"],
  var = accuracy(m_comb[,7], test)["Test set","RMSE"],
  Combination = accuracy(combi, test)["Test set","RMSE"])


