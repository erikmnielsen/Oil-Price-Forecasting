library(readxl)

Futures <- read_excel("~/R_Saves/Futures.xlsx", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))

data <- read_excel("Variable.xlsx")
data <- as.data.frame(data)
data$RO <- (data$WTI/(data$`CPI US`/100))
data$lRO <- log(data$RO)
data$OP <- log(data$`World oil prod`) - lag.xts(log(data$`World oil prod`),k = 1) 
data$OI <- (data$`OECD Pet inv`/data$`US Pet inv`)*data$`US crude inv`
data$dOI <- diff.xts(data$OI,differences = 1)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"
datats <- ts(data[-c(1:227),], start = c(1991,12), frequency = 12)

fut <- ts(Futures[-1], start = c(1991,12), frequency = 12)


# 1-month -----------------------------------------------------------------


PEf<-datats[2:319,"WTI"] - fut[1:318,"CL1"]
MSPEf <- mean(PEf^2) 

#PEft <- datats[2:319,"WTI"] - fut[2:319,1]
#MSPEft <- mean(PEft^2)

RW.fut <- ts(lag(datats[1:318,"WTI"], k = -1))

PE_RWft <- datats[2:319,"WTI"] - RW.fut 
MSPERWft <-  mean(PE_RWft^2) 
#MSPEft/MSPERWft
MSPEf/MSPERWft

futp<-ts(fut[1:318,1])
WTip<-ts(datats[2:319,"WTI"])
DF <- data.frame(futp, WTip, RW.fut)

autoplot(WTip)+
  autolayer(futp)+
  autolayer(RW.fut)


# 3- months ---------------------------------------------------------------
k <- 3
PEf3<-datats[(1+k):319,"WTI"] - fut[1:(319-k),"CL3"]
MSPEf3 <- mean(PEf3^2) 


RW.fut3 <- lag(datats[1:(319-k),"WTI"], k = -3)


PE_RWft3 <- datats[(1+k):319,"WTI"] - RW.fut3 
MSPERWft3 <-  mean(PE_RWft3^2) 
MSPEf3/MSPERWft3

futp3<-ts(fut[1:(319-k),1])
WTip3<-ts(datats[(1+k):319,"WTI"])
RW.fut3_p <- ts(RW.fut3)
DF3 <- data.frame(futp3, WTip3, RW.fut3)

autoplot(WTip3)+
  autolayer(futp3)+
  autolayer(RW.fut3_p)


# 6-months ----------------------------------------------------------------
k <- 6
PEf6<-datats[(1+k):319,"WTI"] - fut[1:(319-k),"CL3"]
MSPEf6 <- mean(PEf6^2) 


RW.fut6 <- lag(datats[1:(319-k),"WTI"], k = -3)


PE_RWft6 <- datats[(1+k):319,"WTI"] - RW.fut6 
MSPERWft6 <-  mean(PE_RWft6^2) 
MSPEf6/MSPERWft6

futp6<-ts(fut[1:(319-k),1])
WTip6<-ts(datats[(1+k):319,"WTI"])
RW.fut6_p <- ts(RW.fut6)
DF6 <- data.frame(futp6, WTip6, RW.fut6)

autoplot(WTip6)+
  autolayer(futp6)+
  autolayer(RW.fut6_p)

# 12-months ---------------------------------------------------------------
k <- 12
PEf12<-datats[(1+k):319,"WTI"] - fut[1:(319-k),"CL3"]
MSPEf12 <- mean(PEf12^2) 


RW.fut12 <- lag(datats[1:(319-k),"WTI"], k = -3)


PE_RWft12 <- datats[(1+k):319,"WTI"] - RW.fut12 
MSPERWft12 <-  mean(PE_RWft12^2) 
MSPEf12/MSPERWft12

futp12<-ts(fut[1:(319-k),1])
WTip12<-ts(datats[(1+k):319,"WTI"])
RW.fut12_p <- ts(RW.fut12)
DF12 <- data.frame(futp12, WTip12, RW.fut12)

autoplot(WTip12)+
  autolayer(futp12)+
  autolayer(RW.fut12_p)


