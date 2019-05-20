library(readxl)

Futures <- read_excel("Futures.xlsx", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))

View(Futures)
data <- read_excel("Variable.xlsx")
data <- as.data.frame(data)
data$RO <- (data$WTI/(data$`CPI US`/100))
data$lRO <- log(data$RO)
data$OP <- log(data$`World oil prod`) - lag.xts(log(data$`World oil prod`),k = 1) 
data$OI <- (data$`OECD Pet inv`/data$`US Pet inv`)*data$`US crude inv`
data$dOI <- diff.xts(data$OI,differences = 1)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"
datats <- ts(data[-c(1:227),], start = c(1991,12), frequency = 12)
datats_test <- ts(data[-c(1:227),], start = c(1991,12), frequency = 12)

fut <- ts(Futures[-1], start = c(1991,12), frequency = 12)
fut_test <- ts(Futures[-1], start = c(1991,12), frequency = 12)

# 1-month -----------------------------------------------------------------


PEf<-datats[2:319,"WTI"] - fut[1:318,"CL1"]
MSPEf <- mean(PEf^2) 

MDirAcc(datats[2:319,"WTI"],fut[1:318,"CL1"])
eriksPT_test(datats[2:319,"WTI"],fut[1:318,"CL1"])

#PEft <- datats[2:319,"WTI"] - fut[2:319,1]
#MSPEft <- mean(PEft^2)

RW.fut <- ts(lag(datats[1:318,"WTI"], k = -1))

PE_RWft <- datats[2:319,"WTI"] - RW.fut 
MSPERWft <-  mean(PE_RWft^2) 
#MSPEft/MSPERWft
MSPEf/MSPERWft

dm.test(PEf,PE_RWft, alternative = "less")

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

MDirAcc(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL3"])
eriksPT_test(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL3"])

RW.fut3 <- lag(datats[1:(319-k),"WTI"], k = -3)

dm.test(PEf3,PE_RWft3, alternative = "less")

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
PEf6<-datats[(1+k):319,"WTI"] - fut[1:(319-k),"CL6"]
MSPEf6 <- mean(PEf6^2) 


RW.fut6 <- lag(datats[1:(319-k),"WTI"], k = -6)

dm.test(PEf6,PE_RWft6, alternative = "less")

PE_RWft6 <- datats[(1+k):319,"WTI"] - RW.fut6 
MSPERWft6 <-  mean(PE_RWft6^2) 
MSPEf6/MSPERWft6

MDirAcc(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL6"])
eriksPT_test(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL6"])

futp6<-ts(fut[1:(319-k),6])
WTip6<-ts(datats[(1+k):319,"WTI"])
RW.fut6_p <- ts(RW.fut6)
DF6 <- data.frame(futp6, WTip6, RW.fut6)

autoplot(WTip6)+
  autolayer(futp6)+
  autolayer(RW.fut6_p)


# 9-months ----------------------------------------------------------------

k <- 9
PEf9<-datats[(1+k):319,"WTI"] - fut[1:(319-k),"CL9"]
MSPEf9 <- mean(PEf9^2) 


RW.fut9 <- lag(datats[1:(319-k),"WTI"], k = -9)

PE_RWft9<- datats[(1+k):319,"WTI"] - RW.fut9 
MSPERWft9 <-  mean(PE_RWft9^2) 
MSPEf9/MSPERWft9

dm.test(PEf9,PE_RWft9, alternative = "less")

MDirAcc(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL9"])
eriksPT_test(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL9"])


# 12-months ---------------------------------------------------------------
k <- 12
PEf12<-datats[(1+k):319,"WTI"] - fut[1:(319-k),"CL12"]
MSPEf12 <- mean(PEf12^2) 


RW.fut12 <- lag(datats[1:(319-k),"WTI"], k = -12)

dm.test(PEf12,PE_RWft12, alternative = "less")

PE_RWft12 <- datats[(1+k):319,"WTI"] - RW.fut12 
MSPERWft12 <-  mean(PE_RWft12^2) 
MSPEf12/MSPERWft12

futp12<-ts(fut[1:(319-k),12])
WTip12<-ts(datats[(1+k):319,"WTI"])
RW.fut12_p <- ts(RW.fut12)
DF12 <- data.frame(futp12, WTip12, RW.fut12)

autoplot(WTip12)+
  autolayer(futp12)+
  autolayer(RW.fut12_p)

plot(PE_RWft12)

MDirAcc(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL12"])
eriksPT_test(datats[(1+k):319,"WTI"],fut[1:(319-k),"CL12"])

# Alternativ 1-month--------------------------------------------------------------------

datats[1:318,"WTI"]*(1+ log(fut[1:318,"CL1"]/datats[1:318,"WTI"]))

PEfalt <- datats[2:319,"WTI"] - (datats[1:318,"WTI"]*(1+ log(fut[1:318,"CL1"]/datats[1:318,"WTI"])))

MSPEalt <- mean(PEfalt^2) 
MSPEalt/MSPERWft

dm.test(PEfalt,PE_RWft, alternative = "less")

MDirAcc(datats[(2):319,"WTI"],(datats[1:318,"WTI"]*(1+ log(fut[1:318,"CL1"]/datats[1:318,"WTI"]))))
eriksPT_test(datats[(2):319,"WTI"],(datats[1:318,"WTI"]*(1+ log(fut[1:318,"CL1"]/datats[1:318,"WTI"]))))
  # Alternativ 3-month ------------------------------------------------------
k <- 3

PEfalt3 <- datats[(1+k):319,"WTI"] - (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL3"]/datats[1:(319-k),"WTI"])))

MSPEalt3 <- mean(PEfalt3^2) 
MSPEalt3/MSPERWft3

dm.test(PEfalt3,PE_RWft3, alternative = "less")

MDirAcc(datats[(1+k):319,"WTI"], (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL3"]/datats[1:(319-k),"WTI"]))))
eriksPT_test(datats[(1+k):319,"WTI"],(datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL3"]/datats[1:(319-k),"WTI"]))))

# Alternativ 6 ------------------------------------------------------------

k <- 6

PEfalt6 <- datats[(1+k):319,"WTI"] - (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL6"]/datats[1:(319-k),"WTI"])))

MSPEalt6 <- mean(PEfalt6^2) 
MSPEalt6/MSPERWft6

dm.test(PEfalt6,PE_RWft6, alternative = "less")

MDirAcc(datats[(1+k):319,"WTI"], (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL6"]/datats[1:(319-k),"WTI"]))))
eriksPT_test(datats[(1+k):319,"WTI"],(datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL6"]/datats[1:(319-k),"WTI"]))))

# Alternativ 9 ------------------------------------------------------------

k <- 9
PEf9alt<-datats[(1+k):319,"WTI"] - (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL9"]/datats[1:(319-k),"WTI"])))
MSPEfalt9 <- mean(PEf9alt^2) 

MSPEfalt9/MSPERWft9

MDirAcc(datats[(1+k):319,"WTI"], (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL9"]/datats[1:(319-k),"WTI"]))))
eriksPT_test(datats[(1+k):319,"WTI"],(datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL9"]/datats[1:(319-k),"WTI"]))))

dm.test(PEf9alt,PE_RWft9, alternative = "less")

# Alternativ 12 -----------------------------------------------------------

k <- 12

PEfalt12 <- datats[(1+k):319,"WTI"] - (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL12"]/datats[1:(319-k),"WTI"])))

MSPEalt12 <- mean(PEfalt12^2) 
MSPEalt12/MSPERWft12

dm.test(PEfalt12,PE_RWft12, alternative = "less")

MDirAcc(datats[(1+k):319,"WTI"], (datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL12"]/datats[1:(319-k),"WTI"]))))
eriksPT_test(datats[(1+k):319,"WTI"],(datats[1:(319-k),"WTI"]*(1+ log(fut[1:(319-k),"CL12"]/datats[1:(319-k),"WTI"]))))
# Genskab  -----------------------------------------------------------------


PEf.test<-datats[2:213,"WTI"] - fut[1:212,"CL1"]
MSPEf.test <- mean(PEf.test^2) 


RW.fut.test <- ts(lag(datats[1:212,"WTI"], k = -1))

PE_RWft.test <- datats[2:213,"WTI"] - RW.fut.test 
MSPERWft.test <-  mean(PE_RWft.test^2) 
MSPEf.test/MSPERWft.test


# Genskab 9 ---------------------------------------------------------------

k <-9
PEf.test.9<-datats_test[(1+k):217,"WTI"] - fut_test[1:(217-k),"CL9"]
MSPEf.test.9 <- mean(PEf.test.9^2) 


RW.fut.test.9 <- ts(lag(datats_test[1:(217-k),"WTI"], k = -9))

PE_RWft.test.9 <- datats_test[(1+k):217,"WTI"] - RW.fut.test.9
MSPERWft.test.9 <-  mean(PE_RWft.test.9^2) 
MSPEf.test.9/MSPERWft.test.9

# Genskab 12 --------------------------------------------------------------
k <- 12
PEf.test.12<-datats_test[(1+k):217,"WTI"] - fut_test[1:(217-k),"CL12"]
MSPEf.test.12 <- mean(PEf.test.12^2) 

RW.fut.test.12 <- lag(datats_test[1:(217-k),"WTI"],k = -12)

PE_RWft.test.12 <- datats_test[(1+k):217,"WTI"] - RW.fut.test.12 
MSPERWft.test.12 <-  mean(PE_RWft.test.12^2) 
MSPEf.test.12/MSPERWft.test.12

futp12<-ts(fut_test[1:(217-k),"CL12"])
WTip12<-ts(datats_test[(1+k):217,"WTI"])
RW.fut12_p <- ts(RW.fut.test.12)

autoplot(WTip12)+
  autolayer(futp12)+
  autolayer(RW.fut12_p)
