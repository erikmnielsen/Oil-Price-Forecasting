

data <- read_excel("VariableHam.xlsx")
data <- as.data.frame(data)
data$RO <- (data$WTI/(data$`CPI US`/100))
data$lRO <- log(data$RO)
data$l_diff_RO <- diff.xts(data$lRO, differences = 1)
data$OP <- log(data$`World oil prod`) - lag.xts(log(data$`World oil prod`),k = 1) 
data$OI <- (data$`OECD Pet inv`/data$`US Pet inv`)*data$`US crude inv`
data$dOI <- diff.xts(data$OI,differences = 1)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"
data$dWTI <- diff.xts(data$WTI, differences = 1)
data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["1973-01/2018-06"]



data.VARn <- data.xts[-1,c("Kilian","dOI","WTI","OP","CPI US")]
VARn.ts <- ts(data.VARn,frequency=12,start=c(1973, 2), end=c(2018, 6))
vn=c(NA)

# h=1, MSPE=0.9681 -------------------------------------------
for (i in 0:181) {
  train = VARn.ts[1:(364+i), ]
  VARnf <- VAR(train, p=12)
  recursive = predict(VARnf, n.ahead=1)
  fcst=recursive$fcst$WTI[,"fcst"] #OBS
  vn[i+1]=fcst
}

testn = subset(VARn.ts[, "WTI"],start=365,end = 545)
RWn = VARn.ts[-c(1:363), "WTI"]
RWnf.ts = ts(RWn[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
VARnf.ts = ts(vn[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))

plot(RWnf.ts)
plot(VARnf.ts)

PE_VARn=VARnf.ts-testn
MSPE_VARn = mean((PE_VARn)^2)
PE_RWn = RWnf.ts-testn
MSPE_RWn = mean((PE_RWn)^2)
MSPE_VARn/MSPE_RWn #fraction of random walk - should be <1 to beat the RW





data.VARnd <- data.xts[-1,c("Kilian","dOI","dWTI","OP","CPI US")]
VARnd.ts <- ts(data.VARnd,frequency=12,start=c(1973, 2), end=c(2018, 6))
vnd=c(NA)

# h=1, MSPE=0.9681 -------------------------------------------
for (i in 0:181) {
  train = VARnd.ts[1:(364+i), ]
  VARndf <- VAR(train, p=12)
  recursive = predict(VARndf, n.ahead=1)
  fcst=recursive$fcst$dWTI[,"fcst"] #OBS
  vnd[i+1]=fcst
}

testnd = subset(VARnd.ts[, "dWTI"],start=365,end = 545)
RWnd = VARnd.ts[-c(1:363), "dWTI"]
RWndf.ts = ts(RWnd[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))
VARndf.ts = ts(vnd[1:181], frequency=12, start=c(2003, 6), end=c(2018, 6))

plot(RWndf.ts)
plot(VARndf.ts)

PE_VARnd=VARndf.ts-testnd
MSPE_VARnd = mean((PE_VARnd)^2)
PE_RWnd =testnd
MSPE_RWnd = mean((PE_RWnd)^2)
MSPE_VARnd/MSPE_RWnd #fraction of random walk - should be <1 to beat the RW
