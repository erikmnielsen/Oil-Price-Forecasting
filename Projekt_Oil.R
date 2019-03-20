library(readxl)
library(forecast)
library(dplyr)
library(timeSeries)
library(vars)
library(aTSA)

# Variable <- read_excel("~/8.semester projekt/Variable.xlsx")
data <- read_excel("Variable.xlsx")
data <- as.data.frame(data)
data$Date <- as.yearmon(format(data$Date), "%Y-%m-%d") #"Q%q%Y"

data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["2000/2005-06"]
plot(data.xts$WTI)



# test --------------------------------------------------------------------



Oil <- ts(Variable[,2:6], start = c(1973,1), frequency = 12)

plot.ts(Oil)
adf.test(Oil[,1])
adf.test(Oil[,2])
adf.test(Oil[,3])
adf.test(Oil[,4])
adf.test(Oil[,5])


testt <- ur.df(Oil[,1],type="trend",lags=lag)



d_WTI<-diff(Oil[,1])
