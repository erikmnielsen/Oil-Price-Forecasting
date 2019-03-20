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

View(data)
data.xts <- xts(data[-1], data[[1]])
data.xts <- data.xts["1973-01/2018-06"]
plot(data.xts)

# test --------------------------------------------------------------------

testt <- ur.df(data.xts$WTI,type=c("none", "drift", "trend"))
testt
summary(testt)
adf.test(data.xts$WTI)
d_WTI<-diff(data.xts$WTI)
# VAR ---------------------------------------------------------------------
VARselect(data.xts)        
VAR <- VAR(data.xts, p=8) 
VAR

Acoef(VAR) ### [KxK] matrix med koeff


