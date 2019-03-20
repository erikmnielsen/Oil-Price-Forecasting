library(readxl)
library(forecast)
library(dplyr)
library(timeSeries)
library(vars)
library(aTSA)

# Variable <- read_excel("~/8.semester projekt/Variable.xlsx")
Variable <- read_excel("Variable.xlsx")


Oil <- ts(Variable[,2:6], start = c(1973,1), frequency = 12)

plot.ts(Oil)
adf.test(Oil[,1])
adf.test(Oil[,2])
adf.test(Oil[,3])
adf.test(Oil[,4])
adf.test(Oil[,5])

#dsfsdfsdfsdfsdf
#sdsdfdfsdsdfsd

d_WTI<-diff(Oil[,1])
