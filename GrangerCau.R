
# DM - manuel + roots  ----------------------------------------------------

library(multDM)

DM.test(VARf.ts, RWf_h1.ts, test_h1, h=1, H1 = "same", c=TRUE)
View(m.df)


eigen1<-roots(VAR(train, p=1))
eigen2<-roots(VAR(train, p=12))
eigen3<-roots(VAR(train, p=24))

eigen4<-roots(VAR(train2, p=1))
eigen5<-roots(VAR(train2, p=12))
eigen6<-roots(VAR(train2, p=24))

eigen7<-roots(VAR(train3, p=1))
eigen8<-roots(VAR(train3, p=12))
eigen9<-roots(VAR(train3, p=24))

eigen7
eigen9
EM1 <- as.data.frame(matrix(nrow=4,ncol=2))
EM1<-cbind(eigen1[1:3], eigen2[1:3],eigen3[1:3],eigen4[1:3],eigen5[1:3],eigen6[1:3],eigen7[1:3],eigen8[1:3],eigen9[1:3])

eigen5[1:3]
for (j in 1:9) {
  for (i in 1:3) {
    if(EM1[i,j] > 1){print("not stat")
    }else print("all good")
  }
}

## DM 

PE.VAR=VARf.ts-test_h1
PE.RW=RWf_h1.ts-test_h1

MSPEDM = mean(PE.VAR^2 - PE.RW^2)
MPEDM = mean(PE.VAR - PE.RW)
SPEDM = PE.VAR^2 - PE.RW^2
PEDM = PE.VAR - PE.RW
lm_PEDM<-lm(PEDM ~ 1)
lm_SPEDM <- lm(SPEDM ~ 1)

vcvPEDM <- vcovHAC(lm_PEDM)

coeftest(lm_PEDM)
coeftest(lm_PEDM, vcov = vcovHAC(lm_PEDM))
coeftest(lm_SPEDM, vcov = vcovHAC(lm_SPEDM))

dm.test(PE.VAR, PE.RW, power = 1)
dm.test(PE.VAR, PE.RW, power = 2)

# Granger Causality -------------------------------------------------------

GC<-VAR(VAR.ts, p= 12)

GC1<-VAR(VAR.ts[,c("lRO","Kilian")], p= 12)

GC2<-VAR(VAR.ts[,c("lRO","OP")], p= 12)

GC3<-VAR(VAR.ts[,c("lRO","dOI")], p= 12)

summary(GC)

GC_ALL <- causality(GC, cause = c("OP", "dOI", "Kilian"), boot = TRUE)

GC_dOIKIL <- causality(GC, cause = c("dOI", "Kilian" ), boot = TRUE)

GC_Kil <- causality(GC, cause = "Kilian")
GC_Kil1 <- causality(GC1, cause = "Kilian")

GC_OP <- causality(GC, cause = "OP")
GC_OP1 <- causality(GC2, cause = "OP")

GC_dOI <- causality(GC, cause = "dOI")
GC_dOI1 <- causality(GC3, cause = "dOI")

GC_lRO <- causality(GC, cause = "lRO")

GC_ALL

GC_dOIKIL

GC_OP
GC_OP1

GC_dOI
GC_dOI1

GC_Kil
GC_Kil1

GC_lRO

GC_matrix <- cbind(GC_OP1$Granger$p.value,GC_dOI1$Granger$p.value,GC_Kil1$Granger$p.value, GC_Hamilton$Granger$p.value)
colnames(GC_matrix)= c("OP","dOI","Kilian","Hamilton1")

grangertest(VAR.ts[,"lRO"],VAR.ts[,"Kilian"])
grangertest(VAR.ts[,"Kilian"],VAR.ts[,"lRO"])
grangertest(VAR.ts[,"OP"],VAR.ts[,"lRO"], order = 12)
grangertest(VAR.ts[,"dOI"],VAR.ts[,"lRO"], order = 12)
grangertest(VARHam.ts[,"Hamilton"],VARHam.ts[,"lRO"])


# Hamilton ----------------------------------------------------------------

data.VARHamil <- data.xts[-1,c("Kilian","dOI","lRO","OP","Hamilton")]
VARHam.ts <- ts(data.VARHamil,frequency=12,start=c(1973, 2), end=c(2018, 6))

GCH<-VAR(VARHam.ts[,c("lRO","Hamilton")], p= 12)

GC_Hamilton <- causality(GCH, cause = "Hamilton")
GC_Hamilton



# strukturel skift --------------------------------------------------------

library(struccchange) 
library(sandwich)

## Skal have den nyeste version 
