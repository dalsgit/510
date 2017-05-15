setwd("C:/study/psu/git/510/exam")
library(astsa)

x=ts(scan("e2x.dat"))
y=ts(scan("e2y.dat"))
plot.ts(x,y,xy.lines=F,xy.labels=F)

regmodel=lm(y~x) #Step 1
summary(regmodel)
acf2(residuals(regmodel), 40) #Step 2
adjreg = sarima (y, 1,0,0, xreg=x) 
adjreg
