setwd("C:/study/psu/git/510/HW")
library(astsa)
library(fracdiff)

# ? Sample
varve = ts(scan ("hw13_varve.dat"))
y = log(varve) - mean(log(varve)) # Center the logs
acf2(y) ## ACF and PACF of the data

## Estimate d:
varvefd = fracdiff(y, nar=0, nma=0,M=30)
summary(varvefd)
d=varvefd$d
d
##Residuals
resid = diffseries(y, d) 
acf2(resid)

# ? Question 1 
x = ts(scan ("hw13_fracdiff.dat"))
plot (x, type="b")
acf2 (x, 40)
mymodel <-  fracdiff(x, nar=1, nma=0, M=30)
summary(mymodel)
mymodel$stderror.dpq 

diff1 = diff(x,1)
acf2 (diff1, 40)

sarima (x, 1, 1, 0)
sarima (x, 0, 1, 1)
sarima (x, 2, 1, 1)

# ? Question 2
mymodel <-  fracdiff(x, nar=0, nma=0, M=30)
summary(mymodel)
d=mymodel$d
d
##Residuals
resid = diffseries(x, d) 
acf2(resid)

# ? Question 3
x = ts(scan ("hw13_globaltmp2.dat"))
plot (x, type="b")
acf2 (x, 40)
mymodel <-  fracdiff(x, nar=0, nma=0, M=30)
summary(mymodel)
d=mymodel$d
d
##Residuals
resid = diffseries(x, d) 
acf2(resid)

# ? Question 4
y = ts(scan ("hw13_sunspots.dat"))
plot (y, type="b")

model = ts.intersect(y, lag1y=lag(y,-1), lag2y=lag(y, -2), lag3y=lag(y,-3), lag4y=lag(y, -4))
x = model[,1]
P = model[,2:5]
c = 65 ## Threshold value

##Regression for values below the threshold
less = (P[,1]<c)
x1 = x[less]
P1 = P[less,]
out1 = lm(x1~P1[,1]+P1[,2]+P1[,3]+P1[,4])
summary(out1)
mse(out1)

##Regression for values above the threshold
greater = (P[,1]>=c)
x2 = x[greater]
P2 = P[greater,]
out2 = lm(x2~P2[,1]+P2[,2]+P2[,3]+P2[,4])
summary(out2)
mse(out2)

##Residuals
res1 = residuals(out1)
res2 = residuals(out2)
less[less==1]= res1
greater[greater==1] = res2
resid = less + greater
acf2(resid)

##Predicted values
less = (P[,1]<c)
greater = (P[,1]>=c)
fit1 = predict(out1)
fit2 = predict(out2)
less[less==1]= fit1
greater[greater==1] = fit2
fit = less + greater
plot(y, type="o")
lines(fit, col = "red", lty="dashed")


mse <- function(sm)
    mean(sm$residuals^2)
