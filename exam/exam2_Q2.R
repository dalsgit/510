setwd("C:/study/psu/git/510/exam")
library(astsa)

y=ts(scan("e2q2.txt"))
plot(y,type="b")
diff1 = diff(y,1)
plot(diff1,type="b")

model = ts.intersect(y, lag1y=lag(y,-1))
x = model[,1]
P = model[,2]
c = -86 ## Threshold value

##Regression for values below the threshold
less = (P<c)
x1 = x[less]
P1 = P[less]
out1 = lm(x1~P1)
summary(out1)

##Regression for values above the threshold
greater = (P>=c)
x2 = x[greater]
P2 = P[greater]
out2 = lm(x2~P2)
summary(out2)

##Residuals
res1 = residuals(out1)
res2 = residuals(out2)
less[less==1]= res1
greater[greater==1] = res2
resid = less + greater
acf2(resid)

##Predicted values
less = (P<c)
greater = (P>=c)
fit1 = predict(out1)
fit2 = predict(out2)
less[less==1]= fit1
greater[greater==1] = fit2
fit = less + greater
plot(y, type="o")
lines(fit, col = "red", lty="dashed")

