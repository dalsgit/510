setwd("C:/study/psu/git/510/HW")
x = scan ("hw1_eriedata.dat")
x = ts (x)
plot (x, type="b")

acf1 = acf(x)
acf1

lag1x = lag (x, -1)
y = cbind (x, lag1x)
ar1model = lm (y[,1] ~ y[,2])
summary (ar1model)

plot(fitted(ar1model),residuals(ar1model))
acf(residuals(ar1model))
x

