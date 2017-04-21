setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
#install.packages("vars") 
#install.packages("astsa")
library(astsa)
library(vars)
library(astsa)

# ? Sample
cmort = ts(scan ("hw11_cmort.dat"))
tempr = ts(scan ("hw11_tempr.dat"))
part = ts(scan ("hw11_part.dat"))

x = cbind(cmort, tempr, part)
plot.ts(x , main = "", xlab = "")
summary(VAR(x, p=1, type="both"))


# ? Question 1 
x = ts(scan ("hw11_metalsemploy.dat"))
plot (x, type="b")
acf2 (x, 20)
diff1=diff(x,1)
plot (diff1, type="b")
acf2 (diff1, 60)

diffsqrd = diff1*diff1
acf2 (diffsqrd, 60)
sarima ( diffsqrd, 1,0,0)
arima ( diffsqrd, order = c(1,0,0))

# ? Question 2
y1 = ts(scan ("hw11_y1hw11.2.dat"))
y2 = ts(scan ("hw11_y2hw11.2.dat"))
y3 = ts(scan ("hw11_y3hw11.2.dat"))

x = cbind(y1, y2, y3)
plot.ts(x , main = "", xlab = "")
fitvar1 = VAR(x, p=1, type="both")
summary(fitvar1)
fitvar2 = VAR(x, p=2, type="both")
summary(fitvar2)

n = 400
k = 3
p = 1
BIC_var1 = log(det(summary(fitvar1)$covres)) + (k^2*p*log(n)/n)
BIC_var1
p = 2
BIC_var2 = log(det(summary(fitvar2)$covres)) + (k^2*p*log(n)/n)
BIC_var2

VARselect(x, lag.max=10, type="both")

acf(residuals(fitvar1)[,1])
acf(residuals(fitvar1)[,2])
acf(residuals(fitvar1)[,3])
