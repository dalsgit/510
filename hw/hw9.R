setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
library(astsa)

# ? Question 1 
x = ts(scan("hw9_xwk9prob1.dat"))
y = ts(scan("hw9_ywk9prob1.dat"))

plot (x, type="b")
acf2 (x, 40)
diff1 = diff(x,1)
acf2 (diff1, 40)
sarima (x, 1, 1, 0)
sarima (x, 1, 1, 1)

ar1model = arima(x, order = c(1,1,0))
ar1model
pwx=ar1model$residuals
newpwy = filter(y, filter = c(1, -1.4698, 0.4698), sides =1)
ccfvalues = ccf (pwx,newpwy,na.action=na.omit)
ccfvalues

alldata=ts.intersect(y, xlag2=lag(x,-2), xlag3=lag(x,-3))
tryit = lm(y~xlag2+xlag3, data = alldata)
summary (tryit)
acf2(residuals(tryit))

# ? Question 2 
x = ts(scan("hw9_inputrate.dat"))
y = ts(scan("hw9_outputrate.dat"))
ccfvalues = ccf (x,y,40)
ccfvalues
alldata=ts.intersect(y,ylag1=lag(y,-1), xlag5 = lag(x,-5),  xlag6 = lag(x,-6))
tryit = lm(y~ylag1+xlag5+xlag6, data = alldata)
summary (tryit)
acf2(residuals(tryit))                     
                    
# ? Question 3
x = ts(scan("hw9_highway.dat"))
plot (x, type="b")
before = window (x, 1, 109)
after = window (x, 110, 139)

sarima(before, 1,0,0,1,0,0,12)
themodel = arima(before, order = c(1,0,0), seasonal = list(order = c(1,0,0), period = 12))
themodel

afterpred = sarima.for(before, 30,1,0,0,1,0,0,12)
diffs = after - afterpred$pred 
plot(diffs)
mean(diffs) 
