setwd("C:/study/psu/git/510/HW")
library(astsa)

# ? Sample

# ? Question 1 
y = arima.sim(n = 400, list (ma=c(0.6, 0.5))) ## n = 400 for an MA(2)
plot (y, type="b")
acf2(y)

y = arima.sim(n = 400, list (ma=c(0.9, 0.6))) ## n = 400 for an MA(2)
plot (y, type="b")
acf2(y)

y = arima.sim (n = 400, list (ar=c(0.7), ma=c(0.8)))
plot (y, type="b")
acf2(y)

# ? Question 2
x = ts(scan ("hw14_metalsemploy.dat"))
plot (x, type="b")
acf2 (x, 40)

diff1 = diff(x,1)
acf2 (diff1, 40)

# ? Question 3
x = ts(scan ("hw14_buffsnow.dat"))
plot (x, type="b")
acf2 (x, 40)

#MA(1)
sarima (x, 0, 0, 1)
#MA(2)
sarima (x, 0, 0, 2)
# ARIMA (1,0,1)
sarima (x, 1, 0, 1)
# ARIMA (1,0,2)
sarima (x, 1, 0, 2)
