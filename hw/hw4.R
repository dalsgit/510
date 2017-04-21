setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
# Question 2
library(astsa)

# ? Question 1
# ARIMA (0,1)×(1,0)12 model non-seasonal MA = .5 and the seasonal AR = .8
acfprob1=ARMAacf(ar = c(0,0,0,0,0,0,0,0,0,0,0,.8), ma=c(.5),lag.max=61)
plot(acfprob1, type="h")

pacfprob1=ARMAacf(ar = c(0,0,0,0,0,0,0,0,0,0,0,.8), ma=c(.5),lag.max=61, pacf=TRUE)
plot(pacfprob1, type="h")

# ARIMA (1,0)×(0,1)12 model with non-seasonal AR = .8 and seasonal MA  =.5 
acfprob2=ARMAacf(ar = c(.8), ma=c(0,0,0,0,0,0,0,0,0,0,0,.5),lag.max=61)
plot(acfprob2, type="h")

pacfprob2=ARMAacf(ar = c(.8), ma=c(0,0,0,0,0,0,0,0,0,0,0,.5),lag.max=61, pacf=TRUE)
plot(pacfprob2, type="h")

# ? Question 2
earnings=scan("hw1_jj.dat")
earnings = log(earnings)
plot (earnings, type="b")
diff1 = diff(earnings,1)
diff1and4=diff(diff1,4)
acf2 (diff1and4, 20)

sarima ( earnings, 1,1,0,0,1,1,4)
sarima ( earnings, 0,1,1,0,1,1,4)
sarima ( earnings, 0,1,1,1,1,0,4)
sarima ( earnings, 1,1,0,1,1,0,4)

# ? Question 3
unemp=scan("hw4_unemp.dat")
plot (unemp, type="b")
diff1=diff(unemp,1)
diff1and12=diff(diff1,12)
plot (diff1and12, type="b")
acf2 (diff1and12, 50)
#ARIMA (2, 1, 0,)× (0,1,1)12 
sarima     (unemp,   2,1,0,0,1,1,12)
sarima.for (unemp,12,2,1,0,0,1,1,12) 
