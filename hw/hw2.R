# Question 1
acfprob3=ARMAacf(ma=c(.6), lag.max=10)
plot(seq(0,10), acfprob3, xlim=c(1,10), xlab="lags", type="h")

pacfprob3 = ARMAacf(ma=c(.6), lag.max=10, pacf=TRUE)
plot(pacfprob3, type="h")

# Question 2
acfprob4=ARMAacf(ar=c(-.6), lag.max=10)
lags=0:10 #creates a variable named lags that ranges from 0 to 10.
plot(lags, acfprob4, xlim=c(1,10), type="h")

pacfprob4 = ARMAacf(ar=c(-.6), lag.max=10, pacf=TRUE)
plot(pacfprob4, type="h")
