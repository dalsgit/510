setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
library(astsa)

# ? Question 2 - Part A
t = 1:100
x1 = 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)
x2 = 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)
x3 = 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)
x =x1+x2+x3
t = 1:128
x.1 = 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)
x.2 = 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)
x.3 = 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)
x.f =x.1+x.2+x.3
par(mfrow=c(4,2))
plot.ts(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13~~~n==100))
plot.ts(x.1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13~~~n==128))
plot.ts(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41~~~n==100))
plot.ts(x.2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41~~~n==128))
plot.ts(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85~~~n==100))
plot.ts(x.3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85~~~n==128))
plot.ts(x, ylim=c(-10,10), main="sum, n=100")
plot.ts(x.f, ylim=c(-10,10), main="sum, n=128")

# ? Question 2 - Part B
P = abs(2*fft(x)/100)^2; Fr = 0:99/100
plot(Fr, P, type="o", xlab="frequency", ylab="periodogram")

P = abs(2*fft(x)/100)^2 
f = 0:50/100 
plot(f, P[1:51], type="o", xlab="frequency", ylab="periodogram") 

P = abs(2*fft(x.f)/128)^2 
f = 0:64/128 
plot(f, P[1:65], type="o", xlab="frequency", ylab="periodogram") 
f
round(P[1:65],7)

# ? Question 2 - Part C
x.rand = x1+x2+x3+rnorm(100,0,5)
par(mfrow=c(2,1))
plot.ts(x, ylim=c(-25,25), main="sum without random, n=100")
plot.ts(x.rand, ylim=c(-25,25), main="sum with random, n=100")
P = abs(2*fft(x.rand)/100)^2 
f = 0:50/128 
plot(f, P[1:51], type="o", xlab="frequency", ylab="periodogram") 
f
round(P[1:51],7)

# ? Question 3 
x = scan ("hw7_speech.dat")
t = 1:1020
regr = lm(x~t)
y = residuals(regr)
par(mfrow=c(2,1))
plot (x, type="b", main="raw data")
plot (y, type="b", main="detrended data")

P = abs(2*fft(y)/1020)^2 
f = 0:510/1020 
plot(f, P[1:511], type="o", xlab="frequency", ylab="periodogram") 
f
round(P[1:511],7)

# ? Question 4
series1 = scan ("hw7_lakesim100.dat")
plot (series1, type="b")
acf2 (series1, 90)
diff1=diff(series1,1)
acf2 (diff1, 60)

sarima (series1, 0, 1, 2)
sarima (series1, 1, 1, 2)
sarima (series1, 0, 1, 1)
