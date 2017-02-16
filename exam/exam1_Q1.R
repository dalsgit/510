setwd("C:/study/psu/git/510/exam")
library(astsa)

x = ts(scan("e1x1.dat"), frequency = 12)
plot (x, type="b")

decomp = decompose (x, type="multiplicative")
decomp
plot (decomp$random)

trendpattern =filter(x, filter=c(1/24,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/24), sides=2)
plot (x, type= "b", main = "moving average annual trend")
lines (trendpattern)

seasonals = x - trendpattern
plot (seasonals, type = "b", main = "Seasonal pattern")

logx = sqrt(sqrt(x))
plot(logx, type="b")

acf2 (logx, 36)

diff1 = diff(x,1)
diff1and12=diff(diff1,12)
plot(diff1and12, type="b")
acf2 (diff1and12, 70)

diff12 = diff(x,12)
plot(diff12, type="b")
acf2 (diff12, 70)

sarima ( x, 1,0,1,1,1,0,12)
sarima ( x, 3,0,1,1,1,0,12)
sarima ( x, 1,0,0,1,1,0,12)

