setwd("C:/study/psu/git/510/exam")
library(astsa)

x = ts(scan("e1x2.dat"))
plot (x, type="b")

diff1 = diff(x,1)
plot (diff1, type="b")
acf2 (diff1, 70)

sarima (x, 0, 1, 1)
sarima (x, 2, 1, 0)
sarima (x, 1, 1, 1)
sarima (x, 2, 1, 1)

sim_x=arima.sim(list(ar = .58), 200)
plot(sim_x)
acf2 (sim_x)
