setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
library(astsa)

# ? Question 1
milk = ts(scan("hw5_pamilk.dat"), start = c(1, 6), frequency = 12)
decomp = decompose (milk, type="additive")
decomp
plot (decomp)

decomp = decompose (milk, type="multiplicative")
decomp
plot (decomp)

stl (milk, "periodic")

# ? Question 2
milk = ts(scan("hw5_pamilk.dat"), start = c(1, 6), frequency = 12)
diff1 = diff(milk, 1)
diff1and12=diff(diff1,12)
acf2 (diff1and12, 50)

sarima ( milk, 0,1,1,1,1,0,12)
sarima ( milk, 2,1,0,1,1,0,12)
sarima ( milk, 2,1,1,1,1,0,12)

sarima ( milk, 0,1,1,0,1,1,12)
sarima     ( milk,    2,1,0,0,1,1,12)
sarima.for ( milk, 12,2,1,0,0,1,1,12)

# Section 5.2 oildata example
oilindex = scan("hw5_oildata.dat")
plot (oilindex, type = "b", main = "Log of Oil Index Series")
expsmoothfit = arima (oilindex, order = c(0,1,1))
expsmoothfit # to see the arima results
predicteds = oilindex - expsmoothfit$residuals # predicted values
plot (oilindex, type="b", main = "Exponential Smoothing of Log of Oil Index")
lines (predicteds)
1.3877*oilindex[100]-0.3877*predicteds[100] # forecast for time 101

# ? Question 3
flour = ts(scan("hw5_flourminn.dat"), frequency = 12)
trendpattern =filter(flour, filter=c(1/24,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/24), sides=2)
plot (flour, type= "b", main = "moving average annual trend")
lines (trendpattern)

seasonals = flour - trendpattern
plot (seasonals, type = "b", main = "Seasonal pattern")

plot(lowess(flour, f = 1), main ="Lowess smoothing")
expsmoothfit = arima (flour, order = c(0,1,1))
expsmoothfit # to see the arima results
predicteds = flour - expsmoothfit$residuals # predicted values
plot (flour, type="b", main = "Exponential Smoothing of Monthly prices at a Minneapolis commodities exchange")
lines (predicteds)
# forecast for time 101
1.2891*flour[100]-0.2891*predicteds[100] 
