setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
library(astsa)
# Question 2
series1 = scan ("hw3_sim1week3.dat")
plot (series1, type="b")
acf2 (series1, 20)
#MA(1)
sarima (series1, 0, 0, 1)
#MA(2)
sarima (series1, 0, 0, 2)
# ARIMA (1,0,1)
sarima (series1, 1, 0, 1)
#MA(2) to forecast values for the next 6 time periods 
sarima.for(series1,6, 0,0,2)

# Question 3
series1 = scan ("hw3_lakehuron.dat")
plot (series1, type="b")
acf2 (series1, 20)
#AR(2)
sarima (series1, 2, 0, 0)
#AR(2) to forecast values for the next 3 time periods 
sarima.for(series1,3, 2,0,0)

# Question 4
prices = scan ("hw3_flourbuffalo.dat")
plot (prices, type="b")
acf2 (prices, 20)
diff1=diff(prices,1)
acf2 (diff1)

