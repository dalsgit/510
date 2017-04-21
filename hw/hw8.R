setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
library(astsa)

# ? Question 2 
salesY = scan("hw8_sales8.dat")
leadX = scan("hw8_lead8.dat")
salesY=ts (salesY)
leadX = ts(leadX)
plot (salesY, type = "b", main = "Sales")
plot (leadX, type = "b", main = "Leading Indicator")
ccf (leadX, salesY, ylab="CCF")

diffsalesY = diff(salesY,1)
diffleadX = diff(leadX,1)   
ccf (diffleadX, diffsalesY, ylab="CCF")
lag2.plot (diffleadX, diffsalesY, 6)

alldata=ts.intersect(salesY,
                     #salesYlag1=lag(salesY,-1), salesYlag2=lag(salesY,-2),
                     leadXlag3 = lag(leadX,-3), leadXlag4 = lag(leadX,-4))
trend=time(alldata[,1])
tryit = lm(salesY~leadXlag3+leadXlag4, data = alldata)
summary (tryit)
tryit2 = lm(salesY~trend+leadXlag3+leadXlag4, data = alldata)
summary (tryit2)

acf2(residuals(tryit))

adjreg = arima (alldata[,1], order = c(1,0,0), xreg = cbind(trend, alldata[,2], alldata[,3])) #AR(1) for residuals
adjreg
sarima (alldata[,1], 1,0,0, xreg = cbind(trend, alldata[,2], alldata[,3])) #AR(1) for residuals
sarima (alldata[,1], 1,0,0, xreg = cbind(alldata[,2], alldata[,3])) #AR(1) for residuals

# ? Question 3 
y = scan("hw8_yvar-week8prob3.dat")
x = scan("hw8_xvar-week8prob3.dat")
y=ts (y)
x = ts(x)

ccfvalues =ccf (x, y, ylab="CCF")
ccfvalues

alldata=ts.intersect(y, ylag1=lag(y,-1), xlag3 = lag(x,-3))
tryit = lm(y~xlag3+ylag1, data = alldata)
summary (tryit)
tryit2 = lm(y~xlag3, data = alldata)
summary (tryit2)
acf2(residuals(tryit2))

# ? Question 1 
y = scan("hw8_yhw8prob1.dat")
x = scan("hw8_xhw8prob1.dat")
y=ts (y)
x = ts(x)
plot (y, type = "b", main = "Y")
plot (x, type = "b", main = "X")
plot.ts(x,y,xy.lines=F,xy.labels=F)

regmodel=lm(y~x) 
summary(regmodel)
acf2(residuals(regmodel)) # shows AR(1) model

ar1res = arima (residuals (regmodel), order = c(1,0,0), include.mean = FALSE) #AR(1) Step 3
sarima (residuals (regmodel), 1,0,0, no.constant = T) #Step 3
xl = ts.intersect(x, lag(x,-1)) # Step 4 Create matrix with x and lag 1 x as elements
xnew=xl[,1] - 0.6382 * xl[,2] # Step 4 Create x variable for adjustment regression
yl = ts.intersect(y,lag(y,-1)) # Step 4 Create matrix with y and lag 1 y as elements
ynew=yl[,1] - 0.6382 * yl[,2] # Step 4 Create y variable for adjustment regression
adjustreg = lm(ynew~xnew) # Step 5 Adjustment regression
summary(adjustreg)
acf2(residuals(adjustreg))


#install.packages("orcutt")
library(orcutt)
cochrane.orcutt(regmodel)

trend = time(y)
arima (y, order = c(1,0,0), xreg = cbind(trend, x)) #This is the adjustment regression with AR(1)
sarima (y, 1,0,0, xreg=cbind(trend, x)) #Step 3 
# drop trend as it is not significant
arima (y, order = c(1,0,0), xreg = cbind(x)) #This is the adjustment regression with AR(1)
sarima (y, 1,0,0, xreg=cbind(x)) #Step 3 

