setwd("C:/study/psu/git/510/HW")
# install.packages("astsa")
library(astsa)

# ? Sample
phlebitisdata = read.table("hw10_phlebitis.csv", header=T, sep=",")
attach (phlebitisdata)
phlebitisdata #This isn't necessary, but you might want to see the data structure
aov.p = aov(Y~(factor(Treatment)*factor(Time))+Error(factor(Animal)),phlebitisdata )
summary(aov.p)

library (nlme) #activates the nlme library
interaction.plot (Time, factor(Treatment), Y, lty=c(1:3),lwd=2,ylab="mean of Y", xlab="time", trace.label="Treatment")
nestinginfo <- groupedData(Y ~ Treatment | Animal, data= phlebitisdata)
fit.compsym <- gls(Y ~ factor(Treatment)*factor(Time), data=nestinginfo, corr=corCompSymm(, form= ~ 1 | Animal))
fit.nostruct <- gls(Y ~ factor(Treatment)*factor(Time), data=nestinginfo, corr=corSymm(, form= ~ 1 | Animal), weights = varIdent(form = ~ 1 | Time))
fit.ar1 <- gls(Y ~ factor(Treatment)*factor(Time), data=nestinginfo, corr=corAR1(, form= ~ 1 | Animal))
fit.ar1het <- gls(Y ~ factor(Treatment)*factor(Time), data=nestinginfo, corr=corAR1(, form= ~ 1 | Animal), weights=varIdent(form = ~ 1 | Time))
anova(fit.compsym, fit.nostruct, fit.ar1, fit.ar1het) #compares the models
anova(fit.compsym)

# ? Question 1 
data = read.table("hw10_glucose.CSV", header=T, sep=",")
attach (data)
data

aov.p = aov(Gluc~(factor(Food)*factor(Time))+Error(factor(Subject)), data)
summary(aov.p)

library (nlme) #activates the nlme library
interaction.plot (Time, factor(Food), Gluc, lty=c(1:3),lwd=2,
                  ylab="Mean of Glucose", xlab="Time", trace.label="Food")

nestinginfo <- groupedData(Gluc ~ Food | Subject, data=data)
fit.compsym <- gls(Gluc ~ factor(Food)*factor(Time), data=nestinginfo, 
                   corr=corCompSymm(, form= ~ 1 | Subject))

fit.nostruct <- gls(Gluc ~ factor(Food)*factor(Time), data=nestinginfo, 
                    corr=corSymm(, form= ~ 1 | Subject), weights = varIdent(form = ~ 1 | Time))

fit.ar1 <- gls(Gluc ~ factor(Food)*factor(Time), data=nestinginfo, 
               corr=corAR1(, form= ~ 1 | Subject))

fit.ar1het <- gls(Gluc ~ factor(Food)*factor(Time), data=nestinginfo, 
                  corr=corAR1(, form= ~ 1 | Subject), weights=varIdent(form = ~ 1 | Time))


anova(fit.compsym, fit.nostruct, fit.ar1, fit.ar1het) #compares the models
anova(fit.compsym) 
anova(fit.nostruct) 

# ? Question 2 
data = read.table("hw10_termites.CSV", header=T, sep=",")
attach (data)
data

interaction.plot (Time, factor(Dose), Y, lty=c(1:2),lwd=2,
                  ylab="Mean number of alive termites", xlab="Time", trace.label="Dose")
nestinginfo <- groupedData(Y ~ Dose | Dish, data=data)
fit.compsym <- gls(Y ~ factor(Dose)*factor(Time), data=nestinginfo, 
                   corr=corCompSymm(, form= ~ 1 | Dish))

fit.ar1 <- gls(Y ~ factor(Dose)*factor(Time), data=nestinginfo, 
               corr=corAR1(, form= ~ 1 | Dish))

anova(fit.compsym, fit.ar1) #compares the models
anova(fit.compsym) 
anova(fit.ar1) 

fit.ar1polytime <- gls(Y ~ factor(Dose)*poly(Time, degree = 3), data=nestinginfo, corr=corAR1(, form= ~ 1 | Dish))
summary(fit.ar1polytime)
