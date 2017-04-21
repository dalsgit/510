setwd("C:/study/psu/git/510/HW")
library(astsa)

# ? Sample

# ? Question 1 
kernel("daniell", 3)
kernel("daniell",c(3,3))
kernel("daniell",c(1,2))

kernel("modified.daniell", 3)

# ? Question 2
x = ts(scan ("hw12_week12sim.dat"))
## raw periodogram
spec.pgram(x, taper=0, log="no")
## smoothen with daniell m=2
k = kernel("daniell", 2)
spec.pgram(x, k, taper=0, log = "no")
## smoothen with modified daniell m=3
k = kernel("modified.daniell", 3)
spec.pgram(x, k, taper=0, log = "no")

specvalues = spec.pgram(x, spans=c(7,7), taper = 0, log="no")
specvalues

specvalues=spec.ar(x, log ="no")
specvalues

acf2 (x, 20)
sarima (x, 2,0,0)
sarima (x, 3,0,0)

# ? Question 3
x = rnorm(250,0,1)
x
specvalues=spec.ar(x, log ="no")

# ? Question 4
x = ts(scan ("hw12_temp.dat"))
## raw periodogram
spec.pgram(x, taper=0, log="no")
## smoothen with daniell m=2
k = kernel("daniell", 2)
spec.pgram(x, k, taper=0, log = "no")

specvalues = spec.pgram(x, spans=c(5,5), taper = 0, log="no")
specvalues
