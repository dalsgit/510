setwd("C:/study/psu/git/510/HW")
x = scan ("hw1_jj.dat")
x = ts (x)
plot (x, type="b")

logx = log(x)
plot (logx, type = "b")

