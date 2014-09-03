#install.packages("depthTools")
require(depthTools)
?MBD

x = matrix(rnorm(1000),100,10)
x_d = MBD(x,plotting=TRUE)

all.equal(as.numeric(x_d$MBD), as.numeric(depthMBD(x)))
all.equal(as.numeric(x_d$MBD), as.numeric(depthMBD(x,x)))

system.time(depthMBD(x))
system.time(MBD(x, plotting = FALSE))

d = cbind(c(1,0.8),c(0.8,1))
x = mvrnorm(1000,c(0,0),d)
depthContour(x,method = "MBD", points = TRUE)
