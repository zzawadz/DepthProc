require(testthat)

x <- matrix(rnorm(12000), nc = 2)
  
#Same results
#system.time( depthLP(x, x, ndir = 2000, p = 2, name = "X") )

system.time(nor1 <- depthLPCPP(x, x, p = 1,a = 1,b = 1, threads = 1))
system.time(nor2 <- depthLPCPP(x, x, p = 1,a = 1,b = 1, threads = 2))
system.time(nor3 <- depthLPCPP(x, x, p = 1,a = 1,b = 1, threads = 8))

system.time(depth(x,x,method = "LP",threads = 4))

expect_equal(nor1, nor2)
nor-par


system.time(depthContour(x,method = "LP", threads = 1))



system.time(depthMah(x, x))

system.time(cov(x))
require(depthproc)

cov(x)
covCPP(x,-1)

max(cov(x)-covCPP(x,-1))
system.time(cov(x))
system.time(covCPP(x,-1))

require(depthproc)

x <- matrix(rnorm(2700)+10, nc = 3)
system.time(colMeans(x))

system.time(meanCPP(x, -1))
max(colMeans(x)-meanCPP(x, -1))


x <- matrix(rnorm(12000), nc = 2)
a = depthMahCPP(x,x,rcov = cov(x),rmean = NULL, 1)
b = depthMahCPP(x,x,rcov = NULL,rmean = NULL, 1)
c = depthMahCPP(x,x,rcov = NULL,rmean = as.matrix(colMeans(x)), 1)
d = depthMahCPP(x,x,rcov = cov(x),rmean = as.matrix(colMeans(x)), 1)
max(abs(a - b))
max(abs(a - c))
max(abs(a - d))


d1 = depthMahCPP(x,x,rcov = cov(x),rmean = matrix(colMeans(x), nc = ncol(x)), 1)
d2 = depthMahCPP(x,x,rcov = cov(x),rmean = as.matrix(colMeans(x)), 1)
max(d1-d2)
