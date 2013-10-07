require(MASS)

X = mvrnorm(100000,rep(0,100),diag(100))
u = matrix(rep(0,50),nrow = 1)
require(depthproc)
system.time(depthproc:::depthMahCPP(X,X))
system.time(depthproc:::depthMah(X,X))
max(depthproc:::depthMahCPP(X,X) - depthproc:::depthMah(X,X))


max(depth(X,X, method="Mahalanobis") - depthproc:::depthMahCPP(X,X))


system.time(runifsphere(1e6,20))
system.time(depthproc:::runifsphereCPP(1e6,20))
