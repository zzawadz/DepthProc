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



u = .testNorm()
u = rbind(u, u+5)

depthContour(u,method = "LP",points=TRUE)

depthContour(DATA,method = "Local",points=TRUE, depth1="LP",depth2="LP")
depthContour(DATA,method = "Local",points=TRUE, depth1="Projection",depth2="Projection",beta=0.7)

