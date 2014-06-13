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




x = mvrnorm(n = 100, mu = c(0,0), Sigma = 3*diag(2))
y = rmvt(n = 100, sigma = diag(2), df = 2)
scaleCurve(x, y, method = "Projection", plot = TRUE, title = "AAA")











data(pension)
plot(pension)
abline(lm(Reserves~Income,data = pension), lty = 3, lwd = 2) #lm
abline(trimProjReg2d(pension[,1],pension[,2]), lwd = 2) #trimprojreg2d
legend("bottomright", c("OLS","TrimLS"), lty = 1:2)




require(MASS)
x = depthproc:::.testNorm()

depth(x,x,method = "Tukey")

depthContour(x,method = "Tukey")
