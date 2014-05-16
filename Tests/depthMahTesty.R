d = 2
x = mvrnorm(2000, rep(0,d), diag(d))

depthContour(x,  points = TRUE, method = "Mahalanobis", mean = colMeans(x), cov =cov(x) )
depthContour(x,  points = TRUE, method = "Mahalanobis")

a1 = depth(x,x, method = "Mahalanobis")
a2 = depth(x,x, method = "Mahalanobis", mean = colMeans(x))

max(a1-a2)


system.time(depth(x,x, method = "Mahalanobis", threads = -2))


d = 2
x = mvrnorm(200000, rep(0,d), diag(d))
system.time(depth(x,x, threads = -1))

system.time(depthContour(x, threads = -1))
system.time(depthContour(x, threads = 1))

