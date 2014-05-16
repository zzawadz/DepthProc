x = mvrnorm(100, c(0,0), diag(2))

depthContour(x,  points = TRUE, method = "Mahalanobis", mean = colMeans(x), cov =cov(x) )
depthContour(x,  points = TRUE, method = "Mahalanobis")
x
