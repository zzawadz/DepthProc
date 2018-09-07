library(microbenchmark)
library(ddalpha)
library(DepthProc)
library(xtable)

generateData <- function(n, ndim) {
  matrix(rnorm(n), ncol = ndim)
}

x <- generateData(1000, 4)

benchResult <- microbenchmark(
  "ddalpha (1000 proj)" = depth.projection(x, x, num.directions = 1000),
  "DepthProc (1000 proj)" = depthProjection(x, threads = 1, ndir = 1000),
  "ddalpha (5000 proj)" = depth.projection(x, x, num.directions = 5000),
  "DepthProc (5000 proj)" = depthProjection(x, threads = 1, ndir = 5000),
  "ddalpha (10000 proj)" = depth.projection(x, x, num.directions = 1e4),
  "DepthProc (10000 proj)" = depthProjection(x, threads = 1, ndir = 1e4)
)

benchResult <- as.data.frame(summary(benchResult))

# lq - lower quantile, uq - upper quantile
benchTable <- benchResult[,c("expr", "lq", "mean", "median", "uq")]
xtable(benchTable) ## create latex table

##### Tukey depth ####
library(depth)
x2d <- generateData(1000, ndim = 2)
depth::depth(u = x2d, x = x2d)


## All points
benchResult2dTukey <- summary(microbenchmark(
  "depth" = apply(x2d, 1, depth, x = x2d),
  "DepthProc" = depthTukey(x2d, exact = TRUE)
))[,c("expr", "lq", "mean", "median", "uq")]

## One point
xm <- colMeans(x2d)
benchResult2dTukey <- summary(microbenchmark(
  "depth" = depth(xm, x = x2d),
  "DepthProc" = depthTukey(xm, x2d, exact = TRUE)
))[,c("expr", "lq", "mean", "median", "uq")]





# precision for a point
depthProc1000 <- replicate(500, depthProjection(x, ndir = 1000))
ddalpha1000 <- replicate(500, depth.projection(x, x, num.directions = 1000))

depthProc10000 <- replicate(500, depthProjection(x, ndir = 10000))
ddalpha10000 <- replicate(500, depth.projection(x, x, num.directions = 10000))


worstDepthProc <- which.max(diff(apply(depthProc1000, 1, range)))
resList <- list(
  depthProc1000 = depthProc1000[worstDepthProc,],
  ddalpha1000 = ddalpha1000[worstDepthProc,],
  depthProc10000 = depthProc10000[worstDepthProc,],
  ddalpha10000 = ddalpha10000[worstDepthProc,]
)

boxplot(resList)
