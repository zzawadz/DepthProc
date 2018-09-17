library(microbenchmark)
library(ddalpha)
library(DepthProc)
library(xtable)

generateData <- function(n, ndim) {
  matrix(rt(n), ncol = ndim)
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

##### Tukey depth #####
library(depth)
x2d <- generateData(1000, ndim = 2)

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

all.equal(
  apply(x2d, 1, depth, x = x2d),
  as.numeric(depthTukey(x2d, exact = TRUE)),
  tolerance = 1e-7
)

# tukey 5d
x5d <- generateData(200, ndim = 5)
benchResult5dTukey <- summary(microbenchmark(
  "depth" = apply(x5d, 1, depth, x = x5d, approx = TRUE),
  "DepthProc" = depthTukey(x5d, exact = TRUE)
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

###### depth median

# 2D case – sample 500 observations from Student t with 3 degree of freedom
generateData <- function(n, ndim, df) {
  matrix(rt(n, df = df), ncol = ndim)
}

library(microbenchmark)
library(depth)
library(DepthProc)
x2d<- generateData(500, 2, 3)

benchResult <- microbenchmark(
  "depth (exact)" = med(x2d, method = "Tukey", approx = FALSE),

  "DepthProc (1000 proj)" = depthMedian(x2d, depth_params = list(
    method = "Tukey",
    threads = 1,
    ndir = 1000
  )),
  "DepthProc (exact)" = depthMedian(x2d, depth_params = list(
    method = "Tukey",
    threads = 1,
    exact = TRUE
  ))
)

benchResult <- as.data.frame(summary(benchResult))

benchTable <- benchResult[,c("expr", "lq", "mean", "median", "uq")]
xtable(benchTable) ## create latex table


5D case – sample 100 observations with 3 degree of freedom
x5d<- generateData(500, 5,3)


benchResult<-microbenchmark(
  "depth (1000 proj)"=med(x5d, method = "Tukey", approx = TRUE,ndir=1000),

  "DepthProc (1000 proj)" =depthMedian(x5d,depth_params = list(method = "Tukey",threads = 1, ndir = 1000)))

benchResult <- as.data.frame(summary(benchResult))

benchTable <- benchResult[,c("expr", "lq", "mean", "median", "uq")]
xtable(benchTable) ## create latex table
