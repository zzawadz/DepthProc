context("Basic api for depth functions")

matrixApiMatchVectorApi <- function(dep) {
  xx <- matrix(rnorm(100), ncol = 2)
  mx <- colMeans(xx)
  matmx <- matrix(mx, ncol = ncol(xx))

  set.seed(123)
  d1 <- dep(mx, xx)
  set.seed(123)
  d2 <- dep(matmx, xx)
  expect_equal(d1, d2)

  set.seed(123)
  d3 <- dep(xx)
  set.seed(123)
  d4 <- dep(xx, xx)
  expect_equal(d3, d4)
}


test_that("Projection depth works with vector", {
  matrixApiMatchVectorApi(depthProjection)
})

test_that("Mahalanobis depth works with vector", {
  matrixApiMatchVectorApi(depthMah)

  xx <- matrix(rnorm(100), ncol = 2)
  expect_equal(
    depthMah(xx,xx, mean = colMeans(xx)),
    depthMah(xx, xx)
  )
})

test_that("Euclidean depth works with vector", {
  matrixApiMatchVectorApi(depthEuclid)
})

test_that("Tukey depth works with vector", {
  matrixApiMatchVectorApi(depthTukey)
})

test_that("LP depth works with vector", {
  matrixApiMatchVectorApi(depthLP)
})

test_that("Depth baisc api", {
  set.seed(123)
  x <- data.frame(rnorm(10))
  set.seed(123)
  d1 <- depth(x,x)
  set.seed(123)
  d2 <- depth(x)
  expect_equal(d1, d2)

  y <- rnorm(100)
  set.seed(123)
  d3 <- depth(y,y)
  set.seed(123)
  d4 <- depth(y)
  expect_equal(d3, d4)
})

