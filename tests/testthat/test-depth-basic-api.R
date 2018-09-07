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
}


test_that("Projection depth works with vector", {
  matrixApiMatchVectorApi(depthProjection)
})

test_that("Mahalanobis depth works with vector", {
  matrixApiMatchVectorApi(depthMah)
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
