context("Regression tests for the reviewed depth defects")

test_that("asymmetryCurve uses the caller's depth method for the y sample", {
  set.seed(7)
  x <- matrix(rnorm(1600), ncol = 2)
  y <- matrix(rnorm(1600), ncol = 2)

  both <- asymmetryCurve(x, y, depth_params = list(method = "Mahalanobis"))
  alone <- asymmetryCurve(y, depth_params = list(method = "Mahalanobis"))

  expect_equal(as.numeric(both[[2]]@.Data), as.numeric(alone@.Data))
  expect_equal(as.numeric(both[[2]]@depth),
               as.numeric(depth(y, y, method = "Mahalanobis")))
})

test_that("fncDepthFM returns one depth per row of u, not of X", {
  set.seed(123)
  X <- matrix(rnorm(20 * 10), nrow = 20)
  u <- matrix(rnorm(5 * 10), nrow = 5)

  expect_equal(length(fncDepthFM(u, X)), nrow(u))
  expect_equal(length(fncDepthFM(X)), nrow(X))
})

test_that("depth() rejects an unusable method instead of returning NULL", {
  x <- matrix(rnorm(100), ncol = 2)

  expect_error(depth(x, x, method = "projection"), "unknown depth method")
  expect_error(depth(x, x, method = "NotADepth"), "unknown depth method")
  expect_error(depth(x, x, method = 3), "single character string")
})

test_that("depthMedian handles ties on one-column input", {
  x <- matrix(c(1, 1, 2, 2), ncol = 1)

  expect_equal(depthMedian(x, list(method = "Euclidean")), 1.5)
  expect_equal(depthMedian(depth(x, x, method = "Euclidean")), 1.5)
})

test_that("depthMedian keeps returning a plain vector", {
  set.seed(123)
  x <- matrix(rnorm(100), ncol = 2)
  med <- depthMedian(x, list(method = "Euclidean"))

  expect_null(dim(med))
  expect_equal(length(med), ncol(x))
  expect_equal(med, depthMedian(depth(x, x, method = "Euclidean")))
})

test_that("depthTukey rejects u and X of different dimension", {
  set.seed(123)
  x <- matrix(rnorm(100), ncol = 2)
  u <- matrix(rnorm(10), ncol = 1)

  expect_error(depthTukey(u, x, exact = TRUE), "dimensions must match")
  expect_error(depthTukey(u, x, exact = FALSE), "dimensions must match")
  expect_equal(length(depthTukey(x[1:5, ], x, exact = TRUE)), 5)
})
