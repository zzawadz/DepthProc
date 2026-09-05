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
  expect_error(depth(x, x, method = 3), "must be a character value")
  expect_error(depth(x, x, method = c("Projection", "Tukey")), "not a vector of length 2")
})

test_that("the functional depths accept a single curve given as a vector", {
  set.seed(123)
  X <- matrix(rnorm(20 * 10), nrow = 20)
  u <- rnorm(10)

  expect_equal(fncDepthFM(u, X), fncDepthFM(matrix(u, nrow = 1), X))
  expect_equal(fncDepthMBD(u, X), fncDepthMBD(matrix(u, nrow = 1), X))
  expect_equal(length(fncDepthFM(u, X)), 1)
  expect_equal(length(fncDepthMBD(u, X)), 1)
})

test_that("the functional depths reject u and X observed at different points", {
  set.seed(123)
  X <- matrix(rnorm(20 * 10), nrow = 20)
  u <- matrix(rnorm(6), nrow = 1)

  expect_error(fncDepthFM(u, X), "they must match")
  expect_error(fncDepthMBD(u, X), "they must match")
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

test_that("fncDepthBD agrees with itself when the reference sample is passed", {
  set.seed(4)
  x <- matrix(rnorm(40), ncol = 2)

  expect_equal(fncDepthBD(x), fncDepthBD(x, x))

  set.seed(9)
  y <- matrix(rnorm(200), ncol = 5)

  expect_equal(fncDepthBD(y), fncDepthBD(y, y))
})

test_that("band depth stays within the modified band depth", {
  set.seed(11)
  x <- matrix(rnorm(150), ncol = 5)

  expect_true(all(fncDepthBD(x) <= fncDepthMBD(x) + 1e-12))
  expect_true(all(fncDepthBD(x) >= 0))
})

test_that("depthDensity works without the np package being attached", {
  skip_if_not_installed("np")

  # np builds the bandwidth call as quote(npudensbw) and evaluates it in its
  # caller's frame, so the name has to resolve from DepthProc's namespace
  expect_false("package:np" %in% search())

  set.seed(31)
  x <- rnorm(60)
  y <- x + rnorm(60, sd = 0.5)

  dens <- suppressWarnings(depthDensity(x, y, nx = 4, ny = 8))

  expect_s4_class(dens, "DepthDensity")
  expect_equal(dim(dens@density), c(8L, 4L))
})
