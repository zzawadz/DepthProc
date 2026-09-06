context("Depth based location and scatter estimators")

test_that("runifsphere returns points on the unit sphere", {
  set.seed(101)
  x2 <- runifsphere(200)
  x5 <- runifsphere(50, p = 5)

  expect_equal(dim(x2), c(200L, 2L))
  expect_equal(dim(x5), c(50L, 5L))
  expect_equal(sqrt(rowSums(x2 ^ 2)), rep(1, 200))
  expect_equal(sqrt(rowSums(x5 ^ 2)), rep(1, 50))
})

test_that("runifsphere covers the whole sphere rather than one orthant", {
  set.seed(102)
  x <- runifsphere(500)

  expect_true(all(apply(x, 2, min) < -0.5))
  expect_true(all(apply(x, 2, max) > 0.5))
})

test_that("CovLP returns a consistent depth weighted covariance object", {
  set.seed(103)
  x <- MASS::mvrnorm(200, c(0, 0), 3 * diag(2))
  cv <- CovLP(x)

  expect_s4_class(cv, "CovDepthWeighted")
  expect_equal(dim(cv@cov), c(2L, 2L))
  expect_equal(cv@cov, t(cv@cov))
  expect_equal(cv@det, det(cv@cov))
  expect_equal(cv@n.obs, nrow(x))
  expect_equal(cv@X, x)
  expect_equal(cv@center, depthMedian(x, list(method = "LP", pdim = 2,
                                              la = 1, lb = 1)))
})

test_that("CovLP accepts a data frame and matches the matrix result", {
  set.seed(104)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))

  expect_equal(CovLP(as.data.frame(x))@cov, CovLP(x)@cov)
})

test_that("CovLP tracks the scale of the sample", {
  set.seed(105)
  small <- MASS::mvrnorm(300, c(0, 0), diag(2))
  large <- MASS::mvrnorm(300, c(0, 0), 9 * diag(2))

  expect_true(all(diag(CovLP(large)@cov) > diag(CovLP(small)@cov)))
})

test_that("depthLocal returns one depth per row of u", {
  set.seed(106)
  x <- MASS::mvrnorm(80, c(0, 0), diag(2))
  u <- x[1:10, , drop = FALSE]

  dep <- depthLocal(u, x, depth_params1 = list(method = "Mahalanobis"))

  expect_s4_class(dep, "DepthLocal")
  expect_equal(length(dep), nrow(u))
  expect_equal(dep@u, u)
  expect_equal(dep@X, x)
  expect_equal(dep@method, "Local")
  expect_true(all(as.numeric(dep) >= 0))
})

test_that("depthLocal defaults depth_params2 to depth_params1 and X to u", {
  set.seed(107)
  x <- MASS::mvrnorm(60, c(0, 0), diag(2))
  params <- list(method = "Mahalanobis")

  one <- depthLocal(x, depth_params1 = params)
  two <- depthLocal(x, x, depth_params1 = params, depth_params2 = params)

  expect_equal(as.numeric(one), as.numeric(two))
  expect_equal(one@depth_params1, params)
  expect_equal(one@depth_params2, params)
})

test_that("depthLocal is deepest near the centre of a symmetric sample", {
  set.seed(108)
  x <- MASS::mvrnorm(150, c(0, 0), diag(2))
  u <- rbind(c(0, 0), c(4, 4))

  dep <- as.numeric(depthLocal(u, x, depth_params1 = list(method = "Mahalanobis")))

  expect_gt(dep[1], dep[2])
})

test_that("depthMedian accepts a data frame and keeps its column names", {
  set.seed(109)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))
  colnames(x) <- c("a", "b")
  params <- list(method = "Euclidean")

  from_df <- depthMedian(as.data.frame(x), params)

  expect_equal(names(from_df), c("a", "b"))
  expect_equal(unname(from_df), unname(depthMedian(x, params)))
})

test_that("depthMedian averages over the convex hull of tied deepest points", {
  square <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1), ncol = 2)
  params <- list(method = "Euclidean")

  expect_equal(depthMedian(square, params, convex = TRUE), c(0.5, 0.5))
  expect_equal(depthMedian(square, params, convex = FALSE), c(0.5, 0.5))
  expect_equal(depthMedian(depth(square, square, method = "Euclidean"),
                           convex = TRUE), c(0.5, 0.5))
})

test_that("depthMedian rejects depth_params for a Depth object", {
  set.seed(804)
  x <- matrix(rnorm(300), ncol = 3)
  dp <- depth(x, method = "Mahalanobis")

  # the generic offers depth_params and the shared help page documents it, but
  # the Depth method used to leave it out of its own signature, so S4 discarded
  # it silently and returned a median from the already-stored method
  expect_error(depthMedian(dp, depth_params = list(method = "LP")),
               "does not apply")
  expect_error(depthMedian(dp, list(method = "LP")), "does not apply")

  # the supported calls still work, and still agree with the matrix method
  expect_equal(depthMedian(dp),
               depthMedian(x, list(method = "Mahalanobis")))
  expect_equal(depthMedian(dp, convex = TRUE),
               depthMedian(x, list(method = "Mahalanobis"), convex = TRUE))
})

test_that("depthMah defaults cov and mean independently of each other", {
  set.seed(330)
  X <- MASS::mvrnorm(200, c(1, -2), matrix(c(3, 1, 1, 2), 2, 2))
  u <- X[1:20, , drop = FALSE]

  cov_hat <- stats::cov(X)
  mean_hat <- colMeans(X)

  # the four argument combinations must agree once the omitted estimate is the
  # one the function would have computed anyway
  both <- as.numeric(depthMah(u, X, cov = cov_hat, mean = mean_hat))

  expect_equal(as.numeric(depthMah(u, X)), both)
  expect_equal(as.numeric(depthMah(u, X, cov = cov_hat)), both)
  expect_equal(as.numeric(depthMah(u, X, mean = mean_hat)), both)
})

test_that("depthMah honours a cov or mean that differs from the sample estimate", {
  set.seed(331)
  X <- MASS::mvrnorm(200, c(1, -2), matrix(c(3, 1, 1, 2), 2, 2))
  u <- X[1:20, , drop = FALSE]

  default <- as.numeric(depthMah(u, X))

  expect_false(isTRUE(all.equal(
    as.numeric(depthMah(u, X, cov = diag(2))), default)))
  expect_false(isTRUE(all.equal(
    as.numeric(depthMah(u, X, mean = c(0, 0))), default)))

  # a custom mean is a plain relocation of the Mahalanobis distance
  shifted <- as.numeric(depthMah(u, X, cov = diag(2), mean = c(0, 0)))
  expect_equal(shifted, as.numeric(1 / (1 + rowSums(u ^ 2))))
})
