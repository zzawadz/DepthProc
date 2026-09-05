context("shared input handling for the depth entry points")

# Every entry point below used to open with its own verbatim copy of the same
# coercion block, and only depthTukey ever gained the dimension check. For
# Mahalanobis, Projection and LP the mismatch reached an Armadillo subtraction
# inside an OpenMP parallel region, where the exception called std::terminate()
# and killed the R session with SIGABRT - not a failure any caller could catch.

test_that("every depth entry point rejects a dimension mismatch", {
  set.seed(801)
  X <- MASS::mvrnorm(80, rep(0, 3), diag(3))
  u <- MASS::mvrnorm(5, rep(0, 2), diag(2))

  expect_error(depthMah(u, X), "'u' has 2 column\\(s\\) but 'X' has 3")
  expect_error(depthProjection(u, X), "dimensions must match")
  expect_error(depthLP(u, X), "dimensions must match")
  expect_error(depthEuclid(u, X), "dimensions must match")
  expect_error(depthTukey(u, X), "dimensions must match")
  expect_error(depthLocal(u, X, depth_params1 = list(method = "Mahalanobis")),
               "dimensions must match")

  # and through the dispatcher, for each method it forwards to
  for (m in c("Mahalanobis", "Projection", "LP", "Euclidean", "Tukey")) {
    expect_error(depth(u, X, method = m), "dimensions must match",
                 info = m)
  }
})

test_that("a mismatch is rejected in both directions", {
  set.seed(802)
  X <- MASS::mvrnorm(80, rep(0, 3), diag(3))
  wide <- MASS::mvrnorm(5, rep(0, 4), diag(4))

  expect_error(depthMah(wide, X), "'u' has 4 column\\(s\\) but 'X' has 3")
})

test_that("the mismatch error is reported against the function the user called", {
  set.seed(803)
  X <- MASS::mvrnorm(50, rep(0, 3), diag(3))
  u <- MASS::mvrnorm(5, rep(0, 2), diag(2))

  # the shared helper is an implementation detail; it must not show up as the
  # call in the condition
  called <- tryCatch(depthProjection(u, X),
                     error = function(e) deparse(conditionCall(e))[1])
  expect_match(called, "^depthProjection\\(")
  expect_false(grepl("coerceDepthInput", called, fixed = TRUE))
})

test_that("data frames and matrices give the same depths", {
  set.seed(804)
  m <- MASS::mvrnorm(60, c(0, 0), diag(2))
  df <- as.data.frame(m)

  # Mahalanobis, not the stochastic Projection default, so equality is testable
  expect_equal(as.numeric(depthMah(df, df)), as.numeric(depthMah(m, m)))
  expect_equal(as.numeric(depthEuclid(df, m)), as.numeric(depthEuclid(m, m)))
  expect_equal(as.numeric(depth(df, df, method = "Mahalanobis")),
               as.numeric(depth(m, m, method = "Mahalanobis")))
  expect_equal(as.numeric(depthLocal(df, df,
                                     depth_params1 = list(method = "Mahalanobis"))),
               as.numeric(depthLocal(m, m,
                                     depth_params1 = list(method = "Mahalanobis"))))
})

test_that("X defaults to u when it is not supplied", {
  set.seed(805)
  m <- MASS::mvrnorm(60, c(0, 0), diag(2))

  expect_equal(as.numeric(depthMah(m)), as.numeric(depthMah(m, m)))
  expect_equal(as.numeric(depthEuclid(m)), as.numeric(depthEuclid(m, m)))
  expect_equal(as.numeric(depth(m, method = "Mahalanobis")),
               as.numeric(depth(m, m, method = "Mahalanobis")))
})

test_that("a bare vector is one column of data and one observation", {
  set.seed(806)
  x <- rnorm(100)

  d <- depthMah(x, x)
  expect_equal(dim(d@X), c(100L, 1L))
  expect_equal(dim(d@u), c(100L, 1L))
  expect_length(as.numeric(d), 100L)

  # a vector u against a matrix X is a single observation, not a column
  X <- MASS::mvrnorm(60, c(0, 0), diag(2))
  one <- depthMah(c(0, 0), X)
  expect_equal(dim(one@u), c(1L, 2L))
  expect_length(as.numeric(one), 1L)
})

test_that("a one-observation reference sample is refused by Mahalanobis depth", {
  X <- matrix(c(1, 2), nrow = 1)
  u <- matrix(c(0.3, -0.7), nrow = 1)

  # arma::cov() of a single row comes back 1 x 1, and the multiplication that
  # followed threw inside an OpenMP loop - SIGABRT, not a catchable error
  expect_error(depthMah(u, X), "not enough to estimate a covariance")

  # an explicit covariance sidesteps the estimate, so it still works
  expect_equal(as.numeric(depthMah(u, X, cov = diag(2))),
               as.numeric(depthEuclid(u, X)))

  # the other kernels cope with a single reference point on their own
  expect_silent(depthProjection(u, X))
  expect_silent(depthLP(u, X))
  expect_silent(depthEuclid(u, X))
})

test_that("depthLocal survives a beta that leaves one point in the neighbourhood", {
  set.seed(807)
  X <- MASS::mvrnorm(50, c(0, 0), diag(2))

  # the neighbourhood used to lose a dimension here: X[keep, ] dropped to a
  # vector and as.matrix() stood it back up as a d x 1 column, so the kernel
  # got the data transposed and aborted the session
  expect_error(depthLocal(X[1:3, ], X, beta = 0.02,
                          depth_params1 = list(method = "Mahalanobis")),
               "not enough to estimate a covariance")

  # with a depth that can work from a single point, it now returns
  d <- depthLocal(X[1:3, ], X, beta = 0.02,
                  depth_params1 = list(method = "Mahalanobis"),
                  depth_params2 = list(method = "Euclidean"))
  expect_length(as.numeric(d), 3L)
  expect_true(all(is.finite(as.numeric(d))))
})
