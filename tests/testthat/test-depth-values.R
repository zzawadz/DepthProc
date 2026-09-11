context("Raw-value depth dispatch")

# .depthValues() is the internal counterpart of depth(): same dispatch, same
# validation, same input coercion, but a bare numeric vector back instead of an
# S4 Depth object. The package's own loops -- .depthLocal() once per row of u,
# fncDepthFM() once per observation point -- used to call depth() and throw the
# object away immediately, paying for the allocation, the slot copies of u and
# X, and the validity check on every iteration.
#
# The risk this file covers is the two dispatch tables drifting apart: a method
# added to .depthMethods but not to .depthValueMethods, or a guard that lives in
# one kernel and not the other. Every method depth() accepts must therefore go
# through .depthValues() and come back with the same numbers.

allMethods <- c("Mahalanobis", "Euclidean", "Projection", "Tukey", "LP")

test_that(".depthValues agrees with depth() for every multivariate method", {
  set.seed(1101)
  X <- MASS::mvrnorm(40, c(0, 5), diag(2) * 5)
  u <- MASS::mvrnorm(10, c(0, 5), diag(2) * 5)

  for (method in allMethods) {
    # Projection and approximate Tukey draw random directions, so the two calls
    # have to start from the same seed to be comparable at all
    set.seed(1102)
    viaObject <- as.numeric(DepthProc:::depth(u, X, method = method))
    set.seed(1102)
    viaValues <- DepthProc:::.depthValues(u, X, method = method)

    expect_equal(viaValues, viaObject, info = method)
    expect_true(is.numeric(viaValues), info = method)
    expect_null(attributes(viaValues), info = method)
  }
})

test_that("every name .depthMethod accepts is also resolvable to a value fun", {
  for (method in names(DepthProc:::.depthMethods)) {
    expect_silent(fun <- DepthProc:::.depthValueMethod(method))
    expect_true(is.function(fun), info = method)
  }
})

test_that("Local, MBD and FM fall back to the S4 route, unwrapped", {
  set.seed(1103)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  local <- DepthProc:::.depthValues(X, X, method = "Local",
                                    depth_params1 = list(method = "Mahalanobis"))
  expect_true(is.numeric(local))
  expect_null(attributes(local))
  expect_equal(local,
               as.numeric(depthLocal(X, X,
                                     depth_params1 = list(method = "Mahalanobis"))))

  xf <- matrix(rnorm(60), ncol = 20)
  mbd <- DepthProc:::.depthValues(xf, xf, method = "MBD")
  expect_true(is.numeric(mbd))
  expect_equal(mbd, as.numeric(fncDepth(xf, xf, method = "MBD")))
})

test_that(".depthValues carries method-specific arguments through", {
  set.seed(1104)
  X <- MASS::mvrnorm(40, c(0, 5), diag(2) * 5)
  u <- MASS::mvrnorm(10, c(0, 5), diag(2) * 5)

  expect_equal(DepthProc:::.depthValues(u, X, method = "LP", pdim = 3, la = 2,
                                        lb = 0.5),
               as.numeric(depthLP(u, X, pdim = 3, la = 2, lb = 0.5)))

  expect_equal(DepthProc:::.depthValues(u, X, method = "Mahalanobis",
                                        cov = diag(2), mean = c(0, 5)),
               as.numeric(depthMah(u, X, cov = diag(2), mean = c(0, 5))))

  set.seed(1105)
  viaValues <- DepthProc:::.depthValues(u, X, method = "Tukey", exact = TRUE)
  set.seed(1105)
  expect_equal(viaValues, as.numeric(depthTukey(u, X, exact = TRUE)))
})

test_that(".depthValues keeps depth()'s validation and coercion", {
  set.seed(1106)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  # the same messages depth() raises, from the one shared validator
  expect_error(DepthProc:::.depthValues(X, X, method = "Nope"),
               "unknown depth method")
  expect_error(DepthProc:::.depthValues(X, X, method = c("LP", "Tukey")),
               "must be a single value")
  expect_error(DepthProc:::.depthValues(X, X, method = 1),
               "must be a character value")
  expect_error(DepthProc:::.depthValues(X, X[, 1, drop = FALSE], method = "LP"),
               "the dimensions must match")

  # a data frame and a bare vector have to be stood up the same way depth()
  # does it -- fncDepthFM() hands the value functions bare columns
  expect_equal(DepthProc:::.depthValues(as.data.frame(X), as.data.frame(X),
                                        method = "Mahalanobis"),
               as.numeric(depthMah(X, X)))
  expect_equal(DepthProc:::.depthValues(X[, 1], X[, 1], method = "Mahalanobis"),
               as.numeric(depthMah(X[, 1], X[, 1])))

  # guards that live in the kernels, not in the S4 wrappers
  expect_error(DepthProc:::.depthValues(X, X[1, , drop = FALSE],
                                        method = "Mahalanobis"),
               "not enough to estimate a covariance matrix")
  expect_error(DepthProc:::.depthValues(X, X, method = "LP",
                                        func = function(x) x),
               "not supported yet")
})

test_that("the S4 wrappers still return what they always did", {
  set.seed(1107)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  expect_s4_class(depthMah(X, X), "DepthMahalanobis")
  expect_s4_class(depthEuclid(X, X), "DepthEuclid")
  expect_s4_class(depthLP(X, X), "DepthLP")
  expect_s4_class(depthProjection(X, X), "DepthProjection")
  expect_s4_class(depthTukey(X, X, exact = TRUE), "DepthTukey")

  dep <- depthMah(X, X)
  expect_equal(dep@u, X)
  expect_equal(dep@X, X)
  expect_equal(dep@method, "Mahalanobis")
})
