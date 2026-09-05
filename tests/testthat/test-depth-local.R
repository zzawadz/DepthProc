context("Local depth")

# depthLocal() runs a full depth() computation of X against a freshly
# symmetrised 2 * nrow(X) sample for every row of u, so every example of it in
# the package is wrapped in \dontrun{}. That left the inner loop with no test at
# any size. These use n = 30 and Mahalanobis depth -- Projection is stochastic
# and would not compare equal between two calls.

localParams <- list(method = "Mahalanobis")

test_that("depthLocal returns one depth per row of u", {
  set.seed(901)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  dep <- depthLocal(X, X, depth_params1 = localParams)

  expect_s4_class(dep, "DepthLocal")
  expect_equal(length(dep), nrow(X))
  expect_true(all(dep > 0 & dep <= 1))
  expect_equal(dep@method, "Local")
  expect_equal(dep@depth_params1, localParams)
  expect_equal(dep@depth_params2, localParams)
})

test_that("depthLocal is deterministic for a deterministic inner depth", {
  set.seed(902)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  expect_equal(as.numeric(depthLocal(X, X, depth_params1 = localParams)),
               as.numeric(depthLocal(X, X, depth_params1 = localParams)))
})

test_that("beta = 1 keeps the whole sample, so local depth is global depth", {
  set.seed(903)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  # with beta = 1 the neighbourhood cutoff is the minimum depth, every point is
  # kept, and the second stage sees X itself
  expect_equal(
    as.numeric(depthLocal(X, X, beta = 1, depth_params1 = localParams)),
    as.numeric(depthMah(X, X))
  )
})

test_that("depthLocal honours a second depth different from the first", {
  set.seed(904)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  mixed <- depthLocal(X, X, depth_params1 = localParams,
                      depth_params2 = list(method = "Euclidean"))

  expect_equal(mixed@depth_params2, list(method = "Euclidean"))
  expect_false(isTRUE(all.equal(
    as.numeric(mixed),
    as.numeric(depthLocal(X, X, depth_params1 = localParams))
  )))
})

test_that("depthLocal accepts u smaller than X, and a data frame", {
  set.seed(905)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)
  u <- X[1:5, , drop = FALSE]

  few <- depthLocal(u, X, depth_params1 = localParams)

  expect_equal(length(few), 5L)
  expect_equal(as.numeric(few),
               as.numeric(depthLocal(X, X,
                                     depth_params1 = localParams))[1:5])
  expect_equal(
    as.numeric(depthLocal(as.data.frame(u), as.data.frame(X),
                          depth_params1 = localParams)),
    as.numeric(few)
  )
})

test_that("depthLocal handles one-dimensional data", {
  set.seed(906)
  X <- matrix(rnorm(30), ncol = 1)

  # the symmetrised sample used to be assembled with apply(), which returns a
  # bare vector when X has one column and needed a shape correction
  dep <- depthLocal(X, X, depth_params1 = localParams)

  expect_equal(length(dep), nrow(X))
  expect_true(all(dep > 0 & dep <= 1))
  expect_equal(as.numeric(depthLocal(X, X, beta = 1,
                                     depth_params1 = localParams)),
               as.numeric(depthMah(X, X)))
})

test_that("a small beta leaves a usable neighbourhood", {
  set.seed(907)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  # beta small enough that the neighbourhood is a couple of points; the
  # subsetting used to drop to a vector here
  expect_equal(length(depthLocal(X, X, beta = 0.1,
                                 depth_params1 = localParams)), nrow(X))
})

test_that("depth(method = 'Local') reaches depthLocal", {
  set.seed(908)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  expect_equal(
    as.numeric(depth(X, X, method = "Local", depth_params1 = localParams)),
    as.numeric(depthLocal(X, X, depth_params1 = localParams))
  )
})

test_that("depthContour draws the Local method at a small grid", {
  set.seed(909)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(depthContour(
    X, n = 6, legend = FALSE,
    depth_params = list(method = "Local", depth_params1 = localParams)
  ))
})

test_that("depthLocal matches the definition transcribed row by row", {
  set.seed(910)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)
  beta <- 0.5

  # P_x = half the sample, half its reflection 2x - X. The reflection is built
  # one row at a time here on purpose: that is the shape the vectorised
  # expression in .depthLocal has to keep producing.
  reference <- vapply(seq_len(nrow(X)), FUN.VALUE = 0, function(i) {
    u <- X[i, , drop = FALSE]
    reflected <- X
    for (j in seq_len(nrow(X))) {
      reflected[j, ] <- 2 * as.numeric(u) - X[j, ]
    }
    d <- as.numeric(depthMah(X, rbind(X, reflected)))
    keep <- signif(d, 6) >= signif(quantile(d, probs = 1 - beta), 6)
    as.numeric(depthMah(u, X[keep, , drop = FALSE]))
  })

  expect_equal(
    as.numeric(depthLocal(X, X, beta = beta, depth_params1 = localParams)),
    reference
  )
})
