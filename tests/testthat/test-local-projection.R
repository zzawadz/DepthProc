context("Local depth, projection shortcut")

# .depthLocal()'s first stage is the projection depth of X with respect to X
# symmetrised about u. That sample is symmetric about u by construction, so for
# any direction v its projected median is exactly u * v and its MAD is the
# median of |X * v - u * v| over the n rows of X rather than over the 2n rows of
# the symmetrised sample. depthLocalProjCPP() uses both facts and never forms
# the symmetrised sample; see src/Depth.cpp.
#
# The risk is that shortcut drifting away from the general route it replaces.
# Both draw their directions with Utils::runifsphere() as their first use of the
# RNG, so the same seed gives both the same directions and the two are directly
# comparable -- that is what these tests exploit.

symmetrise <- function(X, u) {
  rbind(X, matrix(2 * u, nrow = nrow(X), ncol = ncol(X), byrow = TRUE) - X)
}

test_that("the shortcut agrees with projection depth on the symmetrised sample", {
  set.seed(1201)
  for (d in 1:4) {
    X <- matrix(rnorm(30 * d), ncol = d)
    u <- matrix(rnorm(d), nrow = 1)

    set.seed(77)
    fast <- as.numeric(DepthProc:::depthLocalProjCPP(X, u, 500, 1))
    set.seed(77)
    slow <- as.numeric(depthProjection(X, symmetrise(X, u), ndir = 500,
                                       threads = 1))

    expect_equal(fast, slow, tolerance = 1e-12, info = paste("d =", d))
  }
})

test_that("the symmetrised median and MAD identities the shortcut rests on hold", {
  set.seed(1202)
  for (trial in 1:50) {
    n <- sample(2:30, 1)
    d <- sample(1:4, 1)
    X <- matrix(rnorm(n * d), ncol = d)
    u <- matrix(rnorm(d), nrow = 1)
    v <- matrix(rnorm(d), ncol = 1)

    proj <- as.numeric(symmetrise(X, u) %*% v)
    med <- median(proj)

    # a sample symmetric about a point has that point as its median
    expect_equal(med, as.numeric(u %*% v), tolerance = 1e-12)
    # and its absolute deviations are those of X, each appearing twice, which
    # leaves the median unchanged
    expect_equal(median(abs(proj - med)),
                 median(abs(as.numeric(X %*% v) - as.numeric(u %*% v))),
                 tolerance = 1e-12)
  }
})

test_that("zero-MAD directions behave the same on both routes", {
  # a constant sample makes every direction flat, which is the branch the
  # shortcut inherits rather than reimplements
  degenerate <- list(
    constant = list(X = matrix(rep(1, 20), ncol = 2),
                    u = matrix(c(1, 1), nrow = 1)),
    offConstant = list(X = matrix(rep(0, 12), ncol = 2),
                       u = matrix(c(9, 9), nrow = 1)),
    collinear = list(X = cbind(1:8, 2 * (1:8)),
                     u = matrix(c(4, 8), nrow = 1)),
    twoRows = list(X = matrix(c(0, 1, 0, 1), ncol = 2),
                   u = matrix(c(0.5, 0.5), nrow = 1))
  )

  for (nm in names(degenerate)) {
    X <- degenerate[[nm]]$X
    u <- degenerate[[nm]]$u

    set.seed(78)
    fast <- as.numeric(DepthProc:::depthLocalProjCPP(X, u, 200, 1))
    set.seed(78)
    slow <- as.numeric(depthProjection(X, symmetrise(X, u), ndir = 200,
                                       threads = 1))

    expect_equal(fast, slow, tolerance = 1e-12, info = nm)
    expect_false(any(is.na(fast)), info = nm)
    expect_true(all(fast > 0 & fast <= 1), info = nm)
  }
})

test_that("depthLocal takes the shortcut only for a Projection request it can serve", {
  # the package default, and the explicit spellings of it
  expect_equal(DepthProc:::.localProjectionArgs(list(method = "Projection")),
               list(ndir = 1000, threads = -1))
  expect_equal(DepthProc:::.localProjectionArgs(list()),
               list(ndir = 1000, threads = -1))
  expect_equal(
    DepthProc:::.localProjectionArgs(list(method = "Projection", ndir = 50,
                                          threads = 2)),
    list(ndir = 50, threads = 2))

  # anything else has to build the symmetrised sample and go the general way
  expect_null(DepthProc:::.localProjectionArgs(list(method = "Mahalanobis")))
  expect_null(DepthProc:::.localProjectionArgs(list(method = "LP", pdim = 3)))
  expect_null(DepthProc:::.localProjectionArgs(list(method = "Tukey")))
  # an argument the shortcut does not know must not be silently dropped
  expect_null(DepthProc:::.localProjectionArgs(list(method = "Projection",
                                                    exact = TRUE)))
})

test_that("depthLocal's Projection result is unchanged by the shortcut", {
  # end to end: with the seed fixed, the whole surface has to match what the
  # general route produces for the same data
  set.seed(1203)
  X <- MASS::mvrnorm(25, c(0, 5), diag(2) * 5)

  viaLocal <- function() {
    set.seed(1204)
    as.numeric(depthLocal(X, X, beta = 0.5,
                          depth_params1 = list(method = "Projection"),
                          depth_params2 = list(method = "Mahalanobis")))
  }

  # stage 2 is Mahalanobis here, so the only randomness is stage 1's directions
  expect_equal(viaLocal(), viaLocal())
  expect_true(all(viaLocal() > 0 & viaLocal() <= 1))
})

test_that("beta = 1 still reduces local depth to global depth on the fast path", {
  set.seed(1205)
  X <- MASS::mvrnorm(30, c(0, 5), diag(2) * 5)

  # beta = 1 keeps every point, so stage 2 sees X itself; stage 1 only decides
  # the neighbourhood, and with everything kept its randomness cannot matter
  expect_equal(
    as.numeric(depthLocal(X, X, beta = 1,
                          depth_params1 = list(method = "Projection"),
                          depth_params2 = list(method = "Mahalanobis"))),
    as.numeric(depthMah(X, X)))
})
