context("Projection depth on degenerate samples")

# A zero MAD needs more than half the projected values to coincide, which in
# practice means either no random direction is flat or all of them are. A merely
# constant column is not enough: a random direction still mixes in the varying
# coordinate, so its MAD stays positive.

test_that("a point matching a fully degenerate sample is maximally deep", {
  x <- matrix(rep(c(1, 1), 20), ncol = 2, byrow = TRUE)

  dep <- depthProjection(matrix(c(1, 1), nrow = 1), x)

  expect_false(any(is.nan(dep)))
  expect_equal(as.numeric(dep), 1)
  expect_equal(unique(as.numeric(depthProjection(x, x))), 1)
})

test_that("a point off a fully degenerate sample is maximally outlying", {
  x <- matrix(rep(c(1, 1), 20), ncol = 2, byrow = TRUE)

  # outside the sample's support in every direction, so the outlyingness ratio
  # is unbounded and the depth is 0 - not 1, and not NaN
  expect_equal(as.numeric(depthProjection(matrix(c(50, 50), nrow = 1), x)), 0)
  expect_equal(as.numeric(depthProjection(matrix(c(1.001, 1), nrow = 1), x)), 0)
  expect_equal(as.numeric(depthProjection(matrix(c(1, 2), nrow = 1), x)), 0)
})

test_that("a sample with more than half its mass at one point splits cleanly", {
  set.seed(601)
  x <- rbind(matrix(0, nrow = 30, ncol = 2), matrix(rnorm(20), ncol = 2))

  dep <- as.numeric(depthProjection(x, x))

  expect_false(any(is.nan(dep)))
  expect_false(any(is.infinite(dep)))
  expect_equal(unique(dep[1:30]), 1)       # the mode itself
  expect_equal(unique(dep[31:40]), 0)      # everything away from it
  expect_equal(as.numeric(depthProjection(matrix(c(99, 99), nrow = 1), x)), 0)
})

test_that("depth() reaches the same guard through its default method", {
  x <- matrix(rep(c(2, 3), 15), ncol = 2, byrow = TRUE)

  expect_equal(as.numeric(depth(matrix(c(2, 3), nrow = 1), x)), 1)
  expect_equal(as.numeric(depth(matrix(c(9, 9), nrow = 1), x)), 0)
})

test_that("a constant column is not degenerate and is unaffected", {
  set.seed(602)
  x <- cbind(rnorm(40), rep(2, 40))

  dep <- depthProjection(x, x)

  expect_false(any(is.nan(dep)))
  expect_true(all(dep > 0 & dep <= 1))
  expect_gt(length(unique(round(as.numeric(dep), 9))), 1)
})

test_that("well conditioned data still ranks the centre above the periphery", {
  set.seed(603)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))
  u <- rbind(c(0, 0), c(4, 4))

  dep <- as.numeric(depthProjection(u, x))

  expect_gt(dep[1], dep[2])
  expect_true(all(dep > 0 & dep <= 1))
})
