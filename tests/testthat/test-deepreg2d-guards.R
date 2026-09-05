context("deepReg2d input validation")

test_that("deepReg2d rejects input it cannot fit instead of crashing", {
  expect_error(deepReg2d(1, 2), "at least 2 observations")
  expect_error(deepReg2d(numeric(0), numeric(0)), "at least 2 observations")
  expect_error(deepReg2d(1:5, 1:3), "they must match")
})

test_that("deepReg2d fits a sample whose best regression depth is zero", {
  # two points give a single candidate pair scoring depth 0, which used to
  # leave the C++ coefficient vector empty and read past its end
  fit <- deepReg2d(c(1, 2), c(3, 4))

  expect_s4_class(fit, "DeepReg2d")
  expect_equal(fit@coef, c(2, 1))
  expect_equal(fit@depth, 0)
})

test_that("deepReg2d recovers an exactly collinear sample", {
  fit <- deepReg2d(c(1, 2, 3), c(2, 4, 6))

  expect_equal(fit@coef, c(0, 2))
})

test_that("deepReg2d still recovers a clean linear relationship", {
  set.seed(704)
  x <- rnorm(100)
  y <- 2 + 3 * x + rnorm(100, sd = 0.3)

  fit <- deepReg2d(x, y)

  expect_equal(fit@coef, c(2, 3), tolerance = 0.2)
  expect_gt(fit@depth, 0)
})
