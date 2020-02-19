context("DDPlot")

test_that("Basic dd plot", {
  set.seed(123)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))
  y <- MASS::mvrnorm(100, c(0, 0), diag(2)) -5
  pl <- ddPlot(
    x = x,
    y = y, title = "Difference in position",
    name = "X dist", name_y = "Y dist")

  pl
})
