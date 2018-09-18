context("Scale curve tests")

test_that(
"Fix QH6114 qhull precision error:
initial simplex is not convex. Distance=8.7e-19
for xdata", {

  xdata <- structure(
    c(-0.19607854, -0.19607854, -0.26348042, 0.002450943,
      0.002450943, 0.051470518),
    .Dim = 3:2,
    .Dimnames = list(NULL, c("x", "y"))
  )
  sc <- scaleCurve(xdata)
  expect_equal(unique(sc@.Data), 0)
})

test_that("Scale curve plot", {
  set.seed(123)
  x <- MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = 3 * diag(2))
  y <- mvtnorm::rmvt(n = 100, sigma = diag(2), df = 2)
  y <- as.data.frame(y)
  x <- as.data.frame(x)

  pp <- scaleCurve(x, y,
    depth_params = list(method = "Projection"),
    name_y = "Y")
  pp <- getPlot(pp)
  expect_doppelganger("Scale curve plot X vs Y", pp)
})

test_that("Scale curve api call", {
  x <- list(1,2)
  expect_error(scaleCurve(x))
  expect_error(scaleCurve(cbind(1,1), x))
})
