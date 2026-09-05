context("Robust regression, dispersion test, binning and plot builders")

test_that("deepReg2d recovers a clean linear relationship", {
  set.seed(501)
  x <- rnorm(100)
  y <- 2 + 3 * x + rnorm(100, sd = 0.3)

  fit <- deepReg2d(x, y)

  expect_s4_class(fit, "DeepReg2d")
  expect_equal(length(fit@coef), 2L)
  expect_equal(fit@coef, c(2, 3), tolerance = 0.2)
  expect_gt(fit@depth, 0)
  expect_lte(fit@depth, length(x))
})

test_that("deepReg2d resists a cluster of outliers that moves lm", {
  set.seed(502)
  x <- rnorm(100)
  y <- 2 + 3 * x + rnorm(100, sd = 0.3)
  x_out <- c(x, rep(6, 12))
  y_out <- c(y, rep(-40, 12))

  deep <- deepReg2d(x_out, y_out)@coef
  ols <- unname(coef(lm(y_out ~ x_out)))

  expect_lt(abs(deep[2] - 3), abs(ols[2] - 3))
})

test_that("trimProjReg2d recovers a clean linear relationship", {
  set.seed(503)
  x <- rnorm(100)
  y <- 2 + 3 * x + rnorm(100, sd = 0.3)

  fit <- trimProjReg2d(x, y)

  expect_s4_class(fit, "TrimReg2d")
  expect_equal(length(fit@coef), 2L)
  expect_equal(unname(fit@coef), c(2, 3), tolerance = 0.2)
})

test_that("trimProjReg2d keeps more observations as alpha shrinks", {
  set.seed(504)
  x <- rnorm(150)
  y <- 1 + 2 * x + rnorm(150, sd = 0.5)

  light <- trimProjReg2d(x, y, alpha = 0.05)@coef
  heavy <- trimProjReg2d(x, y, alpha = 0.4)@coef

  expect_equal(length(light), 2L)
  expect_equal(length(heavy), 2L)
  expect_equal(unname(light), c(1, 2), tolerance = 0.3)
  expect_equal(unname(heavy), c(1, 2), tolerance = 0.3)
})

test_that("abline accepts both robust regression objects", {
  set.seed(505)
  x <- rnorm(60)
  y <- 1 + 2 * x + rnorm(60, sd = 0.3)

  pdf(NULL)
  on.exit(dev.off())
  plot(x, y)

  expect_silent(abline(deepReg2d(x, y)))
  expect_silent(abline(trimProjReg2d(x, y)))
})

test_that("mWilcoxonTest returns an htest labelled for dispersion", {
  set.seed(506)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))
  y <- MASS::mvrnorm(100, c(0, 0), diag(2) * 1.4)

  res <- mWilcoxonTest(x, y)

  expect_s3_class(res, "htest")
  expect_equal(res$method,
               "Multivariate Wilcoxon test for equality of dispersion")
  expect_equal(unname(res$null.value), 1)
  expect_equal(names(res$null.value), "dispersion ratio")
  expect_gte(res$p.value, 0)
  expect_lte(res$p.value, 1)
})

test_that("mWilcoxonTest detects a large scale difference and ignores none", {
  set.seed(507)
  x <- MASS::mvrnorm(150, c(0, 0), diag(2))
  same <- MASS::mvrnorm(150, c(0, 0), diag(2))
  wider <- MASS::mvrnorm(150, c(0, 0), 9 * diag(2))

  expect_lt(mWilcoxonTest(x, wider)$p.value, 0.01)
  expect_gt(mWilcoxonTest(x, same)$p.value, 0.01)
})

test_that("mWilcoxonTest passes depth_params through and takes an alternative", {
  set.seed(508)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))
  y <- MASS::mvrnorm(100, c(0, 0), 4 * diag(2))

  res <- mWilcoxonTest(x, y, depth_params = list(method = "LP"))

  expect_s3_class(res, "htest")
  expect_lt(res$p.value, 0.05)

  one_sided <- mWilcoxonTest(x, y, alternative = "greater",
                             depth_params = list(method = "Mahalanobis"))

  expect_equal(one_sided$alternative, "greater")
})

test_that("binningDepth2D bins every observation into a square grid", {
  set.seed(509)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  b <- binningDepth2D(x, nbins = 8, k = 1)

  expect_s4_class(b, "BinnDepth2d")
  expect_equal(dim(b@freq), c(8L, 8L))
  expect_equal(sum(b@freq), nrow(x))
  expect_equal(length(b@mid_x), 8L)
  expect_equal(length(b@mid_y), 8L)
  expect_equal(b@input_data, x)
  expect_true(all(diff(b@breaks_x) > 0))
  expect_true(all(diff(b@breaks_y) > 0))
})

test_that("binningDepth2D drops the marginal bins on request", {
  set.seed(510)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  full <- binningDepth2D(x, nbins = 8, k = 1, remove_borders = FALSE)
  inner <- binningDepth2D(x, nbins = 8, k = 1, remove_borders = TRUE)

  expect_equal(dim(inner@freq), c(6L, 6L))
  expect_equal(sum(inner@freq), sum(full@freq[-c(1, 8), -c(1, 8)]))
  expect_lte(sum(inner@freq), sum(full@freq))
  expect_equal(length(inner@mid_x), 6L)
})

test_that("binningDepth2D supports the LP binning method", {
  set.seed(511)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  b <- binningDepth2D(x, binmethod = "LP", nbins = 6, k = 1)

  expect_s4_class(b, "BinnDepth2d")
  expect_equal(dim(b@freq), c(6L, 6L))
  expect_equal(sum(b@freq), nrow(x))
})

test_that("plot on a BinnDepth2d runs", {
  set.seed(512)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))
  b <- binningDepth2D(x)

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(plot(b))
  expect_silent(plot(b, add_mid = FALSE))
})

test_that("ddMvnorm compares the sample against a fitted normal sample", {
  set.seed(513)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  dd <- ddMvnorm(x, depth_params = list(method = "Mahalanobis"))

  expect_s4_class(dd, "DDPlot")
  expect_equal(dd@title, "ddMvnorm")
  expect_equal(length(dd@X), nrow(x))
  expect_equal(length(dd@Y), nrow(x))
  expect_equal(as.numeric(dd@X),
               as.numeric(depth(x, x, method = "Mahalanobis")))
})

test_that("ddMvnorm accepts the robust branch and a custom theoretical size", {
  set.seed(514)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  dd <- ddMvnorm(x, size = 500, robust = TRUE,
                 depth_params = list(method = "Mahalanobis"))

  expect_s4_class(dd, "DDPlot")
  expect_equal(length(dd@Y), nrow(x))
})

test_that("getPlot and plot work on a DDPlot", {
  set.seed(515)
  x <- MASS::mvrnorm(150, c(0, 0), diag(2))
  y <- MASS::mvrnorm(150, c(0, 0), 4 * diag(2))

  dd <- ddPlot(x, y, name = "X dist", name_y = "Y dist")
  p <- getPlot(dd)

  expect_s3_class(p, "ggplot")

  pdf(NULL)
  on.exit(dev.off())
  expect_silent(plot(dd))
})

test_that("depthContour draws and validates its levels argument", {
  set.seed(516)
  x <- MASS::mvrnorm(150, c(0, 0), diag(2))

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(depthContour(x, n = 20, legend = FALSE,
                             depth_params = list(method = "Mahalanobis")))
  expect_error(depthContour(x, n = 10, levels = c(1, 2)),
               "Levels must be numeric vector of length 1")
})

test_that("depthContour honours the convexhull contour method", {
  set.seed(517)
  x <- MASS::mvrnorm(150, c(0, 0), diag(2))

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(depthContour(x, n = 20, legend = FALSE, points = TRUE,
                             contour_method = "convexhull",
                             depth_params = list(method = "Mahalanobis")))
})

test_that("depthPersp returns a lattice surface", {
  set.seed(518)
  x <- MASS::mvrnorm(150, c(0, 0), diag(2))

  p <- depthPersp(x, n = 15, depth_params = list(method = "Mahalanobis"))

  expect_s3_class(p, "trellis")
})

test_that("depthDensity returns matrices matching the grid", {
  skip_if_not_installed("np")

  set.seed(519)
  x <- rnorm(60)
  y <- x + rnorm(60, sd = 0.5)

  dens <- suppressWarnings(depthDensity(x, y, nx = 4, ny = 8))

  expect_s4_class(dens, "DepthDensity")
  expect_equal(length(dens@xgrid), 4L)
  expect_equal(length(dens@ygrid), 8L)
  expect_equal(dim(dens@density), c(8L, 4L))
  expect_equal(dim(dens@density_raw), c(8L, 4L))
  expect_equal(dim(dens@dep_scale), c(8L, 4L))
  expect_equal(dens@density, dens@density_raw / dens@dep_scale)
})

test_that("plot on a DepthDensity runs for both density types", {
  skip_if_not_installed("np")

  set.seed(520)
  x <- rnorm(60)
  y <- x + rnorm(60, sd = 0.5)
  dens <- suppressWarnings(depthDensity(x, y, nx = 4, ny = 8))

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(plot(dens, type = "depth"))
  expect_silent(plot(dens, type = "raw"))
})
