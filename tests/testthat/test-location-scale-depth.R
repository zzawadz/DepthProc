context("Location-scale (Student) depth")

test_that("lsdSampleMaxDepth returns a location and scale estimate", {
  set.seed(401)
  x <- rnorm(200)

  res <- lsdSampleMaxDepth(x)

  expect_s4_class(res, "LSDepth")
  expect_equal(length(res@max_depth), 1L)
  expect_equal(length(res@mu), 1L)
  expect_equal(length(res@sigma), 1L)
  expect_gt(res@sigma, 0)
  expect_gt(res@max_depth, 0)
  expect_lte(res@max_depth, length(x))
  expect_gt(res@mu, min(x))
  expect_lt(res@mu, max(x))
})

test_that("lsdSampleMaxDepth is location and scale equivariant", {
  set.seed(402)
  x <- rnorm(200)

  base <- lsdSampleMaxDepth(x)
  shifted <- lsdSampleMaxDepth(x + 10)
  scaled <- lsdSampleMaxDepth(3 * x)

  expect_equal(shifted@mu, base@mu + 10)
  expect_equal(shifted@sigma, base@sigma)
  expect_equal(shifted@max_depth, base@max_depth)

  expect_equal(scaled@mu, 3 * base@mu)
  expect_equal(scaled@sigma, 3 * base@sigma)
  expect_equal(scaled@max_depth, base@max_depth)
})

test_that("lsdSampleMaxDepth tracks a shifted sample", {
  set.seed(403)
  x <- rnorm(300, mean = 5)

  res <- lsdSampleMaxDepth(x)

  expect_lt(abs(res@mu - 5), 0.5)
})

test_that("lsdSampleDepthContours returns one entry per requested depth", {
  set.seed(404)
  x <- rnorm(200)
  depths <- c(0.1, 0.2, 0.3)

  cont <- lsdSampleDepthContours(x, depth = depths)

  expect_s4_class(cont, "LSDepthContour")
  expect_equal(length(cont), length(depths))
  expect_equal(cont@cont_depth, depths)
  expect_equal(cont@sample, sort(x))
  expect_equal(names(cont[[1]]),
               c("depth", "cont.exist", "mubound", "lbound", "ubound"))
})

test_that("each contour has a lower bound below its upper bound", {
  set.seed(405)
  cont <- lsdSampleDepthContours(rnorm(200), depth = c(0.1, 0.3))

  for (i in seq_along(cont)) {
    expect_true(cont[[i]]$cont.exist)
    expect_true(all(cont[[i]]$lbound <= cont[[i]]$ubound))
    expect_equal(length(cont[[i]]$lbound), length(cont[[i]]$mubound))
    expect_equal(length(cont[[i]]$ubound), length(cont[[i]]$mubound))
  }
})

test_that("a deeper contour sits inside a shallower one", {
  set.seed(406)
  cont <- lsdSampleDepthContours(rnorm(300), depth = c(0.1, 0.3))

  shallow <- lsdGetContour(cont, 0.1)
  deep <- lsdGetContour(cont, 0.3)

  expect_gt(min(deep$mubound), min(shallow$mubound))
  expect_lt(max(deep$mubound), max(shallow$mubound))
})

test_that("lsdGetContour returns a stored contour without recomputing", {
  set.seed(407)
  x <- rnorm(200)
  cont <- lsdSampleDepthContours(x, depth = c(0.1, 0.2))

  expect_identical(lsdGetContour(cont, 0.1), cont[[1]])
  expect_identical(lsdGetContour(cont, 0.2), cont[[2]])
})

test_that("lsdGetContour computes a contour that was not stored", {
  set.seed(408)
  x <- rnorm(200)
  cont <- lsdSampleDepthContours(x, depth = c(0.1, 0.2))

  fresh <- lsdGetContour(cont, 0.3)

  expect_true(fresh$cont.exist)
  expect_equal(fresh$depth, 0.3 * length(x))
  expect_true(all(fresh$lbound <= fresh$ubound))
})

test_that("lsdAddContour draws onto an existing plot", {
  set.seed(409)
  x <- lsdSampleDepthContours(rnorm(200), depth = c(0.1, 0.3))

  pdf(NULL)
  on.exit(dev.off())

  plot(x)
  expect_silent(lsdAddContour(x, 0.1, col = "grey50"))
  expect_silent(lsdAddContour(x, 0.3, col = "grey10", border = "red"))
})

test_that("plot on an LSDepthContour runs for all and for chosen contours", {
  set.seed(410)
  x <- lsdSampleDepthContours(rnorm(200), depth = c(0.1, 0.2, 0.3))

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(plot(x))
  expect_silent(plot(x, cont = c(0.1, 0.3), col = c("grey20", "grey60")))
})
