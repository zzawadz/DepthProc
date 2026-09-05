context("DDPlot")

test_that("ddPlot returns the depths of the pooled sample against each set", {
  set.seed(123)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))
  y <- MASS::mvrnorm(100, c(0, 0), diag(2)) - 5
  params <- list(method = "Mahalanobis")  # Projection is stochastic

  pl <- ddPlot(x = x, y = y, title = "Difference in position",
               name = "X dist", name_y = "Y dist", depth_params = params)

  expect_s4_class(pl, "DDPlot")
  expect_identical(pl@title, "Difference in position")

  # both axes hold the depth of the *pooled* sample, measured against x for one
  # axis and against y for the other - swapping them is the failure this pins
  pooled <- rbind(x, y)
  expect_equal(as.vector(pl@X), as.vector(depth(pooled, x, method = "Mahalanobis")))
  expect_equal(as.vector(pl@Y), as.vector(depth(pooled, y, method = "Mahalanobis")))

  # y is shifted well away from x, so its own depths must dominate
  expect_gt(mean(as.vector(pl@Y)[101:200]), mean(as.vector(pl@X)[101:200]))
})

test_that("ddPlot keeps the data set names and puts them on the axes", {
  set.seed(124)
  x <- MASS::mvrnorm(60, c(0, 0), diag(2))
  y <- MASS::mvrnorm(60, c(1, 1), diag(2))
  params <- list(method = "Mahalanobis")

  named <- ddPlot(x, y, name = "X dist", name_y = "Y dist", depth_params = params)
  expect_identical(named@name, "X dist")
  expect_identical(named@name_y, "Y dist")

  p <- getPlot(named)
  expect_identical(p$labels$x, "X dist depth")
  expect_identical(p$labels$y, "Y dist depth")

  # the defaults reproduce the labels this plot has always carried
  plain <- ddPlot(x, y, depth_params = params)
  pp <- getPlot(plain)
  expect_identical(pp$labels$x, "X depth")
  expect_identical(pp$labels$y, "Y depth")
})

test_that("ddPlot rejects samples of differing dimension", {
  set.seed(125)
  x <- MASS::mvrnorm(40, c(0, 0), diag(2))
  y <- MASS::mvrnorm(40, rep(0, 3), diag(3))

  expect_error(ddPlot(x, y), "ncol\\(x\\) != ncol\\(y\\)")
})

test_that("indexLiu counts the points away from the diagonal", {
  set.seed(126)
  x <- MASS::mvrnorm(80, c(0, 0), diag(2))
  y <- MASS::mvrnorm(80, c(3, 3), diag(2))
  pl <- ddPlot(x, y, depth_params = list(method = "Mahalanobis"))

  gaps <- abs(as.vector(pl@X) - as.vector(pl@Y))
  expect_equal(indexLiu(pl, c(0, 0.1, 0.5)),
               c(sum(gaps > 0), sum(gaps > 0.1), sum(gaps > 0.5)))
  # the index is non-increasing in gamma
  expect_true(!is.unsorted(rev(indexLiu(pl, seq(0, 1, by = 0.1)))))
})

test_that("ddPlot records which sample every point came from", {
  set.seed(127)
  x <- MASS::mvrnorm(50, c(0, 0), diag(2))
  y <- MASS::mvrnorm(30, c(2, 2), diag(2))
  params <- list(method = "Mahalanobis")

  pl <- ddPlot(x, y, name = "Left", name_y = "Right", depth_params = params)

  # both axes hold depths of the pooled sample, so the origin of a point is not
  # recoverable from its coordinates - the slot is the only record of it
  expect_s3_class(pl@sample, "factor")
  expect_equal(levels(pl@sample), c("Left", "Right"))
  expect_equal(as.character(pl@sample), rep(c("Left", "Right"), c(50, 30)))
  expect_equal(length(pl@sample), length(pl@X))
})

test_that("getPlot colours the points by sample and labels the legend", {
  set.seed(128)
  x <- MASS::mvrnorm(50, c(0, 0), diag(2))
  y <- MASS::mvrnorm(50, c(2, 2), diag(2))
  params <- list(method = "Mahalanobis")

  p <- getPlot(ddPlot(x, y, name = "Left", name_y = "Right",
                      depth_params = params))

  expect_s3_class(p, "ggplot")
  expect_equal(as.character(p$layers[[1]]$data$sample),
               rep(c("Left", "Right"), each = 50))

  built <- ggplot2::ggplot_build(p)
  colours <- unique(built$data[[1]]$colour)
  expect_equal(length(colours), 2L)
  # the first sample keeps the blue every DD plot has been drawn in
  expect_equal(colours[1], "#0072B2")
})

test_that("color_by_sample = FALSE reproduces the single-colour plot", {
  set.seed(129)
  x <- MASS::mvrnorm(40, c(0, 0), diag(2))
  y <- MASS::mvrnorm(40, c(2, 2), diag(2))
  params <- list(method = "Mahalanobis")

  plain <- ddPlot(x, y, depth_params = params, color_by_sample = FALSE)

  expect_equal(length(plain@sample), 0L)

  built <- ggplot2::ggplot_build(getPlot(plain))
  expect_equal(unique(built$data[[1]]$colour), "blue")
})

test_that("a DDPlot with one named sample is not split into two colours", {
  set.seed(130)
  x <- MASS::mvrnorm(40, c(0, 0), diag(2))
  y <- MASS::mvrnorm(40, c(2, 2), diag(2))

  # name and name_y both default to their own values, but a caller can repeat
  # one; a single level is not something to colour by
  same <- ddPlot(x, y, name = "S", name_y = "S",
                 depth_params = list(method = "Mahalanobis"))

  expect_equal(levels(same@sample), "S")

  built <- ggplot2::ggplot_build(getPlot(same))
  expect_equal(unique(built$data[[1]]$colour), "blue")
})

test_that("ddMvnorm still draws one colour, having only one data set", {
  set.seed(131)
  x <- MASS::mvrnorm(60, c(0, 0), diag(2))

  dd <- ddMvnorm(x, depth_params = list(method = "Mahalanobis"))

  expect_equal(length(dd@sample), 0L)

  built <- ggplot2::ggplot_build(getPlot(dd))
  expect_equal(unique(built$data[[1]]$colour), "blue")
})
