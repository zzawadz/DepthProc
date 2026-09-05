context("binningDepth2D bin construction")

test_that("binningDepth2D handles nbins small enough to leave only border bins", {
  set.seed(701)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))

  b <- binningDepth2D(x, nbins = 2)

  expect_s4_class(b, "BinnDepth2d")
  expect_equal(dim(b@freq), c(2L, 2L))
  expect_equal(sum(b@freq), nrow(x))
})

test_that("binningDepth2D is unchanged for the usual bin counts", {
  set.seed(702)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))

  for (nbins in c(4, 8, 12)) {
    b <- binningDepth2D(x, nbins = nbins)
    expect_equal(sum(b@freq), nrow(x))
    expect_equal(nrow(b@freq), ncol(b@freq))
  }
})

test_that("binningDepth2D still drops the marginal bins on request", {
  set.seed(703)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  full <- binningDepth2D(x, nbins = 8, remove_borders = FALSE)
  inner <- binningDepth2D(x, nbins = 8, remove_borders = TRUE)

  expect_equal(dim(inner@freq), c(6L, 6L))
  expect_equal(sum(inner@freq), sum(full@freq[-c(1, 8), -c(1, 8)]))
})

test_that("binningDepth2D returns usable bin centres for two bins", {
  set.seed(705)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  b <- binningDepth2D(x, nbins = 2)

  # the two border bins are unbounded, so their centres have no finite
  # neighbour to be offset from and used to come back as Inf
  expect_true(all(is.finite(b@mid_x)))
  expect_true(all(is.finite(b@mid_y)))
  expect_lt(b@mid_x[1], b@breaks_x[2])
  expect_gt(b@mid_x[2], b@breaks_x[2])
  expect_lt(b@mid_y[1], b@breaks_y[2])
  expect_gt(b@mid_y[2], b@breaks_y[2])
})

test_that("bin centres stay finite for every supported bin count and method", {
  set.seed(706)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  for (nbins in c(2, 4, 8, 12)) {
    b <- binningDepth2D(x, nbins = nbins)
    expect_true(all(is.finite(b@mid_x)))
    expect_true(all(is.finite(b@mid_y)))
  }

  lp <- binningDepth2D(x, binmethod = "LP", nbins = 6)
  expect_true(all(is.finite(lp@mid_x)))
})

test_that("plot marks the bin centres for a two-bin grid", {
  set.seed(707)
  x <- MASS::mvrnorm(100, c(0, 0), diag(2))
  b <- binningDepth2D(x, nbins = 2)

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(plot(b, add_mid = TRUE))
})
