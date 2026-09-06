context("Functional depth dispatch and helpers")

test_that("fncDepth on a matrix treats rows as curves by default", {
  set.seed(201)
  x <- matrix(rnorm(15 * 20), ncol = 20)

  dep <- fncDepth(x)

  expect_s4_class(dep, "FncDepthMBD")
  expect_equal(length(dep), nrow(x))
  expect_equal(as.numeric(dep), fncDepthMBD(x))
  expect_equal(dep@u, x)
  expect_equal(dep@X, x)
  expect_equal(dep@method, "MBD")
})

test_that("fncDepth byrow = FALSE transposes the sample", {
  set.seed(202)
  x <- matrix(rnorm(15 * 20), ncol = 20)

  by_col <- fncDepth(x, byrow = FALSE)

  expect_equal(length(by_col), ncol(x))
  expect_equal(as.numeric(by_col), as.numeric(fncDepth(t(x))))
})

test_that("fncDepth dispatches the FM method and passes dep1d_params through", {
  set.seed(203)
  x <- matrix(rnorm(15 * 20), ncol = 20)
  params <- list(method = "Mahalanobis")

  dep <- fncDepth(x, method = "FM", dep1d_params = params)

  expect_s4_class(dep, "FncDepthFM")
  expect_equal(dep@method, "FM")
  expect_equal(as.numeric(dep), fncDepthFM(x, x, dep1d_params = params))
})

test_that("fncDepth takes its index from the column names", {
  set.seed(204)
  x <- matrix(rnorm(10 * 4), ncol = 4)

  expect_equal(fncDepth(x)@index, seq_len(ncol(x)))

  colnames(x) <- c("t1", "t2", "t3", "t4")
  idx <- fncDepth(x)@index

  expect_true(is.ordered(idx))
  expect_equal(as.character(idx), colnames(x))
})

test_that("fncDepth rejects u and X of different classes", {
  set.seed(205)
  x <- matrix(rnorm(40), ncol = 4)

  expect_error(fncDepth(x, as.data.frame(x)), "same class")
})

test_that("fncDepth on a zoo object keeps the time index", {
  skip_if_not_installed("xts")

  set.seed(206)
  x <- matrix(rnorm(10 * 8), ncol = 8)
  time <- as.POSIXct(1:8 * 86400, origin = "1970-01-01", tz = "UTC")
  x_xts <- xts::xts(t(x), order.by = time)

  dep <- fncDepth(x_xts)

  expect_s4_class(dep, "FncDepthMBD")
  expect_equal(length(dep), ncol(x_xts))
  expect_equal(as.numeric(dep), as.numeric(fncDepth(x)))
  expect_equal(dep@index, zoo::index(x_xts))
})

test_that("fncDepthBD returns valid band depths", {
  set.seed(207)
  x <- matrix(rnorm(20 * 5), ncol = 5)

  dep <- fncDepthBD(x)

  expect_equal(length(dep), nrow(x))
  expect_true(all(dep >= 0 & dep <= 1))
  expect_equal(dep, fncDepthBD(x, x))
})

test_that("fncDepthBD ranks a central curve above an outlying one", {
  set.seed(208)
  x <- rbind(matrix(rnorm(30 * 6), ncol = 6), rep(8, 6))

  dep <- fncDepthBD(x)

  # a curve above the whole sample is never inside a band, so it bottoms out
  # at the (n - 1) / choose(n, 2) floor
  expect_equal(dep[31], min(dep))
  expect_equal(dep[31], (nrow(x) - 1) / choose(nrow(x), 2))
  expect_lt(dep[31], max(dep))
})

test_that("fncDepthMedian returns the deepest curve", {
  set.seed(209)
  x <- matrix(rnorm(21 * 10), ncol = 10)

  med <- fncDepthMedian(x, method = "MBD")
  dep <- fncDepthMBD(x)

  expect_equal(length(med), ncol(x))
  expect_equal(unname(med), unname(x[which.max(dep), ]))
})

test_that("fncDepthMedian with unique = FALSE keeps the matrix of deepest curves", {
  set.seed(210)
  x <- matrix(rnorm(21 * 10), ncol = 10)

  med <- fncDepthMedian(x, method = "MBD", unique = FALSE)

  expect_true(is.matrix(med))
  expect_equal(ncol(med), ncol(x))
  expect_equal(nrow(med), 1L)
})

test_that("fncDepthMedian honours the FM method", {
  set.seed(211)
  x <- matrix(rnorm(21 * 10), ncol = 10)
  params <- list(method = "Mahalanobis")

  med <- fncDepthMedian(x, method = "FM", dep1d_params = params)
  dep <- fncDepthFM(x, x, dep1d_params = params)

  expect_equal(unname(med), unname(x[which.max(dep), ]))
})

test_that("fncGetBand returns the pointwise range of the deepest curves", {
  set.seed(212)
  x <- matrix(rnorm(20 * 6), ncol = 6)
  obj <- fncDepth(x)

  band <- fncGetBand(obj, band = 0.5)

  expect_s4_class(band, "FncBand")
  expect_equal(dim(band), c(ncol(x), 2L))
  expect_equal(band@level, 0.5)
  expect_equal(band@index, obj@index)
  expect_true(all(band[, 1] <= band[, 2]))
})

test_that("a wider fncGetBand band contains a narrower one", {
  set.seed(213)
  x <- matrix(rnorm(40 * 6), ncol = 6)
  obj <- fncDepth(x)

  narrow <- fncGetBand(obj, band = 0.25)
  wide <- fncGetBand(obj, band = 0.9)

  expect_true(all(wide[, 1] <= narrow[, 1]))
  expect_true(all(wide[, 2] >= narrow[, 2]))
})

test_that("fncGetBand at band = 1 spans the whole sample", {
  set.seed(214)
  x <- matrix(rnorm(20 * 6), ncol = 6)

  band <- fncGetBand(fncDepth(x), band = 1)

  expect_equal(band[, 1], apply(x, 2, min))
  expect_equal(band[, 2], apply(x, 2, max))
})

test_that("fncBoxPlot builds a ggplot with one ribbon per band", {
  set.seed(215)
  x <- matrix(rnorm(20 * 6), ncol = 6)

  p <- fncBoxPlot(x, bands = c(0, 0.5, 1), method = "MBD")

  expect_s3_class(p, "ggplot")
  expect_equal(nlevels(p$data$level), 3L)
  expect_equal(nrow(p$data), 3L * ncol(x))
})

test_that("fncDepthFM agrees with the univariate depths it dispatches to", {
  set.seed(216)
  x <- matrix(rnorm(15 * 8), ncol = 8)

  # the sum the loop builds, spelled out through the public entry points
  by_hand <- rowMeans(vapply(seq_len(ncol(x)), FUN.VALUE = numeric(nrow(x)),
                             function(i) {
                               as.numeric(depthMah(x[, i], x[, i]))
                             }))

  expect_equal(fncDepthFM(x, x, dep1d_params = list(method = "Mahalanobis")),
               by_hand)
})

test_that("fncDepthFM honours a univariate method other than the default", {
  set.seed(217)
  x <- matrix(rnorm(15 * 8), ncol = 8)

  mah <- fncDepthFM(x, x, dep1d_params = list(method = "Mahalanobis"))
  euc <- fncDepthFM(x, x, dep1d_params = list(method = "Euclidean"))

  expect_equal(length(mah), nrow(x))
  expect_false(isTRUE(all.equal(mah, euc)))
})

test_that("fncDepthFM validates its univariate method before the loop", {
  set.seed(218)
  x <- matrix(rnorm(15 * 8), ncol = 8)

  expect_error(fncDepthFM(x, x, dep1d_params = list(method = "NotADepth")),
               "unknown depth method")
  expect_error(fncDepthFM(x, x, dep1d_params = list(method = 3)),
               "must be a character value")
})

test_that("a functional depth is refused as fncDepthFM's univariate depth", {
  set.seed(219)
  x <- matrix(rnorm(15 * 8), ncol = 8)

  # depth() -> fncDepth() -> fncDepthFM() -> depth() used to make this recurse
  expect_error(fncDepthFM(x, x, dep1d_params = list(method = "FM")),
               "sample of curves")
  expect_error(fncDepth(x, method = "FM", dep1d_params = list(method = "MBD")),
               "sample of curves")
})

test_that("depth() and fncDepth() agree on which methods exist", {
  set.seed(220)
  x <- matrix(rnorm(15 * 8), ncol = 8)
  params <- list(method = "Mahalanobis")

  expect_equal(as.numeric(depth(x, x, method = "FM", dep1d_params = params)),
               as.numeric(fncDepth(x, x, method = "FM", dep1d_params = params)))
  expect_equal(as.numeric(depth(x, x, method = "MBD")),
               as.numeric(fncDepth(x, x, method = "MBD")))
})

test_that("fncDepthFM rejects a dep1d_params that is not a list", {
  set.seed(221)
  x <- matrix(rnorm(15 * 8), ncol = 8)

  # fncDepth() forwards ... to fncDepthFM(), and R partial-matches dep1d onto
  # dep1d_params; the string used to reach depth()'s `method` by position
  expect_error(fncDepth(x, method = "FM", dep1d = "Mahalanobis"),
               "must be a list")
  expect_error(fncDepth(x, method = "FM", dep1d = "Mahalanobis"),
               "Mahalanobis")
  expect_error(fncDepthFM(x, x, dep1d_params = "Mahalanobis"),
               "must be a list")
})
