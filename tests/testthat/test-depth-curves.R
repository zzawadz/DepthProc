context("Depth curves and their combination")

# asymmetryCurve fills NA wherever the alpha trimmed subset is too small, so
# every curve here uses several hundred rows and asserts the curve is not all
# NA before its values are compared.

test_that("scaleCurve grows with alpha and starts at zero volume", {
  set.seed(301)
  x <- MASS::mvrnorm(400, c(0, 0), diag(2))

  sc <- scaleCurve(x, name = "A")

  expect_s4_class(sc, "ScaleCurve")
  expect_equal(length(sc), length(sc@alpha))
  expect_equal(sc@name, "A")
  expect_equal(range(sc@alpha), c(0, 1))
  expect_equal(as.numeric(sc)[1], 0)
  expect_true(all(diff(as.numeric(sc)) >= -1e-9))
})

test_that("scaleCurve separates samples of different scale", {
  set.seed(302)
  tight <- MASS::mvrnorm(400, c(0, 0), diag(2))
  wide <- MASS::mvrnorm(400, c(0, 0), 4 * diag(2))

  expect_true(all(as.numeric(scaleCurve(wide)) >= as.numeric(scaleCurve(tight))))
})

test_that("scaleCurve with y returns both curves in a list", {
  set.seed(303)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  y <- MASS::mvrnorm(300, c(0, 0), 4 * diag(2))
  # Projection depth draws random directions, so a curve compared against a
  # second call has to be built with a deterministic depth
  params <- list(method = "Mahalanobis")

  both <- scaleCurve(x, y, name = "A", name_y = "B", depth_params = params)

  expect_s4_class(both, "ScaleCurveList")
  expect_equal(length(both), 2L)
  expect_equal(vapply(both, function(z) z@name, ""), c("A", "B"))
  expect_equal(as.numeric(both[[1]]),
               as.numeric(scaleCurve(x, name = "A", depth_params = params)))
  expect_equal(as.numeric(both[[2]]),
               as.numeric(scaleCurve(y, name = "B", depth_params = params)))
})

test_that("scaleCurve accepts a data frame and rejects a vector", {
  set.seed(304)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))

  params <- list(method = "Mahalanobis")

  expect_equal(as.numeric(scaleCurve(as.data.frame(x), depth_params = params)),
               as.numeric(scaleCurve(x, depth_params = params)))
  expect_error(scaleCurve(1:10), "must be a matrix or data frame")
  expect_error(scaleCurve(x, y = 1:10), "must be a matrix or data frame")
})

test_that("asymmetryCurve returns a usable curve on a large enough sample", {
  set.seed(305)
  x <- MASS::mvrnorm(400, c(0, 0), diag(2))

  ac <- asymmetryCurve(x, name = "A")

  expect_s4_class(ac, "AsymmetryCurve")
  expect_equal(length(ac), length(ac@alpha))
  expect_false(all(is.na(as.numeric(ac))))
  expect_true(all(as.numeric(ac)[!is.na(as.numeric(ac))] >= 0))
})

test_that("asymmetryCurve rejects a sample that is too small", {
  set.seed(306)

  expect_error(asymmetryCurve(matrix(rnorm(10), ncol = 2)), "Too small sample")
  expect_error(asymmetryCurve(MASS::mvrnorm(300, c(0, 0), diag(2)),
                              y = 1:10), "must be a matrix")
})

test_that("asymmetryCurve with y returns both curves in a list", {
  set.seed(307)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  y <- MASS::mvrnorm(300, c(3, 0), diag(2))

  both <- asymmetryCurve(x, y, name = "A", name_y = "B")

  expect_s4_class(both, "AsymmetryCurveList")
  expect_equal(length(both), 2L)
  expect_equal(vapply(both, function(z) z@name, ""), c("A", "B"))
})

test_that("combineDepthCurves joins two curves and then a list", {
  set.seed(308)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  y <- MASS::mvrnorm(300, c(0, 0), 2 * diag(2))

  a <- scaleCurve(x, name = "A")
  b <- scaleCurve(y, name = "B")
  c_ <- scaleCurve(x, name = "C")

  pair <- combineDepthCurves(a, b)
  expect_s4_class(pair, "ScaleCurveList")
  expect_equal(length(pair), 2L)

  three <- combineDepthCurves(pair, c_)
  expect_equal(length(three), 3L)
  expect_equal(vapply(three, function(z) z@name, ""), c("A", "B", "C"))

  from_list <- combineDepthCurves(.list = list(a, b, c_))
  expect_equal(vapply(from_list, function(z) z@name, ""), c("A", "B", "C"))
})

test_that("combineDepthCurves warns and renames on a duplicate curve name", {
  set.seed(309)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  a <- scaleCurve(x, name = "A")

  expect_warning(combineDepthCurves(a, a), "not unique")

  dup <- suppressWarnings(combineDepthCurves(a, a))
  expect_equal(vapply(dup, function(z) z@name, ""), c("A", "A1"))
})

test_that("combineDepthCurves is symmetric between a curve and a list", {
  set.seed(310)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  a <- scaleCurve(x, name = "A")
  b <- scaleCurve(x, name = "B")
  c_ <- scaleCurve(x, name = "C")

  lst <- combineDepthCurves(a, b)

  expect_equal(vapply(combineDepthCurves(c_, lst), function(z) z@name, ""),
               vapply(combineDepthCurves(lst, c_), function(z) z@name, ""))
})

test_that("as.matrix lays the curves out in named columns", {
  set.seed(311)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  y <- MASS::mvrnorm(300, c(0, 0), 2 * diag(2))

  both <- scaleCurve(x, y, name = "A", name_y = "B")
  m <- as.matrix(both)

  expect_true(is.matrix(m))
  expect_equal(dim(m), c(length(both[[1]]), 2L))
  expect_equal(colnames(m), c("A", "B"))
  expect_equal(m[, "A"], as.numeric(both[[1]]))
  expect_equal(m[, "B"], as.numeric(both[[2]]))
})

test_that("getPlot builds a ggplot with one row per curve point", {
  set.seed(312)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  y <- MASS::mvrnorm(300, c(0, 0), 2 * diag(2))

  both <- scaleCurve(x, y, name = "A", name_y = "B")
  p <- getPlot(both)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$layers[[1]]$data), 2L * length(both[[1]]))
  expect_equal(levels(factor(p$layers[[1]]$data$names)), c("A", "B"))
})

test_that("plot on a curve and on a curve list runs", {
  set.seed(313)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  sc <- scaleCurve(x, name = "A")

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(plot(sc))
  expect_silent(plot(combineDepthCurves(sc, scaleCurve(x, name = "B"))))
})

test_that("listClass names the container for each DepthCurve subclass", {
  set.seed(320)
  x <- MASS::mvrnorm(300, c(0, 0), diag(2))
  params <- list(method = "Mahalanobis")

  sc <- scaleCurve(x, depth_params = params, name = "A")
  ac <- asymmetryCurve(x, depth_params = params, name = "A")
  sc2 <- scaleCurve(x, depth_params = params, name = "B")
  ac2 <- asymmetryCurve(x, depth_params = params, name = "B")

  expect_equal(listClass(sc), "ScaleCurveList")
  expect_equal(listClass(ac), "AsymmetryCurveList")

  # the two callers that used to build the name with paste0()
  expect_s4_class(combineDepthCurves(sc, sc2), "ScaleCurveList")
  expect_s4_class(combineDepthCurves(ac, ac2), "AsymmetryCurveList")
})

test_that("a DepthCurve subclass with no container reports which class is missing", {
  methods::setClass("OrphanCurve", contains = c("DepthCurve", "numeric"),
                    where = globalenv())
  on.exit(methods::removeClass("OrphanCurve", where = globalenv()))

  set.seed(321)
  x <- MASS::mvrnorm(50, c(0, 0), diag(2))
  orphan <- methods::new("OrphanCurve", 1, depth = depthMah(x, x),
                         name = "A", title = "t", alpha = 1)

  # the paste0() convention gave "undefined class \"OrphanCurveList\"" from
  # deep inside methods::new(); the message now names the class and the fix
  expect_error(listClass(orphan), "OrphanCurveList")
  expect_error(listClass(orphan), "listClass")
})
