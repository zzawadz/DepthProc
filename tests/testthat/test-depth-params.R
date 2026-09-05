context("depth_params validation and the contour renderer")

test_that("depthSpec builds a list the ordinary entry points accept", {
  set.seed(701)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  spec <- depthSpec("Mahalanobis")

  expect_s3_class(spec, "depthSpec")
  expect_equal(unclass(spec), list(method = "Mahalanobis"))
  expect_equal(as.numeric(scaleCurve(x, depth_params = spec)),
               as.numeric(scaleCurve(x, depth_params = list(method = "Mahalanobis"))))
  expect_equal(depthMedian(x, spec),
               depthMedian(x, list(method = "Mahalanobis")))
})

test_that("depthSpec carries method arguments through", {
  set.seed(702)
  x <- MASS::mvrnorm(120, c(0, 0), diag(2))

  spec <- depthSpec("Tukey", exact = TRUE)

  expect_equal(spec$exact, TRUE)
  expect_equal(depthMedian(x, spec),
               depthMedian(x, list(method = "Tukey", exact = TRUE)))
})

test_that("depthSpec rejects a method that does not exist", {
  expect_error(depthSpec("tukey"), "unknown depth method")
  expect_error(depthSpec("tukey"), "Tukey")
  expect_error(depthSpec("NotADepth"), "unknown depth method")
  expect_error(depthSpec(3), "single character value")
})

test_that("depthSpec rejects an argument the method does not take", {
  # the point of the constructor: ndirs used to be dropped without a word
  expect_error(depthSpec("Projection", ndirs = 100), "not an argument")
  expect_error(depthSpec("Projection", ndirs = 100), "it takes")
  expect_error(depthSpec("Mahalanobis", exact = TRUE), "not an argument")
  expect_error(depthSpec("Euclidean", ndir = 10), "no further arguments")

  # and accepts the ones it does
  expect_silent(depthSpec("Projection", ndir = 100, threads = 1))
  expect_silent(depthSpec("LP", pdim = 2, la = 1, lb = 1))
  expect_silent(depthSpec("Local", beta = 0.3))
})

test_that("a plain depth_params list still works but is now checked", {
  set.seed(703)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  expect_silent(scaleCurve(x, depth_params = list(method = "Mahalanobis")))
  expect_error(scaleCurve(x, depth_params = list(method = "mahalanobis")),
               "unknown depth method")
  expect_error(depthMedian(x, list(method = "NotADepth")),
               "unknown depth method")
  expect_error(ddPlot(x, x, depth_params = list(method = "NotADepth")),
               "unknown depth method")
  expect_error(depthContour(x, depth_params = list(method = "NotADepth")),
               "unknown depth method")
  expect_error(depthPersp(x, depth_params = list(method = "NotADepth")),
               "unknown depth method")
})

test_that("depth_params must be a named list without u or X", {
  set.seed(704)
  x <- MASS::mvrnorm(200, c(0, 0), diag(2))

  expect_error(scaleCurve(x, depth_params = list("Mahalanobis")),
               "must be named")
  expect_error(scaleCurve(x, depth_params = "Mahalanobis"),
               "must be a list")
  expect_error(scaleCurve(x, depth_params = "Mahalanobis"),
               'list\\(method = "Mahalanobis"\\)')
  expect_error(depthMedian(x, list(method = "Mahalanobis", u = x)),
               "must not contain")
  expect_error(depthMedian(x, list(method = "Mahalanobis", X = x)),
               "must not contain")
})

test_that("depthLocal names which of its two parameter lists is wrong", {
  set.seed(705)
  x <- MASS::mvrnorm(30, c(0, 0), diag(2))

  expect_error(depthLocal(x, x, depth_params1 = list(method = "NotADepth")),
               "depth_params1")
  expect_error(depthLocal(x, x,
                          depth_params1 = list(method = "Mahalanobis"),
                          depth_params2 = list(method = "NotADepth")),
               "depth_params2")
})

test_that("contourMethod is a property of the depth class", {
  set.seed(706)
  x <- MASS::mvrnorm(80, c(0, 0), diag(2))

  # Tukey depth is piecewise constant, so its level sets are traced by a hull
  expect_equal(contourMethod(depthTukey(x, x, exact = TRUE)), "convexhull")
  expect_equal(contourMethod(depthMah(x, x)), "contour")
  expect_equal(contourMethod(depthEuclid(x, x)), "contour")
  expect_equal(contourMethod(depthProjection(x, x)), "contour")
  expect_equal(contourMethod(depthLP(x, x)), "contour")
})

test_that("depthContour asks the depth object rather than the input string", {
  set.seed(707)
  x <- MASS::mvrnorm(80, c(0, 0), diag(2))

  pdf(NULL)
  on.exit(dev.off())

  expect_silent(depthContour(x, n = 8, legend = FALSE,
                             depth_params = list(method = "Tukey")))
  expect_silent(depthContour(x, n = 8, legend = FALSE,
                             contour_method = "convexhull",
                             depth_params = list(method = "Mahalanobis")))

  # a contour_method that is not one of the three used to fall through to
  # "contour" without a word
  expect_error(depthContour(x, n = 8, contour_method = "hull"), "should be one of")
})

test_that("depthPersp titles the plot from the computed depth", {
  set.seed(708)
  x <- MASS::mvrnorm(80, c(0, 0), diag(2))

  pdf(NULL)
  on.exit(dev.off())

  expect_equal(depthPersp(x, n = 8,
                          depth_params = list(method = "Mahalanobis"))$main,
               "Mahalanobis depth")
  # no method given: the title reports depth()'s default, not a hardcoded guess
  expect_equal(depthPersp(x, n = 8)$main, "Projection depth")
})
