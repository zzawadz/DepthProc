context("Tukey depth dispatch")

test_that("the dispatch table forwards threads to every parallel method", {
  # threads is a named formal of depth(), so it is captured there and never
  # reaches `...`; the Tukey arm was once the one arm not passing it on,
  # leaving depthTukey() permanently at its own threads = -1 default. The arms
  # now live in .depthMethods, which fncDepthFM() resolves against too.
  for (method in c("Mahalanobis", "Projection", "Tukey", "LP")) {
    arm <- paste(deparse(body(.depthMethods[[method]])), collapse = " ")
    expect_match(arm, "threads = threads", fixed = TRUE, info = method)
  }
})

test_that("depth(method = 'Tukey', threads = ) runs and agrees with depthTukey", {
  set.seed(801)
  x <- MASS::mvrnorm(60, c(0, 0), diag(2))

  expect_equal(
    as.vector(depth(x, x, method = "Tukey", threads = 1, exact = TRUE)),
    as.vector(depthTukey(x, x, threads = 1, exact = TRUE))
  )
})

test_that("exact Tukey depth warns instead of silently approximating above 2d", {
  set.seed(802)
  x3 <- MASS::mvrnorm(60, rep(0, 3), diag(3))

  expect_warning(depthTukey(x3, x3, exact = TRUE), "two-dimensional data only")
  expect_warning(depth(x3, x3, method = "Tukey", exact = TRUE),
                 "two-dimensional data only")
})

test_that("exact Tukey depth is silent where it is genuinely exact", {
  set.seed(803)
  x2 <- MASS::mvrnorm(60, c(0, 0), diag(2))
  x1 <- matrix(rnorm(60), ncol = 1)

  expect_silent(depthTukey(x2, x2, exact = TRUE))
  # 1d is computed by the ecdf path, which is exact whatever `exact` says
  expect_silent(depthTukey(x1, x1, exact = TRUE))
  expect_silent(depthTukey(x3 <- MASS::mvrnorm(30, rep(0, 3), diag(3)),
                           x3, exact = FALSE))
})
