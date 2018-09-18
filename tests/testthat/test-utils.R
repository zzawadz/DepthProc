context("Utils")

test_that("DepthProc Theme", {
  pp <- ggplot2::qplot(Species, Sepal.Length, data = iris) +
    DepthProc:::.depTheme()
  expect_doppelganger("depthproc theme", pp)
})

test_that("extractIndexFromMatrix", {
  x <- cbind(1,1,1)
  expect_equal(DepthProc:::extractIndexFromMatrix(x), 1:3)
})

test_that(".testNorm", {
  expect_equal(dim(DepthProc:::.testNorm(3)), c(100, 3))
})


test_that(".addAlpha", {
  expect_equivalent(DepthProc:::.addAlpha("red", 0.5),
               "#FF000080")
})
