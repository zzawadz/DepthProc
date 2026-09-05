context("Unsupported arguments are named rather than failing internally")

# The unassigned local named `depth` in these functions does NOT raise
# "object 'depth' not found". These functions live in the package namespace,
# which also holds the exported depth() function, so the name resolves by
# lexical scoping to that function object and the failure lands one step
# later. The expectations below pin the new explicit errors, not those.

test_that("depthLP rejects the unsupported func argument", {
  set.seed(801)
  x <- matrix(rnorm(40), ncol = 2)

  expect_error(depthLP(x, x, func = function(z) z), "'func' is not supported")
  expect_error(depthLP(x, x, func = "median"), "'func' is not supported")
})

test_that("depthLP still works when func is left alone", {
  set.seed(802)
  x <- matrix(rnorm(40), ncol = 2)

  expect_equal(length(depthLP(x, x)), nrow(x))
  expect_equal(length(depthLP(x, x, func = NULL)), nrow(x))
  expect_equal(length(depth(x, x, method = "LP")), nrow(x))
})

test_that("fncDepth names the valid methods for an unknown one", {
  set.seed(803)
  x <- matrix(rnorm(10 * 6), ncol = 6)

  expect_error(fncDepth(x, method = "BD"), "unknown functional depth method")
  expect_error(fncDepth(x, method = "BD"), "fncDepthBD")
  expect_error(fncDepth(x, method = "mbd"), "must be one of")
  expect_error(fncDepth(x, method = "NotADepth"), "unknown functional depth method")
})

test_that("the zoo method reports an unknown method the same way", {
  skip_if_not_installed("xts")

  set.seed(804)
  x <- matrix(rnorm(10 * 6), ncol = 6)
  z <- zoo::zoo(t(x), order.by = 1:6)

  expect_error(fncDepth(z, method = "BD"), "unknown functional depth method")
})

test_that("the supported functional methods still work", {
  set.seed(805)
  x <- matrix(rnorm(10 * 6), ncol = 6)

  expect_equal(length(fncDepth(x, method = "MBD")), 10L)
  expect_equal(length(fncDepth(x, method = "FM")), 10L)
})
