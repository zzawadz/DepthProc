context("Test MBD")

test_that("Equal to fda", {
  data("katowice.airpollution")
  expect_equal(fda:::fMBD(t(katowice.airpollution)),
               fncDepthMBD(katowice.airpollution))

})

context("Test MBD")

test_that("Equal interfaces", {
  data("katowice.airpollution")
  expect_equal(
    fncDepthMBD(katowice.airpollution),
    fncDepthMBD(katowice.airpollution, katowice.airpollution)
  )

  expect_equal(
    fncDepthMBD(katowice.airpollution)[10:20],
    fncDepthMBD(katowice.airpollution[10:20, ], katowice.airpollution)
  )

})

test_that("Ref rank", {
  x <- c(1, 1, 2, 3, 4, 4, 4, 4, 4, 5)
  expect_equal(rank(x), refRank(x, x))

  x <- c(5, 5, 3, 2, 11, 1, 11, 11)
  expect_equal(rank(x), refRank(x, x))

  x <- c(5, 5, 3, 2, 11, 1, 11, 11)
  y <- c(5, 5, 3, 2, 130, 1, 12, 12)

  expect_equal(refRank(y, x), rank(x))

  x <- c(1, 5, 5, 10)
  expect_equal(refRank(c(1, 4, 5, 10), x), c(1.0, 1.0, 2.5, 4.0))

  x <- c(4, 5, 6)
  expect_equal(refRank(c(4.5, 5), x), c(1, 2))

  x <- c(1, 4, 4, 4, 4, 5, 6)
  expect_equal(refRank(x[4:5], x), rank(x)[4:5])

  x <- sort(katowice.airpollution[, 1])

  rank.x <- rank(x)

  expect_equal(refRank(x, x), rank.x)
  expect_equal(refRank(x[10:30], x), rank.x[10:30])
  expect_equal(refRank(x[70:90], x), rank.x[70:90])
})
