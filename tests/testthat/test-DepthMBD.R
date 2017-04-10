context("Test MBD")

test_that("Equal to fda", {
  
  data("katowice.pollination")
  expect_equal(fda:::fMBD(t(katowice.pollination)),
    fncDepthMBD(katowice.pollination))
      
})
