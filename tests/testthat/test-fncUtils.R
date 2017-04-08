context("Test for utility functions used for functional depths")

test_that("Proper order of index created from columns", {
  data("katowice.pollination")
  
  result <- structure(1:24,
    .Label = c("H1", "H2", "H3", "H4", "H5", "H6", 
      "H7", "H8", "H9", "H10", "H11", "H12", "H13", "H14", "H15", "H16", 
      "H17", "H18", "H19", "H20", "H21", "H22", "H23", "H24"),
    class = c("ordered", "factor"))
  
  expect_equal(.extractIndexFromMatrix(katowice.pollination), result)
})
