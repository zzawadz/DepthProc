context("Scale curve tests")

test_that(
"Fix QH6114 qhull precision error:
initial simplex is not convex. Distance=8.7e-19
for xdata", {

  xdata <- structure(
    c(-0.19607854, -0.19607854, -0.26348042, 0.002450943,
      0.002450943, 0.051470518),
    .Dim = 3:2,
    .Dimnames = list(NULL, c("x", "y"))
  )
  sc <- scaleCurve(xdata)
  expect_equal(unique(sc@.Data), 0)
})
