# NUMBERED EXAMPELS APPEARING IN THE TEXT

set.seed(123)

#EXAMPLE 1
require("MASS")

x = mvrnorm(100, c(0, 0), diag(2))
y = mvrnorm(100, c(0, 0), diag(2) * 1.4)
mWilcoxonTest(x, y)
