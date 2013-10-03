setClass("Depth", representation(u = "matrix", X = "matrix", "VIRTUAL"))

setClass("DepthEuclid", representation(), contains = c("Depth","numeric"))


