setClass("Depth", representation(u = "matrix", X = "matrix", method = "character", name = "character", "VIRTUAL"))
setClass("DepthEuclid", representation(), contains = c("Depth","numeric"))
setClass("DepthProjection", representation(), contains = c("Depth","numeric"))
setClass("DepthMahalanobis", representation(), contains = c("Depth","numeric"))
setClass("DepthTukey", representation(), contains = c("Depth","numeric"))
setClass("DepthLP", representation(), contains = c("Depth","numeric"))

setClass("DDPlot", representation(X = c("Depth"), Y = "Depth"))



setClass("ScaleCurve", representation(depth = "Depth", alpha = "numeric"), contains="numeric")
setClass("ScaleCurveList", contains="list")


#Generics
setGeneric("getPlot", function(object) standardGeneric("getPlot"))
setGeneric("as.matrix", function(x,...) standardGeneric("as.matrix"))
