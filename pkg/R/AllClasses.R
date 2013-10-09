setClass("Depth", representation(u = "matrix", X = "matrix", method = "character", name = "character", "VIRTUAL"))
setClass("DepthEuclid", representation(), contains = c("Depth","numeric"))
setClass("DepthProjection", representation(), contains = c("Depth","numeric"))
setClass("DepthMahalanobis", representation(), contains = c("Depth","numeric"))
setClass("DepthTukey", representation(), contains = c("Depth","numeric"))
setClass("DepthLP", representation(), contains = c("Depth","numeric"))

setClass("DDPlot", representation(X = c("Depth"), Y = "Depth"))


setClass("DepthCurve", representation(depth = "Depth","VIRTUAL"))
setClass("DepthCurveList", representation("VIRTUAL"))

setClass("ScaleCurve", representation(alpha = "numeric")
                                          , contains=c("numeric","DepthCurve"))
setClass("ScaleCurveList", contains=c("DepthCurveList", "list"))

setClass("AsymmetryCurve", representation(alpha = "numeric")
         , contains=c("numeric","DepthCurve"))
setClass("AsymmetryCurveList", contains=c("DepthCurveList", "list"))


#Generics
setGeneric("getPlot", function(object) standardGeneric("getPlot"))
setGeneric(".getPlot", function(object) standardGeneric(".getPlot"))
setGeneric("as.matrix", function(x,...) standardGeneric("as.matrix"))
