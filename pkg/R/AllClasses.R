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


#' The title for my S4 class that extends \code{"character"} class.
#'
#' Some details about this class and my plans for it in the body.
#'
#'  @section Slots:
#' \describe{
#'    \item{freq}{Matrix with number of elements in certain bin.}
#'    \item{mid_x}{Middle values on x-axis.}
#'    \item{mid_y}{Middle values on y-axis.}
#'    \item{breaks_x}{Boundaries of bins.}
#'    \item{breaks_y}{Boundaries of bins.}
#'    \item{input_data}{Binned data.}
#'    \item{max_depth_x}{Point with maximum depth on x-axis.}
#'    \item{max_depth_y}{Point with maximum depth on y-axis.}
#'  }
#'  
#'  @section Slots:
#' \describe{
#'    \item{freq}{Matrix with number of elements in certain bin.}
#'    \item{mid_x}{Middle values on x-axis.}
#'    \item{mid_y}{Middle values on y-axis.}
#'    \item{breaks_x}{Boundaries of bins.}
#'    \item{breaks_y}{Boundaries of bins.}
#'    \item{input_data}{Binned data.}
#'    \item{max_depth_x}{Point with maximum depth on x-axis.}
#'    \item{max_depth_y}{Point with maximum depth on y-axis.}
#'  }
#'  
#'  
#' @name BinnDepth2d
#' @rdname BinnDepth2d
#' @exportClass 
setClass("BinnDepth2d", representation=list(freq = "matrix", mid_x = "numeric", mid_y = "numeric", breaks_x = "numeric", breaks_y = "numeric", input_data = "matrix", max_depth_x = "numeric", max_depth_y = "numeric"))

#Generics
setGeneric("getPlot", function(object) standardGeneric("getPlot"))
setGeneric(".getPlot", function(object) standardGeneric(".getPlot"))
setGeneric("as.matrix", function(x,...) standardGeneric("as.matrix"))
