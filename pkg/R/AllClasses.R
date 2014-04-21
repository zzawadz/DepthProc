#####################################
######### Depth definitions #########
#####################################

#' Depth
#' 
#' Virtual class with structure for every depth class from depthproc package.
#'
#'  @section Slots:
#' \describe{
#'    \item{u}{Matrix with number of elements in certain bin.}
#'    \item{X}{Middle values on x-axis.}
#'    \item{method}{Middle values on y-axis.}
#'    \item{name}{Boundaries of bins.}
#'  }
#'  
#'  
#'
#' @name Depth
#' @aliases DepthEuclid DepthProjection DepthMahalanobis DepthTukey DepthLP DepthLocal
#' @rdname Depth-class
#' @exportClass 
setClass("Depth", representation(u = "matrix", X = "matrix", method = "character", name = "character", "VIRTUAL"))
setClass("DepthEuclid", representation(), contains = c("Depth","numeric"))
setClass("DepthProjection", representation(), contains = c("Depth","numeric"))
setClass("DepthMahalanobis", representation(), contains = c("Depth","numeric"))
setClass("DepthTukey", representation(), contains = c("Depth","numeric"))
setClass("DepthLP", representation(), contains = c("Depth","numeric"))
setClass("DepthLocal", representation(), contains = c("Depth","numeric"))

#####################################
######### DDPlot #########
#####################################

#' DDPlot
#' 
#' Class fro DDPlot
#'
#'  @section Slots:
#' \describe{
#'    \item{X}{Object of class \link{Depth}.}
#'    \item{Y}{Object of class \link{Depth}.}
#'  }
#'  
#'  
#'
#' @name DDPlot
#' @rdname DDPlot-class
#' @exportClass 
setClass("DDPlot", representation(X = c("Depth"), Y = "Depth"))


setClass("DepthCurve", representation(depth = "Depth","VIRTUAL"))
setClass("DepthCurveList", representation("VIRTUAL"))

setClass("ScaleCurve", representation(alpha = "numeric")
                                          , contains=c("numeric","DepthCurve"))
setClass("ScaleCurveList", contains=c("DepthCurveList", "list"))

setClass("AsymmetryCurve", representation(alpha = "numeric")
         , contains=c("numeric","DepthCurve"))
setClass("AsymmetryCurveList", contains=c("DepthCurveList", "list"))


#' BinnDepth2d
#' 
#' Class that stores result of function binningDepth2D(...)
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
#'  
#' @name BinnDepth2d
#' @rdname BinnDepth2d
#' @exportClass 
setClass("BinnDepth2d", representation=list(freq = "matrix", mid_x = "numeric", mid_y = "numeric", breaks_x = "numeric", breaks_y = "numeric", input_data = "matrix", max_depth_x = "numeric", max_depth_y = "numeric"))

#Generics
setGeneric("getPlot", function(object) standardGeneric("getPlot"))
setGeneric(".getPlot", function(object) standardGeneric(".getPlot"))
setGeneric("as.matrix", function(x,...) standardGeneric("as.matrix"))

#####################################################
########### Classes for robust regression ###########
#####################################################

#' RobReg
#' 
#' Virtual class for robust regression methods from depthproc package
#'
#'  @section Slots:
#' \describe{
#'    \item{coef}{coefficients of fitted model}
#'  }
#'  
#'  
#' @name RobReg
#' @rdname RobReg
#' @exportClass 
setClass("RobReg", representation(coef = "numeric", "VIRTUAL"))

#' DeepReg2d
#' 
#' Class for robust regression methods from depthproc package
#'
#'  @section Slots:
#' \describe{
#'    \item{coef}{coefficients of fitted model}
#'    \item{depth}{regression depth of the fitted values}
#'  }
#'  
#'  
#' @name DeepReg2d
#' @rdname RDeepReg2d
#' @exportClass 
setClass("DeepReg2d", representation=list(depth = "numeric"), contains="RobReg")

#' TrimReg2d
#' 
#' Class for robust regression methods from depthproc package
#'
#'  @section Slots:
#' \describe{
#'    \item{coef}{coefficients of fitted model}
#'  }
#'  
#'  
#' @name TrimReg2d
#' @rdname TrimReg2d
#' @exportClass 
setClass("TrimReg2d", contains="RobReg")

#' @title Add line to plot
#'
#'  @description Add fitted line to a plot. This is overloaded function for robust regression methods from package depthproc.
#'  
setMethod("abline", "RobReg",function(a, ...) { abline(a@coef, ...)})
