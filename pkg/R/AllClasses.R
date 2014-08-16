#' Adds plots
#' 
#' @docType methods
#' @rdname grapes-plus-grapes-methods
#' 
#' @param e1 object
#' @param e2 object
#' 
#' @details 
#' 
#' See \code{\link{DepthCurve}} for description.
#' 
#' @export
setGeneric("%+%", function(e1,e2) standardGeneric("%+%"))


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
#' @export 
setClass("Depth", representation(u = "matrix", X = "matrix", method = "character", name = "character", "VIRTUAL"))
setClass("DepthEuclid", representation(), contains = c("Depth","numeric"))
setClass("DepthProjection", representation(), contains = c("Depth","numeric"))
setClass("DepthMahalanobis", representation(), contains = c("Depth","numeric"))
setClass("DepthTukey", representation(), contains = c("Depth","numeric"))
setClass("DepthLP", representation(), contains = c("Depth","numeric"))
setClass("DepthLocal", representation("depth1" = "character","depth2" = "character"), contains = c("Depth","numeric"))

#####################################
######### DDPlot ####################
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
#' @export 
setClass("DDPlot", representation(X = c("Depth"), Y = "Depth", title = "character"))

#####################################
######### DepthCurve ################
#####################################

#' DepthCurve and DepthCurveList
#' 
#' This page describes mechanism behavior of ScaleCurve and AsymmetryCurve
#'
#' @section DepthCurve and DepthCurveList:
#' 
#' DepthCurve is a virtual class that contains methods (getPlot(...) and plot(...)) for rendering single curve such as ScaleCurve or AsymmetryCurve. Such object can be added by overloaded operator '%+%'. This 'addition' create DepthCurveList that can be used for rendering plot with multiple curves. Sample session (using ScaleCurve) is shown in Examples section.
#' 
#' @examples
#' require(MASS)
#' require(mvtnorm)
#' x = mvrnorm(n = 100, mu = c(0,0), Sigma = 2*diag(2))
#' y = rmvt(n = 100, sigma = diag(2), df = 4)
#' s1 = scaleCurve(x, method = "Projection", plot = FALSE)
#' s2 = scaleCurve(y, method = "Projection", plot = FALSE, name = "Set2")
#' 
#' 
#' sc_list = s1 %+% s2 # Add one curve to another
#' 
#' plot(sc_list) # Draw plot with two curves
#' 
#' z = mvrnorm(n = 100, mu = c(0,0), Sigma = 1*diag(2))
#' s3 = scaleCurve(z, method = "Projection", plot = FALSE)
#' plot(sc_list%+%s3) # Add third curve and draw a plot
#' 
#'
#' @name DepthCurve
#' @aliases DepthCurve DepthCurveList
#' @rdname DepthCurve-class 
#' @export  
setClass("DepthCurve", representation(depth = "Depth", title = "character", alpha = "numeric","VIRTUAL"))
setClass("DepthCurveList", representation("VIRTUAL"))



#' ScaleCurve and ScaleCurveList
#' 
#' ScaleCurve is a class that stores results of \link{scaleCurve} function.
#' 
#' @section Slots:
#' \describe{
#' 
#'    ScaleCurve intherits behviour from numeric vector, so raw values of ScaleCurve can be accessed via as.numeric(...).
#'    
#'    \item{alpha}{central regions}.}
#'  
#' 
#' The mechanism of creating plots with multiple curves is shown in \link{DepthCurve} (same mechanism is applied for AsymmetryCurve).
#' 
#' @examples
#' require(MASS)
#' require(mvtnorm)
#' x = mvrnorm(n = 100, mu = c(0,0), Sigma = 2*diag(2))
#' y = rmvt(n = 100, sigma = diag(2), df = 4)
#' s1 = scaleCurve(x, method = "Projection", plot = FALSE)
#' s2 = scaleCurve(y, method = "Projection", plot = FALSE, name = "Set2")
#' 
#' 
#' sc_list = s1 %+% s2 # Add one curve to another
#' 
#' plot(sc_list) # Draw plot with two curves
#' 
#' z = mvrnorm(n = 100, mu = c(0,0), Sigma = 1*diag(2))
#' s3 = scaleCurve(z, method = "Projection", plot = FALSE)
#' plot(sc_list%+%s3) # Add third curve and draw a plot
#' 
#'
#' @name ScaleCurve
#' @aliases ScaleCurve ScaleCurveList
#' @rdname ScaleCurve-class 
#' @export  
setClass("ScaleCurve", contains=c("numeric","DepthCurve"))
setClass("ScaleCurveList", contains=c("DepthCurveList", "list"))



#' AsymmetryCurve and AsymmetryCurveList
#' 
#' AsymmetryCurve is a class that stores results of \link{asymmetryCurve} function.
#' 
#' @section Slots:
#' \describe{
#' 
#'    AsymmetryCurve intherits behviour from numeric vector, so raw values of AsymmetryCurve can be accessed via as.numeric(...).
#'    
#'    \item{alpha}{central regions}.}
#'  
#' 
#' The mechanism of creating plots with multiple curves is shown in \link{DepthCurve} (same mechanism is applied for ScaleCurve).
#' 
#'
#' @name AsymmetryCurve
#' @aliases AsymmetryCurve AsymmetryCurveList
#' @rdname AsymmetryCurve-class 
#' @export 
setClass("AsymmetryCurve", contains=c("numeric","DepthCurve"))
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
#' @export
setClass("BinnDepth2d", representation=list(freq = "matrix", mid_x = "numeric", mid_y = "numeric", breaks_x = "numeric", breaks_y = "numeric", input_data = "matrix", max_depth_x = "numeric", max_depth_y = "numeric"))

#' @export
#' @title Create ggplot object from DepthCurve, DepthCurveList and DDPlot classes.
#' @rdname getPlot-methods
#' @docType methods
#' 
#'  @param object a DDPlot ScaleCurve or AsymmetryCurve object class.
#'  
#'  @description Create an object of class ggplot from DepthCurve and DepthCurveList.
#' @name getPlot
setGeneric("getPlot", function(object) standardGeneric("getPlot"))
setGeneric(".getPlot", function(object) standardGeneric(".getPlot"))

#' @export
#' @title as.matrix method for DepthCurveList.
#' @rdname as.matrix-methods
#' @docType methods
#'
#'  @param x an object of class that inherits from DepthCurveList (ScaleCurveList or AsymmetryCurveList).
#'  @param ... other arguments passed to standard as.matrix function.
#'  
#'  @description Create a matrix from DepthCurve and DepthCurveList.
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
#' @export
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
#' @export
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
#' @export
setClass("TrimReg2d", contains="RobReg")




#' @title Add line to plot
#'
#'  @param a an object of class RobReg
#'  @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#'  
#'  @description Add fitted line to a plot. This is overloaded function for robust regression methods from package depthproc.
#' @export
#' 
setMethod("abline", "RobReg",function(a, ...) { abline(a@coef, ...)})


setMethod("show", "Depth", function(object)
{
  cat("Depth method: ", object@method, "\n")
  cat("Set name:", object@name, "\n")
  
  print(object@.Data,width = 20)
})


################################### Depth Density ############################
setClass("DepthDensity", representation=list(
  xgrid = "numeric",
  ygrid = "numeric",
  dep_scale = "matrix",
  density_raw = "matrix",
  density = "matrix"))





#' @title Method for plotting DepthCurve and DDPlot object.
#' @docType methods
#' @rdname plot-methods
#'  
#' @param x object that inherits from DepthCurve class (ScaleCurve or AsymmetryCurve), or DDPlot class.
#'  
#'  @description Plot Depth curve
#' @export
#' 
#' @examples
#' 
#' require(MASS)
#' x = mvrnorm(n = 100, mu = c(0,0), Sigma = 3*diag(2))
#' sc = scaleCurve(x)
#' plot(sc)
#' 
setGeneric("plot")
