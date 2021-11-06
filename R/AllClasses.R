#' Adds plots
#'
#' @docType methods
#' @rdname combineDepthCurves-methods
#'
#' @param x object
#' @param y object
#' @param .list list of plots to combine.
#'
#' @details
#'
#' See \code{\link{DepthCurve-class}} for description.
#'
#' @export
setGeneric("combineDepthCurves", function(x, y, .list = NULL) {
  standardGeneric("combineDepthCurves")
})

#####################################
######### Depth definitions #########
#####################################

#' Depth
#'
#' Virtual class with structure for every depth class from depthproc package.
#'
#' @slot u data set.
#' @slot X reference set.
#' @slot method depth type.
#'
#' @rdname Depth-class
#' @exportClass Depth
#'
#' @importFrom colorspace heat_hcl
#' @importFrom geometry convhulln
#' @importFrom lattice wireframe
#' @importFrom sm binning
#' @importFrom grDevices col2rgb extendrange gray.colors rgb
#' @importFrom graphics filled.contour lines matplot points polygon rect segments
#' @importFrom stats cov ecdf lm mad median na.omit quantile rnorm wilcox.test
#' @importFrom utils tail
#'
setClass("Depth",
         slots = c(u = "matrix", X = "matrix", method = "character"),
         contains = "VIRTUAL")
setClass("DepthEuclid", contains = c("Depth", "numeric"))
setClass("DepthProjection", contains = c("Depth", "numeric"))
setClass("DepthMahalanobis", contains = c("Depth", "numeric"))
setClass("DepthTukey", contains = c("Depth", "numeric"))
setClass("DepthLP", contains = c("Depth", "numeric"))
setClass("DepthLocal",
         slots = c(depth_params1 = "list", depth_params2 = "list"),
         contains = c("Depth", "numeric"))

#####################################
############## DDPlot ###############
#####################################

#' DDPlot
#'
#' Class fro DDPlot
#'
#' @slot X Object of class \link{Depth-class}.
#' @slot Y Object of class \link{Depth-class}.
#' @slot title title of a plot.
#'
#' @export
setClass("DDPlot", slots = c(X = "Depth", Y = "Depth", title = "character"))

#####################################
############ DepthCurve #############
#####################################

#' DepthCurve
#'
#' This page describes mechanism behavior of ScaleCurve and AsymmetryCurve
#'
#' @slot depth object of \link{Depth-class}
#' @slot name name of dataset used on plot
#' @slot title title of a plot
#' @slot alpha central area values
#'
#' @details
#'
#' DepthCurve is a virtual class that contains methods (getPlot(...) and plot(...)) for rendering single curve such as ScaleCurve or AsymmetryCurve. Such object can be combined by overloaded operator '%+%'. This 'addition' create DepthCurveList that can be used for rendering plot with multiple curves. Sample session (using ScaleCurve) is shown in Examples section.
#'
#' @examples
#' library(mvtnorm)
#' x <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 2 * diag(2))
#' y <- rmvt(n = 100, sigma = diag(2), df = 4)
#' s1 <- scaleCurve(x, depth_params = list(method = "Projection"))
#' s2 <- scaleCurve(y, depth_params = list(method = "Projection"), name = "Set2")
#'
#' sc_list <- combineDepthCurves(s1, s2) # Add one curve to another
#'
#' plot(sc_list) # Draw plot with two curves
#'
#' z <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 1 * diag(2))
#' s3 <- scaleCurve(z, depth_params = list(method = "Projection"))
#' plot(combineDepthCurves(sc_list, s3)) # Add third curve and draw a plot
#'
#' @export
setClass("DepthCurve",
         slots = c(depth = "Depth", name = "character", title = "character",
                   alpha = "numeric"),
         contains = "VIRTUAL")

#' DepthCurveList
#'
#' DepthCurveList is a special container for DepthCurve objects. See \link{DepthCurve-class}
#'
setClass("DepthCurveList", contains = "VIRTUAL")

#' ScaleCurve and ScaleCurveList
#'
#' ScaleCurve is a class that stores results of \link{scaleCurve} function.
#'
#' ScaleCurve intherits behviour from numeric vector, so raw values of ScaleCurve can be accessed via as.numeric(...).
#'
#' The mechanism of creating plots with multiple curves is shown in \link{DepthCurve-class} (same mechanism is applied for AsymmetryCurve).
#'
#' @examples
#' library(mvtnorm)
#' x <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 2 * diag(2))
#' y <- rmvt(n = 100, sigma = diag(2), df = 4)
#' s1 <- scaleCurve(x, depth_params = list(method = "Projection"))
#' s2 <- scaleCurve(y, depth_params = list(method = "Projection"), name = "Set2")
#'
#' sc_list <- combineDepthCurves(s1, s2) # Add one curve to another
#'
#' plot(sc_list) # Draw plot with two curves
#'
#' z <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 1 * diag(2))
#' s3 <- scaleCurve(z, depth_params = list(method = "Projection"))
#' plot(combineDepthCurves(sc_list, s3)) # Add third curve and draw a plot
#'
#' @export
setClass("ScaleCurve", contains = c("DepthCurve", "numeric"))
setClass("ScaleCurveList", contains = c("DepthCurveList", "list"))

#' AsymmetryCurve and AsymmetryCurveList
#'
#' AsymmetryCurve is a class that stores results of \link{asymmetryCurve} function.
#'
#' The mechanism of creating plots with multiple curves is shown in \link{DepthCurve-class} (same mechanism is applied for ScaleCurve).
#'
#' @export
setClass("AsymmetryCurve", contains = c("DepthCurve", "numeric"))
setClass("AsymmetryCurveList", contains = c("DepthCurveList", "list"))

#' BinnDepth2d
#'
#' Class that stores result of function binningDepth2D(...)
#'
#' @slot freq Matrix with number of elements in certain bin.
#' @slot mid_x Middle values on x-axis.
#' @slot mid_y Middle values on y-axis.
#' @slot breaks_x Boundaries of bins.
#' @slot breaks_y Boundaries of bins.
#' @slot input_data Binned data.
#' @slot max_depth_x Point with maximum depth on x-axis.
#' @slot max_depth_y Point with maximum depth on y-axis.
#'
#' @export
#'
setClass("BinnDepth2d",
         slots = c(freq = "matrix", mid_x = "numeric", mid_y = "numeric",
                   breaks_x = "numeric", breaks_y = "numeric",
                   input_data = "matrix", max_depth_x = "numeric",
                   max_depth_y = "numeric"))

#' @name getPlot
#' @title Create ggplot object from DepthCurve, DepthCurveList and DDPlot classes.
#'
#' @docType methods
#'
#' @param object a DDPlot ScaleCurve or AsymmetryCurve object class.
#'
#' @description
#'
#' Create an object of class ggplot from DepthCurve and DepthCurveList.
#'
#' @export
#' @rdname getPlot-methods
#'
setGeneric("getPlot", function(object) {
  standardGeneric("getPlot")
})
setGeneric(".getPlot", function(object) {
  standardGeneric(".getPlot")
})

#' @title as.matrix method for DepthCurveList.
#'
#' @param x an object of class that inherits from DepthCurveList (ScaleCurveList or AsymmetryCurveList).
#' @param ... other arguments passed to standard as.matrix function.
#'
#' @description Create a matrix from DepthCurve and DepthCurveList.
#' @docType methods
#' @rdname as.matrix-methods
#' @export

setGeneric("as.matrix", function(x, ...) {
  standardGeneric("as.matrix")
})

#####################################################
########### Classes for robust regression ###########
#####################################################

#' RobReg
#'
#' Virtual class for robust regression methods from depthproc package
#'
#' @slot coef coefficients of fitted model
#'
#' @export
#'
setClass("RobReg", slots = c(coef = "numeric"), contains = "VIRTUAL")

#' DeepReg2d
#'
#' Class for robust regression methods from depthproc package
#'
#' @slot coef coefficients of fitted model
#' @slot depth regression depth of the fitted values
#'
#' @export
#'
setClass("DeepReg2d", slots = c(depth = "numeric"), contains = "RobReg")

#' TrimReg2d
#'
#' Class for robust regression methods from depthproc package
#'
#' @export
#'
setClass("TrimReg2d", contains = "RobReg")

#' @title Add line to plot
#' @description Add fitted line to a plot. This is overloaded function for robust regression methods from package depthproc.
#'
#' @param a an object of class RobReg
#' @param b not used.
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#' @param h not supported.
#' @param v not supported.
#' @param reg not supported.
#' @param coef not supported.
#' @param untf not supported.
#'
#' @export
#' @aliases abline,RobReg,ANY,ANY,ANY-method
#'
setMethod("abline", "RobReg", function(a, ...) {
  abline(a@coef, ...)
})
setMethod("show", "Depth", function(object) {
  cat("Depth method: ", object@method, "\n")
  print(object@.Data, width = 20)
})

#####################################################
################### Depth Density ###################
#####################################################

#' DepthDensity
#'
#' Class for depth based density estimator.
#'
#' @details
#'
#' \code{\link{depthDensity}}
#'
#' @export
#'
setClass("DepthDensity",
         slots = c(xgrid = "numeric", ygrid = "numeric", dep_scale = "matrix",
                   density_raw = "matrix", density = "matrix"))

#' @title Method for plotting DepthCurve and DDPlot object.
#' @docType methods
#' @rdname plot-methods
#'
#' @param x object that inherits from DepthCurve class (ScaleCurve or AsymmetryCurve), or DDPlot class.
#' @param y not supported.
#' @param \dots not supported.
#'
#' @description Plot Depth curve
#' @export
#'
#' @examples
#'
#' x <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 3 * diag(2))
#' sc <- scaleCurve(x)
#' plot(sc)
#'
setGeneric("plot")
