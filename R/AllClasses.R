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
methods::setGeneric("combineDepthCurves", function(x, y, .list = NULL) {
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
methods::setClass("Depth",
         slots = c(u = "matrix", X = "matrix", method = "character"),
         contains = "VIRTUAL")
methods::setClass("DepthEuclid", contains = c("Depth", "numeric"))
methods::setClass("DepthProjection", contains = c("Depth", "numeric"))
methods::setClass("DepthMahalanobis", contains = c("Depth", "numeric"))
methods::setClass("DepthTukey", contains = c("Depth", "numeric"))
methods::setClass("DepthLP", contains = c("Depth", "numeric"))
methods::setClass("DepthLocal",
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
#' @slot name name of the x data set, used to label the horizontal axis.
#' @slot name_y name of the y data set, used to label the vertical axis.
#' @slot sample factor, one level per point, saying which of the two data sets
#'   it came from. Both axes hold the depths of the pooled sample, so this is
#'   the only record of a point's origin. An empty factor means the points are
#'   not distinguished and are all drawn in one colour, which is how a DDPlot
#'   built without it --- \code{\link{ddMvnorm}}, where every point comes from
#'   the same data set --- still renders.
#'
#' @export
methods::setClass("DDPlot",
         slots = c(X = "Depth", Y = "Depth", title = "character",
                   name = "character", name_y = "character",
                   sample = "factor"),
         prototype = methods::prototype(name = "X", name_y = "Y",
                                        sample = factor()))

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
#' x <- MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = 2 * diag(2))
#' y <- mvtnorm::rmvt(n = 100, sigma = diag(2), df = 4)
#' s1 <- DepthProc::scaleCurve(x, depth_params = list(method = "Projection"))
#' s2 <- DepthProc::scaleCurve(y, depth_params = list(method = "Projection"), name = "Set2")
#'
#' sc_list <- DepthProc::combineDepthCurves(s1, s2) # Add one curve to another
#'
#' plot(sc_list) # Draw plot with two curves
#'
#' z <- MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = 1 * diag(2))
#' s3 <- DepthProc::scaleCurve(z, depth_params = list(method = "Projection"))
#' plot(DepthProc::combineDepthCurves(sc_list, s3)) # Add third curve and draw a plot
#'
#' @export
methods::setClass("DepthCurve",
         slots = c(depth = "Depth", name = "character", title = "character",
                   alpha = "numeric"),
         contains = "VIRTUAL")

#' DepthCurveList
#'
#' DepthCurveList is a special container for DepthCurve objects. See \link{DepthCurve-class}
#'
methods::setClass("DepthCurveList", contains = "VIRTUAL")

#' @title Container class for a DepthCurve
#'
#' @docType methods
#' @rdname depthCurveListClass-methods
#'
#' @param object an object that inherits from \link{DepthCurve-class}.
#'
#' @description
#'
#' Returns the name of the \link{DepthCurveList-class} class that holds curves of
#' \code{object}'s class. \code{plot()} on a single curve and
#' \code{\link{combineDepthCurves}} on two of them both need that name, and
#' every \code{DepthCurve} subclass declares it with its own method rather than
#' having it inferred from the subclass name.
#'
#' @export
methods::setGeneric("depthCurveListClass", function(object) {
  standardGeneric("depthCurveListClass")
})

#' @rdname depthCurveListClass-methods
#' @export
methods::setMethod("depthCurveListClass", "DepthCurve", function(object) {
  # Fallback for subclasses defined outside the package, which historically
  # relied on the <Name>/<Name>List naming convention. Checking the class here
  # turns methods::new()'s generic "undefined class" into a message that names
  # both the subclass at fault and the fix.
  cls <- paste0(class(object), "List")

  if (!methods::isClass(cls) || !methods::extends(cls, "DepthCurveList")) {
    stop(gettextf(
      paste("no depthCurveListClass() method for class %s, and the %s",
            "convention gives %s, which is not a DepthCurveList; define a",
            "depthCurveListClass() method for %s returning the name of its",
            "container class"),
      sQuote(class(object)), sQuote("<Name>List"), sQuote(cls),
      sQuote(class(object))
    ))
  }

  cls
})

#' ScaleCurve and ScaleCurveList
#'
#' ScaleCurve is a class that stores results of \link{scaleCurve} function.
#'
#' ScaleCurve intherits behviour from numeric vector, so raw values of ScaleCurve can be accessed via as.numeric(...).
#'
#' The mechanism of creating plots with multiple curves is shown in \link{DepthCurve-class} (same mechanism is applied for AsymmetryCurve).
#'
#' @examples
#' x <- MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = 2 * diag(2))
#' y <- mvtnorm::rmvt(n = 100, sigma = diag(2), df = 4)
#' s1 <- DepthProc::scaleCurve(x, depth_params = list(method = "Projection"))
#' s2 <- DepthProc::scaleCurve(y, depth_params = list(method = "Projection"), name = "Set2")
#'
#' sc_list <- DepthProc::combineDepthCurves(s1, s2) # Add one curve to another
#'
#' plot(sc_list) # Draw plot with two curves
#'
#' z <- MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = 1 * diag(2))
#' s3 <- DepthProc::scaleCurve(z, depth_params = list(method = "Projection"))
#' plot(DepthProc::combineDepthCurves(sc_list, s3)) # Add third curve and draw a plot
#'
#' @export
methods::setClass("ScaleCurve", contains = c("DepthCurve", "numeric"))
methods::setClass("ScaleCurveList", contains = c("DepthCurveList", "list"))

#' @rdname depthCurveListClass-methods
#' @export
methods::setMethod("depthCurveListClass", "ScaleCurve",
                   function(object) "ScaleCurveList")

#' AsymmetryCurve and AsymmetryCurveList
#'
#' AsymmetryCurve is a class that stores results of \link{asymmetryCurve} function.
#'
#' The mechanism of creating plots with multiple curves is shown in \link{DepthCurve-class} (same mechanism is applied for ScaleCurve).
#'
#' @export
methods::setClass("AsymmetryCurve", contains = c("DepthCurve", "numeric"))
methods::setClass("AsymmetryCurveList", contains = c("DepthCurveList", "list"))

#' @rdname depthCurveListClass-methods
#' @export
methods::setMethod("depthCurveListClass", "AsymmetryCurve",
                   function(object) "AsymmetryCurveList")

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
methods::setClass("BinnDepth2d",
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
methods::setGeneric("getPlot", function(object) {
  standardGeneric("getPlot")
})
methods::setGeneric(".getPlot", function(object) {
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

methods::setGeneric("as.matrix", function(x, ...) {
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
methods::setClass("RobReg", slots = c(coef = "numeric"), contains = "VIRTUAL")

#' DeepReg2d
#'
#' Class for robust regression methods from depthproc package
#'
#' @slot coef coefficients of fitted model
#' @slot depth regression depth of the fitted values
#'
#' @export
#'
methods::setClass("DeepReg2d", slots = c(depth = "numeric"), contains = "RobReg")

#' TrimReg2d
#'
#' Class for robust regression methods from depthproc package
#'
#' @export
#'
methods::setClass("TrimReg2d", contains = "RobReg")

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
methods::setMethod("abline", "RobReg", function(a, ...) {
  abline(a@coef, ...)
})
methods::setMethod("show", "Depth", function(object) {
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
methods::setClass("DepthDensity",
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
#' x <- MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = 3 * diag(2))
#' sc <- DepthProc::scaleCurve(x)
#' plot(sc)
#'
methods::setGeneric("plot")
