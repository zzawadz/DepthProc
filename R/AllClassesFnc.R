#### Functional depths ####

#' Functional Depth
#'
#' Virtual class with structure for every functional depth class from depthproc package. Inherits from \code{\link{Depth-class}}.
#'
#' @slot index numeric, or time-based object.
#'
#' @rdname FunctionalDepth-class
#' @exportClass FunctionalDepth
#'
setClass("FunctionalDepth",
         slots = c(index = "ANY"),
         contains = c("VIRTUAL", "Depth"))
setClass("FncDepthMBD", contains = c("FunctionalDepth", "numeric"))
setClass("FncDepthFM", contains = c("FunctionalDepth", "numeric"))
setClass("FncBand",
         slots = c(index = "ANY", level = "numeric"),
         contains = "matrix")
