#### Functional depths ####

#' Functional Depth
#'
#' Virtual class with structure for every functional depth class from depthproc package.
#'
#' @slot u data set.
#' @slot X reference set.
#' @slot method functional depth type.
#' @slot name name that will be used on plots. By default it is a name of variable passed to fncDepth.
#' @slot index numeric, or time-based object.
#'  
#' @aliases DepthMBD
#' @rdname FunctionalDepth-class
#' @exportClass FunctionalDepth
#' 
setClass("FunctionalDepth",
         slots = c(u = "matrix", X = "matrix", method = "character",
                   name = "character", index = "ANY", val_name = "ANY"),
         contains = "VIRTUAL")
setClass("FncDepthMBD", contains = c("FunctionalDepth", "numeric"))
setClass("FncDepthFM", contains = c("FunctionalDepth", "numeric"))
setClass("FncBand",
         slots = c(index = "ANY", level = "numeric"),
         contains = "matrix")