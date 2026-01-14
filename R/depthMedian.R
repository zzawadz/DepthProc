#' @title Depth median
#' @docType methods
#' @rdname depthMedian-methods
#'
#' @param x object of class Depth or matrix.
#' @param depth_params list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact).
#' @param convex logical. If true, than centroid of the convex hull created from deepest points is returned.
#'
#' @description
#'
#' Return point with maximum depth function value. If multiple points have the same value, mean average of them will be returned.
#'
#' @export
#'
#' @examples
#'
#' # depthMedian for matrix
#' x <- matrix(rnorm(600), nc = 3)
#' depthMedian(x)
#'
#' # depthMedian works with object of class Depth
#' dp <- depth(x)
#' depthMedian(dp)
#'
methods::setGeneric("depthMedian", function(x, depth_params = list(), convex = FALSE) {
  standardGeneric("depthMedian")
})

#' @rdname depthMedian-methods
#' @importFrom grDevices chull
#' @export
methods::setMethod("depthMedian", "matrix", function(x, depth_params = list(), convex = FALSE) {
  ux_list <- list(u = x, X = x)
  depths <- do.call(depth, c(ux_list, depth_params))
  med <- x[depths == max(depths), ]

  if (ncol(x) != length(med) && convex) {
    med <- colMeans(med[chull(med), ])
  } else if(ncol(x) != length(med)) {
    med <- colMeans(med)
  }
  med
})

#' @rdname depthMedian-methods
#' @export
methods::setMethod("depthMedian", "data.frame", function(x, depth_params = list(), convex = FALSE) {
  x <- as.matrix(x)
  depthMedian(x, depth_params, convex = convex)
})

#' @rdname depthMedian-methods
#' @export
methods::setMethod("depthMedian", "Depth", function(x, convex = FALSE) {
  pos <- which(x == max(x))
  med <- x@u[pos, ]

  if (ncol(x@u) != length(med) && convex) {
    med <- colMeans(med[chull(med), ])
  } else if(ncol(x@u) != length(med)) {
    med <- colMeans(med)
  }
  med

  med
})
