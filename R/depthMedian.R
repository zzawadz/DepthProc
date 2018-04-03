#' @title Depth median
#' @docType methods
#' @rdname depthMedian-methods
#'
#' @param x object of class Depth or matrix.
#' @param depth_params list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact).
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
setGeneric("depthMedian", function(x, depth_params = list()) {
  standardGeneric("depthMedian")
})

#' @rdname depthMedian-methods
#' @export
setMethod("depthMedian", "matrix", function(x, depth_params = list(), convex = TRUE) {
  ux_list <- list(u = x, X = x)
  depths <- do.call(depth, c(ux_list, depth_params))
  med <- x[depths == max(depths), ]

  if (ncol(x) != length(med) && convex) {
    med <- colMeans(med[chull(med),])
  } else if(ncol(x) != length(med)) {
    med <- colMeans(med)
  }
  med
})

#' @rdname depthMedian-methods
#' @export
setMethod("depthMedian", "data.frame", function(x, depth_params = list()) {
  x <- as.matrix(x)
  depthMedian(x, depth_params)
})

#' @rdname depthMedian-methods
#' @export
setMethod("depthMedian", "Depth", function(x) {
  pos <- which(x == max(x))
  med <- x@u[pos, ]

  if (ncol(x@u) != length(med)) {
    med <- colMeans(med)
  }

  med
})
