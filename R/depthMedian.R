#' @title Depth median
#' @docType methods
#' @rdname depthMedian-methods
#'
#' @param x object of class Depth or matrix.
#' @param depth_params list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact), or a \code{\link{depthSpec}}, which checks them. Used by the \code{matrix} and \code{data.frame} methods, which compute the depths; a \code{Depth} object already carries its own, so passing this to the \code{Depth} method is an error rather than a silent no-op.
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
#' DepthProc::depthMedian(x)
#'
#' # depthMedian works with object of class Depth
#' dp <- DepthProc::depth(x)
#' DepthProc::depthMedian(dp)
#'
methods::setGeneric("depthMedian", function(x, depth_params = list(), convex = FALSE) {
  standardGeneric("depthMedian")
})

#' @rdname depthMedian-methods
#' @importFrom grDevices chull
#' @export
methods::setMethod("depthMedian", "matrix", function(x, depth_params = list(), convex = FALSE) {
  ux_list <- list(u = x, X = x)
  depths <- do.call(depth, c(ux_list, .depthParams(depth_params)))
  med <- x[depths == max(depths), , drop = FALSE]

  if (nrow(med) > 1L) {
    if (convex && ncol(med) > 1L) {
      med <- med[chull(med), , drop = FALSE]
    }
    med <- colMeans(med)
  } else {
    med <- med[1L, ]
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
methods::setMethod("depthMedian", "Depth", function(x, depth_params = list(),
                                                   convex = FALSE) {
  # the generic and the shared help page both offer depth_params, but the depths
  # in a Depth object are already computed - accepting and discarding it would
  # hand back a median from the stored method with no sign the override was lost
  if (length(depth_params) > 0L) {
    stop(gettextf(
      paste("'depth_params' does not apply to an object of class %s: its depths",
            "were already computed with method %s. Call depthMedian() on the data",
            "itself to use a different method."),
      dQuote(class(x)[1L]), dQuote(x@method)
    ))
  }

  pos <- which(x == max(x))
  med <- x@u[pos, , drop = FALSE]

  if (nrow(med) > 1L) {
    if (convex && ncol(med) > 1L) {
      med <- med[chull(med), , drop = FALSE]
    }
    med <- colMeans(med)
  } else {
    med <- med[1L, ]
  }
  med
})
