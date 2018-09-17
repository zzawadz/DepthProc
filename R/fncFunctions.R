# Simple functions based on functional depths

#' @title Functional median
#' @description Calculate functional median based on data depth.
#' @export
#'
#' @param u data matrix
#' @param X reference set. If null u will be used as reference.
#' @param method depth method
#' @param byrow byrow
#' @param unique if true
#' @param \dots other arguments passed to fncDepth
#'
#' @examples
#'
#' x <- matrix(rnorm(600), nc = 20)
#' md <- fncDepthMedian(x, method = "FM", dep1d = "Mahalanobis")
#'
fncDepthMedian <- function(u, X = NULL, method = "MBD", byrow = NULL,
                           unique = TRUE, ...) {
  depths <- fncDepth(u, X, method = method, byrow = byrow, ...)
  meds <- depths@u[depths == max(depths), , drop = FALSE] #nolint

  if (unique) {
    meds <- colMeans(meds)
  }

  return(meds)
}

#' @title Functional bands
#' @description Extract bands from functional depth object.
#' @export
#'
#' @param obj object that inherits from FunctionalDepth.
#' @param band single numeric value.
#'
#' @examples
#'
#' x <- matrix(rnorm(600), nc = 20)
#' obj <- fncDepth(x, method = "FM", dep1d = "Mahalanobis")
#' fncGetBand(obj)
#'
fncGetBand <- function(obj, band = 0.5) {
  u <- obj@u
  depths <- as.numeric(obj)
  bands_q <- stats::quantile(obj, 1 - band)
  tmp_u <- u[depths >= bands_q, , drop = FALSE] #nolint
  bands <- t(apply(tmp_u, 2, range))
  methods::new("FncBand", bands, index = obj@index, level = band)
}
