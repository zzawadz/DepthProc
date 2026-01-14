#' @title Basic function for functional depths
#'
#' @description Calculates depth functions.
#' @export
#'
#' @param u data
#' @param X reference set. If null u will be used as reference.
#' @param method depth method - "MBD" (default), or "FM" (Frainman-Muniz depth)
#' @param byrow logical or character.
#' @param \dots additional arguments passed to fncDepthFM.
#'
#' @importFrom zoo index
#' @rdname fncDepth
#' @examples
#'
#' x <- matrix(rnorm(60), ncol = 20)
#' DepthProc::fncDepth(x, method = "FM", dep1d = "Mahalanobis")
#' DepthProc::fncDepth(x, byrow = FALSE)
#'
#' # zoo and xts
#' data("sample_matrix", package = "DepthProc")
#' sample.xts <- xts::as.xts(sample_matrix, descr = "my new xts object")
#' DepthProc::fncDepth(sample.xts)
#'
fncDepth <- function(u, X = NULL, method = "MBD", byrow = NULL, ...) {

  if (!is.null(X)) {

    if (all(class(u) != class(X))) {
      stop("u and X must be the the same class!")
    }
  }

  UseMethod("fncDepth")
}

#' @export
#' @rdname fncDepth
fncDepth.matrix <- function(u, X = NULL, method = "MBD", byrow = NULL, ...) {

  fast_mbd <- FALSE
  if (is.null(X) && method == "MBD") {
    fast_mbd <- TRUE
  }
  if (is.null(X)) {
    X <- u
  }

  # For matrix - by default row is an observation
  if (is.null(byrow)) {
    byrow <- TRUE
  }
  if (!byrow) {
    u <- t(u)
    X <- t(X)
  }

  if (method == "FM") {
    dept <- (fncDepthFM(u, X, ...))
    depth <- methods::new("FncDepthFM", dept)
  }
  if (method == "MBD") {

    if (fast_mbd) {
      dept <- fncDepthMBD(u)
    } else {
      dept <- (fncDepthMBD(u, X))
    }

    depth <- methods::new("FncDepthMBD", dept)
  }

  depth@u <- u
  depth@X <- X
  depth@method <- method
  depth@index <- extractIndexFromMatrix(u)

  return(depth)
}

#' @export
#' @rdname fncDepth
fncDepth.zoo <- function(u, X = NULL, method = "MBD", byrow = NULL, ...) {

  if (is.null(byrow)) {
    byrow <- FALSE
  }
  if (is.null(X)) {
    X <- u
  }

  um <- as.matrix(u)
  Xm <- as.matrix(X)

  if (!byrow) {
    um <- t(um)
    Xm <- t(Xm)
  }

  depth <- fncDepth(um, Xm, method, byrow = TRUE, ...)

  if (!byrow) {
    depth@index <- index(u)
  }

  depth
}

#' @title FM Depth
#' @export
#' @description Computes Frainman-Muniz depth for functional data.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix. If it is a matrix or data frame, then each row is viewed as one multivariate observation.
#' @param dep1d_params parameters passed to depth function used in one dimension.
#'
#' @examples
#' x <- matrix(rnorm(60), nc = 20)
#' DepthProc::fncDepthFM(x)
#'
fncDepthFM <- function(u, X, dep1d_params = list(method = "Projection")) {

  if (missing(X)) {
    X <- u
  }

  depths <- rep(0, nrow(X))

  for (i in seq_len(ncol(X))) {

    dep1d_params$u <- u[, i]
    dep1d_params$X <- X[, i]

    depths <- depths + do.call(depth, dep1d_params)
  }

  depths <- as.numeric(depths / ncol(X))

  return(depths)
}

#'@title Modified band depth
#'@export
#'@description Computes the modified band depth.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix. If it is a matrix or data frame, then each row is viewed as one multivariate observation.
#'
#' @examples
#'
#' x <- matrix(rnorm(60), nc = 20)
#' DepthProc::fncDepthMBD(x)
#' DepthProc::fncDepthMBD(x, x)
#'
fncDepthMBD <- function(u, X) {

  if (missing(X)) {
    depth <- fastMBD(t(u))
  } else {
    depth <- fastMBDRef(t(u), t(X))
  }

  as.numeric(depth)
}

fastMBD <- function(u)
{
  p <- nrow(u)
  n <- ncol(u)
  rmat <- apply(u, 1, rank)
  down <- rmat - 1
  up <- n - rmat
  (rowSums(up * down) / p + n - 1) / choose(n, 2)
}

fastMBDRef <- function(u, X) {

  p <- nrow(X)
  n <- ncol(X)

  rmat <- u

  for(i in 1:p) {
    rmat[i, ] <- refRank(u[i, ], X[i, ])
  }

  rmat <- t(rmat)

  down <- rmat
  up <- n - rmat
  (rowSums(up * down) / p + n - 1) / choose(n, 2)
}


#'@title Band Depth
#'@export
#'@description Computes the band depth.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data according to which the depth is calculated, optional
#'
#' @examples
#'
#' x <- matrix(rnorm(60), nc = 20)
#' DepthProc::fncDepthMBD(x)
#' DepthProc::fncDepthMBD(x, x)
#'
fncDepthBD <- function(u, X) {

  if (missing(X)) {
    depth <- fastBD(t(u))
  } else {
    depth <- fastBDRef(t(u), t(X))
  }

  as.numeric(depth)
}

fastBD <- function(u)
{
  p <- nrow(u)
  n <- ncol(u)
  rmat <- apply(u, 1, rank)
  down <- apply(rmat,1,min) - 1
  up <- n - apply(rmat,1,max)
  ((up * down)+ n - 1) / choose(n, 2)
}

fastBDRef <- function(u, X) {

  p <- nrow(X)
  n <- ncol(X)

  rmat <- u

  for(i in 1:p) {
    rmat[i, ] <- DepthProc:::refRank(u[i, ], X[i, ])
  }

  rmat <- t(rmat)

  down <- apply(rmat,1,min)
  up <- n - apply(rmat,1,max)
  ((up * down) + n - 1) / choose(n, 2)
}
