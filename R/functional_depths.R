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
#' DepthProc::fncDepth(x, method = "FM", dep1d_params = list(method = "Mahalanobis"))
#' DepthProc::fncDepth(x, byrow = FALSE)
#'
#' # zoo and xts
#' data("sample_matrix", package = "xts")
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
  } else if (method == "MBD") {

    if (fast_mbd) {
      dept <- fncDepthMBD(u)
    } else {
      dept <- (fncDepthMBD(u, X))
    }

    depth <- methods::new("FncDepthMBD", dept)
  } else {
    stop(gettextf(
      "unknown functional depth method %s; must be one of %s%s",
      sQuote(method),
      paste(sQuote(c("MBD", "FM")), collapse = ", "),
      if (identical(method, "BD")) " (band depth is available as fncDepthBD)" else ""
    ))
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
  if (is.vector(X)) {
    X <- matrix(X, nrow = 1)
  }
  if (is.vector(u)) {
    u <- matrix(u, ncol = ncol(X))
  }
  if (ncol(u) != ncol(X)) {
    stop(gettextf(
      "'u' is observed at %d point(s) but 'X' at %d; they must match",
      ncol(u), ncol(X)
    ))
  }

  # The loop below runs once per observation point. Calling the public depth()
  # in it re-ran the whole dispatcher every iteration -- including the MBD/FM
  # arms, which call straight back into fncDepth(), so the two dispatch routes
  # were mutually recursive. Resolve and validate the univariate method once,
  # here, and call its implementation directly.
  # Not as.list(): fncDepth() forwards `...` here, and R partial-matches a bare
  # dep1d = "Mahalanobis" onto dep1d_params. That used to land the string in
  # depth()'s `method` by position and so appeared to work; say what is wrong
  # instead of depending on an argument order.
  if (!is.list(dep1d_params)) {
    stop(gettextf(
      paste("'dep1d_params' must be a list of arguments for the univariate",
            "depth, not %s; did you mean dep1d_params = list(method = %s)?"),
      sQuote(class(dep1d_params)[1L]),
      if (is.character(dep1d_params) && length(dep1d_params) == 1L) {
        dQuote(dep1d_params)
      } else {
        "..."
      }
    ))
  }

  method <- dep1d_params$method
  if (is.null(method)) {
    method <- "Projection"
  }
  threads <- dep1d_params$threads
  if (is.null(threads)) {
    threads <- -1
  }
  dep1d_params[c("u", "X", "method", "threads")] <- NULL

  # The seam the two taxonomies used to leave open: going back through depth()
  # made the functional methods reachable from here, and they are defined over a
  # whole sample of curves rather than over one point of one, so asking for one
  # recursed until the stack ran out.
  if (method %in% c("MBD", "FM")) {
    stop(gettextf(
      paste("%s is a depth for a sample of curves, not for a single point of",
            "one, so it cannot be the univariate depth in 'dep1d_params'"),
      sQuote(method)
    ))
  }

  # a local named `depth` here would resolve to the package's own depth()
  # function rather than erroring, so this one is deliberately not called that
  dep1d <- .depthMethod(method)
  dep1d_args <- c(list(u = NULL, X = NULL, threads = threads), dep1d_params)

  depths <- rep(0, nrow(u))

  for (i in seq_len(ncol(X))) {

    dep1d_args$u <- u[, i]
    dep1d_args$X <- X[, i]

    depths <- depths + do.call(dep1d, dep1d_args)
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
    if (is.vector(u)) {
      u <- matrix(u, nrow = 1)
    }
    depth <- fastMBD(t(u))
  } else {
    if (is.vector(X)) {
      X <- matrix(X, nrow = 1)
    }
    if (is.vector(u)) {
      u <- matrix(u, ncol = ncol(X))
    }
    if (ncol(u) != ncol(X)) {
      stop(gettextf(
        "'u' is observed at %d point(s) but 'X' at %d; they must match",
        ncol(u), ncol(X)
      ))
    }
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

  # refRank returns 0 where u lies below every reference curve, so the
  # self-reference correction has to be clamped: without it down goes to -1 and
  # the depth comes out negative. Where u is one of the reference curves
  # refRank is at least 1, so the clamp never fires and the self-consistency
  # with fastMBD is untouched.
  down <- pmax(rmat - 1, 0)
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
    rmat[i, ] <- refRank(u[i, ], X[i, ])
  }

  rmat <- t(rmat)

  # clamped for the same reason as in fastMBDRef: a curve below the whole
  # reference sample has refRank 0 everywhere, which would make down -1
  down <- pmax(apply(rmat,1,min) - 1, 0)
  up <- n - apply(rmat,1,max)
  ((up * down) + n - 1) / choose(n, 2)
}
