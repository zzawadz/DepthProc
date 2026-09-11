# The one table mapping a depth method name to the function that implements it.
#
# It used to be a switch() inside depth() -- and fncDepthFM() reached it by
# calling back into depth() once per observation point, with a different method
# than the one its own caller had asked for. depth(method = "FM") therefore ran
# fncDepth() -> fncDepthFM() -> depth(), so the whole dispatcher, and anything
# ever added to it, sat inside a hot loop, and the two routes into it were free
# to disagree about which methods exist. fncDepthFM() now resolves an entry here
# once, before its loop, and calls it directly.
#
# Every entry takes (u, X, threads, ...) so a caller does not have to know which
# methods use threads and which ignore them; .depthMethod() is the only place
# that has to.
.depthMethods <- list(
  Mahalanobis = function(u, X, threads, ...) {
    depthMah(u, X, threads = threads, ...)
  },
  Euclidean = function(u, X, threads, ...) {
    depthEuclid(u, X)
  },
  Projection = function(u, X, threads, ...) {
    depthProjection(u, X, threads = threads, ...)
  },
  Tukey = function(u, X, threads, ...) {
    depthTukey(u, X, threads = threads, ...)
  },
  LP = function(u, X, threads, ...) {
    depthLP(u, X, threads = threads, ...)
  },
  Local = function(u, X, threads, ...) {
    depthLocal(u, X, ...)
  },
  MBD = function(u, X, threads, ...) {
    fncDepth(u, X, method = "MBD", ...)
  },
  FM = function(u, X, threads, ...) {
    fncDepth(u, X, method = "FM", ...)
  }
)

# Validates a method name and returns it. Split out of .depthMethod() so the
# raw-value table below can reuse the one set of checks rather than growing a
# second, drifting copy of them.
.depthMethodName <- function(method) {
  if (!is.character(method)) {
    stop(gettextf("'method' must be a character value, not %s",
                  sQuote(class(method)[1L])))
  }
  if (length(method) != 1L) {
    stop(gettextf("'method' must be a single value, not a vector of length %d",
                  length(method)))
  }
  if (is.na(method) || !(method %in% names(.depthMethods))) {
    stop(gettextf(
      "unknown depth method %s; must be one of %s",
      sQuote(method),
      paste(sQuote(names(.depthMethods)), collapse = ", ")
    ))
  }

  method
}

# Validates a method name and returns its implementation. Splitting this out of
# depth() is what lets a caller resolve the method once and then call it many
# times without paying for -- or bypassing -- the validation.
.depthMethod <- function(method) {
  .depthMethods[[.depthMethodName(method)]]
}

# The raw-value twin of .depthMethods: the same methods behind the same
# (u, X, threads, ...) calling convention, but returning the plain numeric
# vector instead of the S4 Depth object built around it.
#
# Every depthXxx() ends by handing its numeric result to methods::new() along
# with u, X and the method name. That object is the right return value for a
# user calling depth() once. It is pure waste for the package's own loops,
# which call a depth function per iteration and immediately as.numeric() the
# result away: each iteration allocates the object, copies u and X into its
# slots and runs the class's validity check, then drops all of it. In
# .depthLocal() the X being copied is the 2 * nrow(X)-row symmetrised sample,
# once per row of u.
.depthValueMethods <- list(
  Mahalanobis = function(u, X, threads, ...) {
    .depthMahValues(u, X, threads = threads, ...)
  },
  Euclidean = function(u, X, threads, ...) {
    .depthEuclidValues(u, X)
  },
  Projection = function(u, X, threads, ...) {
    .depthProjectionValues(u, X, threads = threads, ...)
  },
  Tukey = function(u, X, threads, ...) {
    .depthTukeyValues(u, X, threads = threads, ...)
  },
  LP = function(u, X, threads, ...) {
    .depthLPValues(u, X, threads = threads, ...)
  }
)

# Resolves a method to a function returning depths as a bare numeric vector,
# with the same input contract as the public depth functions: the returned
# function coerces u and X itself, because its callers are loops that hand it a
# different slice each iteration -- fncDepthFM() passes bare columns and relies
# on the callee to stand them up as one-column matrices.
#
# Local, MBD and FM have no kernel of their own to expose -- Local is
# depthLocal()'s own loop over this very table, and the two functional depths
# are defined over a sample of curves and forward to fncDepth(). They keep the
# S4 route, unwrapped here, so that every name .depthMethod() accepts is also
# accepted by this one and the two tables cannot drift apart.
.depthValueMethod <- function(method) {
  name <- .depthMethodName(method)

  valueFun <- .depthValueMethods[[name]]
  if (is.null(valueFun)) {
    objectFun <- .depthMethods[[name]]
    return(function(u, X, threads, ...) {
      as.numeric(objectFun(u, X, threads = threads, ...))
    })
  }

  function(u, X, threads, ...) {
    dat <- .coerceDepthInput(u, X)
    # as.numeric() because the C++ kernels hand back an n x 1 matrix, which the
    # S4 route flattened on the way out of the Depth object. Without it the two
    # routes would return different shapes for the same call, and a caller that
    # indexed the result would quietly get a matrix.
    as.numeric(valueFun(dat$u, dat$X, threads = threads, ...))
  }
}

# The internal counterpart of depth(): same arguments, same dispatch, same
# validation, same coercion, but a bare numeric vector back. For callers that
# would have written as.numeric(do.call(depth, ...)).
.depthValues <- function(u, X, method = "Projection", threads = -1, ...) {
  .depthValueMethod(method)(u, X, threads = threads, ...)
}

#' @title Depth calculation
#'
#' @description Calculate depth functions.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#' @param method Character string which determines the depth function. \code{method} can be one of "Projection" (the default), "Mahalanobis", "Euclidean", "Tukey", "LP" or "Local" for multivariate data, or "MBD" or "FM" for functional data, in which case the call is forwarded to \code{\link{fncDepth}}. Any other value is an error. For details see \code{\link{depth}}.
#' @param threads number of threads used in parallel computations. Default value -1 means that all possible cores will be used. It is forwarded to the "Mahalanobis", "Projection", "LP" and "Tukey" methods; "Euclidean" and "Local" have no parallel implementation and ignore it.
#' @param ... parameters specific to method --- see \code{\link{depthEuclid}}
#'
#' @details
#'
#' {The Mahalanobis depth} \deqn{ {D}_{MAH}(y, {X} ^ {n}) = \frac{ 1 }{ 1 + {{(y - \bar{x})} ^ {T}}{{S} ^ {-1}}(y - \bar{x}) }, } where \eqn{ S } denotes the sample covariance matrix \eqn{ {X} ^ {n} }.
#'
#' A symmetric projection depth \eqn{ D\left( x, X\right) } of a point \eqn{ x \in {{{R}} ^ {d}} }, \eqn{ d \ge 1 } is defined as
#' \deqn{ D\left( x, X\right)_{PRO} = {{\left[ 1 + su{{p}_{\left\| u \right\| = 1}}\frac{ \left| {{u} ^ {T}}x - Med\left( {{u} ^ {T}}X\right)\right| }{ MAD\left( {{u} ^ {T}}X\right) }\right]} ^ {-1}}, }
#' where Med denotes the univariate median, \eqn{ MAD\left( Z \right) } = \eqn{ Med\left(\left| Z - Med\left( Z \right)\right|\right) }. Its sample version denoted by \eqn{ D\left( x, {X} ^ {n} \right) } or \eqn{ D\left( x, {X} ^ {n} \right) } is obtained by replacing \eqn{ F } by its empirical counterpart \eqn{ {{F}_{n}} } calculated from the sample \eqn{ {X} ^ {n} } .
#'
#' Next interesting depth is the weighted \eqn{ {L} ^ {p} } depth. The weighted \eqn{ {L} ^ {p} } depth \eqn{ D({x}, F) } of a point \eqn{ {x} \in {R} ^ {d} }, \eqn{ d \ge 1 } generated by \eqn{ d } dimensional random vector \eqn{ {X} } with distribution \eqn{ F }, is defined as \eqn{ D({x}, F) = \frac{1 }{ 1 + Ew({{\left\| x - X \right\| }_{p}}) }, } where \eqn{ w } is a suitable weight function on \eqn{ [0, \infty) }, and \eqn{ {{\left\| \cdot \right\| }_{p}} } stands for the \eqn{ {L} ^ {p} } norm (when p = 2 we have usual Euclidean norm). We assume that \eqn{ w } is non-decreasing and continuous on \eqn{ [0, \infty) } with \eqn{ w(\infty-) = \infty }, and for \eqn{ a, b \in {{{R}} ^ {d}} } satisfying \eqn{ w(\left\| a + b \right\|) \le w(\left\| a \right\|) + w(\left\| b \right\|) }. Examples of the weight functions are: \eqn{ w(x) = a + bx }, \eqn{ a, b > 0 } or \eqn{ w(x) = {x} ^ {\alpha} }. The empirical version of the weighted \eqn{ {L} ^ {p} } depth is obtained by replacing distribution \eqn{ F } of \eqn{ {X} } in \eqn{ Ew({{\left\| {x} - {X} \right\| }_{p}}) = \int {w({{\left\| x - t \right\| }_{p}})}dF(t) } by its empirical counterpart calculated from the sample \eqn{ {{{X}} ^ {n}} }...
#'
#' The Projection and Tukey's depths are calculated using an approximate algorithm. Calculations of Mahalanobis, Euclidean and \eqn{ L ^ p } depths are exact. Returns the depth of multivariate point u with respect to data set X.
#'
#' @references
#'
#' Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), Ann. Statist., 27, 783--858.
#'
#' Mosler K (2013). Depth statistics. In C Becker, R Fried, K S (eds.), Robustness and Complex Data Structures, Festschrift in Honour of Ursula Gather, pp. 17--34. Springer.
#'
#' Rousseeuw, P.J. and Struyf, A. (1998), Computing location depth and regression depth in higher dimensions, Stat. Comput., 8, 193--203.
#'
#' Zuo, Y. and Serfling, R. (2000), General Notions of Statistical Depth Functions, Ann. Statist., 28, no. 2, 461--482.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @seealso \code{\link{depthContour}} and \code{\link{depthPersp}} for depth graphics.
#'
#' @examples
#'
#' # Calculation of Projection depth
#' data("starsCYG", package = "robustbase")
#' DepthProc::depth(t(colMeans(starsCYG)), starsCYG)
#'
#' # Also for matrices
#' DepthProc::depth(starsCYG, starsCYG)
#'
#' # Projection depth applied to a large bivariate data set
#' x <- matrix(rnorm(9999), nc = 3)
#' DepthProc::depth(x, x)
#'
#' @keywords multivariate nonparametric robust depth function
#'
#' @export
#'
depth <- function(u, X, method = "Projection", threads = -1, ...) {

  dat <- .coerceDepthInput(u, X)

  depthFun <- .depthMethod(method)

  output <- depthFun(dat$u, dat$X, threads = threads, ...)

  return(output)
}

#' @title Euclidean Depth
#' @export
#'
#' @description Computes the euclidean depth of a point or vectors of points with respect to a multivariate data set.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#'
#' @details
#'
#' Calculation of Euclidean depth is exact.
#'
#' Returns the depth of multivariate point \code{u} with respect to data set \code{X}.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#' x <- matrix(rnorm(9999), nc = 3)
#' DepthProc::depthEuclid(x, x)
#'
#' @keywords multivariate nonparametric depth function
#'
depthEuclid <- function(u, X) {

  dat <- .coerceDepthInput(u, X)
  u <- dat$u
  X <- dat$X

  depth <- .depthEuclidValues(u, X)

  methods::new("DepthEuclid", depth, u = u, X = X, method = "Euclidean")
}

# Expects u and X already coerced by .coerceDepthInput().
.depthEuclidValues <- function(u, X) {
  n <- dim(u)[1]
  center <- colMeans(X)
  center <- matrix(rep(center, n), nrow = n, byrow = TRUE)
  1 / (1 + (rowSums((u - center) ^ 2)))
}

#' @title Mahalanobis Depth
#' @export
#' @description Computes the mahalanobis depth of a point or vectors of points with respect to a multivariate data set.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#' @param threads number of threads used in parallel computations. Default value -1 means that all possible cores will be used.
#' @param cov custom covariance matrix passed. If NULL standard calculations will be based on standard covariance estimator.
#' @param mean custom mean vector. If null --- mean average will be used.
#'
#' @details
#'
#' Calculation of Mahalanobis depth is exact.
#'
#' Returns the depth of multivariate point \code{u} with respect to data set \code{X}.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#' x <- matrix(rnorm(9999), nc = 3)
#' DepthProc::depthMah(x, x)
#'
#' @keywords multivariate nonparametric depth function
#'
depthMah <- function(u, X, cov = NULL, mean = NULL, threads = -1) {

  dat <- .coerceDepthInput(u, X)
  u <- dat$u
  X <- dat$X

  depth <- .depthMahValues(u, X, cov = cov, mean = mean, threads = threads)

  methods::new("DepthMahalanobis", depth, u = u, X = X, method = "Mahalanobis")
}

# Expects u and X already coerced by .coerceDepthInput().
.depthMahValues <- function(u, X, cov = NULL, mean = NULL, threads = -1) {

  if (is.null(cov) && nrow(X) < 2L) {
    # a single observation has no sample covariance; Armadillo returns a 1x1
    # from arma::cov() and the multiplication that follows throws inside an
    # OpenMP loop, which aborts the R session rather than raising an error
    # call = sys.call(-1) so the condition still names the public function the
    # user called, now that the guard lives one frame below it
    stop(simpleError(
      gettextf(
        paste("'X' has %d observation, which is not enough to estimate a",
              "covariance matrix; use at least two rows, or pass 'cov'"),
        nrow(X)
      ),
      call = sys.call(-1)
    ))
  }

  if (!is.null(mean)) {
    mean <- matrix(mean, ncol = length(mean))
  }

  depthMahCPP(u, X, cov, mean, threads)
}

#' @title Projection Depth
#' @export
#' @description Computes the Projection depth of a point or vectors of points with respect to a multivariate data set.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#' @param ndir number of directions used in computations
#' @param threads number of threads used in parallel computations. Default value -1 means that all possible cores will be used.
#'
#' @details
#'
#' Irrespective of dimension, Projection and Tukey's depth is obtained by approximate calculation.
#'
#' Returns the depth of multivariate point \code{u} with respect to data set \code{X}.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#' x <- matrix(rnorm(3000), nc = 3)
#' a <- DepthProc::depthProjection(x, x, ndir = 2000)
#'
#' @keywords multivariate nonparametric depth function
#'
depthProjection <- function(u, X, ndir = 1000, threads = -1) {

  dat <- .coerceDepthInput(u, X)
  u <- dat$u
  X <- dat$X

  depth <- .depthProjectionValues(u, X, ndir = ndir, threads = threads)

  methods::new("DepthProjection", depth, u = u, X = X, method = "Projection")
}

# Expects u and X already coerced by .coerceDepthInput().
.depthProjectionValues <- function(u, X, ndir = 1000, threads = -1) {
  depthProjCPP(u, X, ndir, threads)
}

#' @title Tukey Depth
#' @export
#' @description Computes the Tukey depth of a point or vectors of points with respect to a multivariate data set.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#' @param ndir number of directions used in computations
#' @param threads number of threads used in parallel computations. Default value -1 means that all possible cores will be used.
#' @param exact if TRUE the exact algorithm will be used. It is implemented for two-dimensional data only; for a higher-dimensional \code{X} the approximate algorithm is used instead and a warning is raised. One-dimensional data is always computed exactly, whatever \code{exact} is set to.
#'
#' @details
#'
#' Irrespective of dimension, Projection and Tukey's depth is obtained by approximate calculation.
#'
#' Returns the depth of multivariate point \code{u} with respect to data set \code{X}.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#' \dontrun{
#' x <- matrix(rnorm(3000), nc = 3)
#' DepthProc::depthTukey(x, ndir = 2000)
#' }
#'
#' # Exact algorithm in 2d
#' x <- matrix(rnorm(2000), nc = 2)
#' DepthProc::depthTukey(x, exact = TRUE)
#'
#' @keywords multivariate nonparametric depth function
#'
depthTukey <- function(u, X, ndir = 1000, threads = -1, exact = FALSE) {

  dat <- .coerceDepthInput(u, X)
  u <- dat$u
  X <- dat$X

  depth <- .depthTukeyValues(u, X, ndir = ndir, threads = threads,
                             exact = exact)

  methods::new("DepthTukey", depth, u = u, X = X, method = "Tukey")
}

# Expects u and X already coerced by .coerceDepthInput().
.depthTukeyValues <- function(u, X, ndir = 1000, threads = -1, exact = FALSE) {

  tukey1d <- function(u, X) {
    Xecdf <- ecdf(X)
    uecdf <- Xecdf(u)
    uecdf2 <- 1 - uecdf
    min.ecdf <- uecdf > uecdf2
    depth <- uecdf
    depth[min.ecdf] <- uecdf2[min.ecdf]
    depth
  }

  if (exact && ncol(X) > 2) {
    # the exact algorithm exists for 2d only; say so rather than quietly
    # handing back an approximation the caller explicitly asked not to get
    warning(simpleWarning(
      gettextf(
        paste("exact Tukey depth is available for two-dimensional data only;",
              "X has %d columns, so the approximate algorithm with ndir = %d",
              "random directions was used instead"),
        ncol(X), ndir
      ),
      call = sys.call(-1)
    ))
  }

  if (ncol(X) == 1) {
    depth <- tukey1d(u, X)
  } else if (ncol(X) == 2 && exact) {
    depth <- depthTukeyCPP(u, X, exact, threads)
  } else {
    # if number of dimensions is greater than 2
    proj <- t(runifsphere(ndir, ncol(X)))
    xut <- X %*% proj
    uut <- u %*% proj

    OD <- matrix(nrow = nrow(uut), ncol = ncol(uut))

    for (i in 1:ndir) {
      OD[, i] <- tukey1d(uut[, i], xut[, i])
    }

    depth <- apply(OD, 1, min)
  }

  depth
}

#' @title LP Depth
#' @export
#' @description Computes the LP depth of a point or vectors of points with respect to a multivariate data set.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#' @param pdim dimension used in calculating depth function.
#' @param la slope the weighing function.
#' @param lb intercept in the weighing function.
#' @param threads number of threads used in parallel computations. Default value -1 means that all possible cores will be used.
#' @param func the weighing function. Currently it is not supported.
#'
#' @details
#'
#' Returns the depth of multivariate point \code{u} with respect to data set \code{X}.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#' x <- matrix(rnorm(3000), ncol = 3)
#'
#' # Same results
#' DepthProc::depthLP(x, x, pdim = 2)
#'
#' @keywords multivariate nonparametric depth function
#'
depthLP <- function(u, X, pdim = 2, la = 1, lb = 1, threads = -1,
                    func = NULL) {

  dat <- .coerceDepthInput(u, X)
  u <- dat$u
  X <- dat$X

  depth <- .depthLPValues(u, X, pdim = pdim, la = la, lb = lb,
                          threads = threads, func = func)

  methods::new("DepthLP", depth, u = u, X = X, method = "LP")
}

# Expects u and X already coerced by .coerceDepthInput().
.depthLPValues <- function(u, X, pdim = 2, la = 1, lb = 1, threads = -1,
                           func = NULL) {

  if (!is.null(func)) {
    stop(simpleError("'func' is not supported yet; leave it as NULL",
                     call = sys.call(-1)))
  }

  depthLPCPP(u, X, pdim, la, lb, threads = threads)
}
