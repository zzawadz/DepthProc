.depthLocal <- function(u, X, beta, depth_params1, depth_params2) {
  ncol <- ncol(X)
  nrow <- nrow(X)

  # Fix for dim 1
  tmp <- t(apply(X, 1, function(k) {
    2 * u - k
  }))

  if (ncol(tmp) != ncol(X)) {
    tmp <- t(tmp)
  }

  symDATA <- rbind(X, tmp)

  uxDepthList1 <- list(u = X, X = symDATA)

  depths <- as.numeric(do.call(depth, c(uxDepthList1, depth_params1)))
  quan <- quantile(depths, probs = 1 - beta)
  Rset <- as.matrix(X[signif(depths, digits = 6) >= signif(quan, digits = 6), ])

  uxDepthList2 <- list(u = u, X = Rset)

  as.numeric(do.call(depth, c(uxDepthList2, depth_params2)))
}

#' @title Local depth
#'
#' @description Computes local version of depth according to proposals of Paindaveine and Van Bever --- see referencess.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame. If it is a matrix or data frame, then each row is viewed as one multivariate observation.
#' @param beta cutoff value for neighbourhood
#' @param depth_params1 list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact).
#' @param depth_params2 as above --- default is depth_params1.
#'
#' @details
#'
#' A successful concept of local depth was proposed by Paindaveine and Van Bever (2012). For defining a neighbourhood of a point authors proposed using idea of symmetrisation of a distribution (a sample) with respect to a point in which depth is calculated. In their approach instead of a distribution \eqn{ {P} ^ {X} }, a distribution \eqn{ {{P}_{x}} = \frac{ 1 }{ 2 }{{P} ^ {X}} + \frac{ 1 }{ 2 }{{P} ^ {2x - X}} } is used. For any \eqn{ \beta \in [0, 1] }, let us introduce the smallest depth region bigger or equal to \eqn{ \beta }, \deqn{ {R} ^ {\beta}(F) = \bigcap\limits_{\alpha \in A(\beta)} {{{D}_{\alpha}}}(F), } where \eqn{ A(\beta) = \left\{ \alpha \ge 0:P\left[ {{D}_{\alpha}}(F)\right] \ge \beta\right\} }. Then for a locality parameter \eqn{ \beta } we can take a neighbourhood of a point \eqn{ x } as \eqn{ R_{x} ^ {\beta}(P) }.
#'
#' Formally, let \eqn{ D(\cdot, P) } be a depth function. Then the local depth with the locality parameter \eqn{ \beta } and w.r.t. a point \eqn{ x } is defined as \deqn{ L{{D} ^ {\beta}}(z, P):z \to D(z, P_{x} ^ {\beta}), } where \eqn{ P_{x} ^ {\beta}(\cdot) = P\left( \cdot |R_{x} ^ {\beta}(P)\right) } is cond. distr. of \eqn{ P } conditioned on \eqn{ R_{x} ^ {\beta}(P) }.
#'
#' @references
#'
#' Paindaveine, D., Van Bever, G. (2013) From depth to local depth : a focus on centrality. Journal of the American Statistical Association 105, 1105--1119.
#'
#' @examples
#'
#' \dontrun{
#' # EXAMPLE 1
#' data <- MASS::mvrnorm(100, c(0, 5), diag(2) * 5)
#' # By default depth_params2 = depth_params1
#' DepthProc::depthLocal(data, data, depth_params1 = list(method = "LP"))
#' DepthProc::depthLocal(data, data, depth_params1 = list(method = "LP"),
#'            depth_params2 = list(method = "Projection"))
#' # Depth contour
#' DepthProc::depthContour(data, depth_params = list(method = "Local", depth_params1 = list(method = "LP")))
#'
#' # EXAMPLE 2
#' data("inf.mort", package = "DepthProc")
#' data("maesles.imm", package = "DepthProc")
#' data1990 <- na.omit(cbind(inf.mort[, 1], maesles.imm[, 1]))
#' DepthProc::depthContour(data1990,
#'              depth_params = list(
#'                method = "Local",
#'                depth_params1 = list(method = "LP"),
#'                beta = 0.3
#'              ))
#'
#' # EXAMPLE 3
#' Sigma1 <- matrix(c(10, 3, 3, 2), 2, 2)
#' X1 <- MASS::mvrnorm(n = 8500, mu = c(0, 0), Sigma1)
#' Sigma2 <- matrix(c(10, 0, 0, 2), 2, 2)
#' X2 <- MASS::mvrnorm(n = 1500, mu = c(-10, 6), Sigma2)
#' BALLOT <- rbind(X1, X2)
#'
#' train <- sample(1:10000, 100)
#' data <- BALLOT[train, ]
#' DepthProc::depthContour(data,
#'             depth_params = list(
#'               method = "Local",
#'               beta = 0.3,
#'               depth_params1 = list(method = "Projection")
#'             ))
#' }
#'
#' @export
#'
depthLocal <- function(u, X, beta = 0.5,
                       depth_params1 = list(method = "Projection"),
                       depth_params2 = depth_params1) {

  if (missing(X)) {
    X <- u
  }

  depths <- seq_len(nrow(u))

  for (i in seq_len(nrow(u))) {
    depths[i] <- .depthLocal(
      u[i,, drop = FALSE], X, beta, depth_params1, depth_params2) #nolint
  }

  methods::new("DepthLocal", depths, u = u, X = X, method = "Local",
      depth_params1 = depth_params1, depth_params2 = depth_params2)
}
