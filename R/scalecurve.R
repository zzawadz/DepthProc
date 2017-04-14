#' @title Scale curve
#' @export
#'
#' @importFrom geometry convhulln
#'
#' @description Draws a scale curve: measure of dispersion.
#'
#' @param x Multivariate data as a matrix.
#' @param y Additional matrix with multivariate data.
#' @param alpha Vector with values of central area to be used in computation.
#' @param name Name of matrix X used in legend.
#' @param name_y Name of matrix Y used in legend.
#' @param title title of the plot.
#' @param depth_params list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact).
#' @param ... Any additional parameters for plotting.
#'
#' @details
#'
#' For sample depth function \eqn{ D({x}, {{{Z}} ^ {n}}) }, \eqn{ {x} \in {{{R}} ^ {d}} }, \eqn{ d \ge 2 }, \eqn{ {Z} ^ {n} = \{{{{z}}_{1}}, ..., {{{z}}_{n}}\} \subset {{{R}} ^ {d}} }, \eqn{ {{D}_{\alpha}}({{{Z}} ^ {n}}) } denoting \eqn{\alpha} --- central region, we can define the scale curve \eqn{ SC(\alpha) = \left(\alpha, vol({{D}_{\alpha}}({{{Z}} ^ {n}})\right) \subset {{{R}} ^ {2}} }, for \eqn{ \alpha \in [0, 1] }
#'
#' The scale curve is a two-dimensional method of describing the dispersion of random vector around the depth induced median.
#'
#' Function scalecurve for determining the volumes of the convex hull containing points from alpha central regions, uses function convhulln from geometry package.
#'
#' The minimal dimension of data in X or Y is 2.
#'
#' ggplot2 package is used to draw a plot.
#'
#' @return
#'
#' Returns the volume of the convex hull containing subsequent central points of \code{X}.
#'
#' @references
#'
#' Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 783--858.
#'
#' Chaudhuri, P. (1996), On a Geometric Notion of Quantiles for Multivariate Data, \emph{Journal of the American Statistical Association}, 862--872.
#'
#' Dyckerhoff, R. (2004), Data Depths Satisfying the Projection Property, \emph{Allgemeines Statistisches Archiv.}, \bold{88}, 163--190.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @seealso \code{\link{depthContour}} and \code{\link{depthPersp}} for depth graphics.
#'
#' @examples
#' library(mvtnorm)
#' x <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 3 * diag(2))
#' y <- rmvt(n = 100, sigma = diag(2), df = 2)
#' scaleCurve(x, y, depth_params = list(method = "Projection"))
#' # Comparing two scale curves
#' # normal distribution and mixture of normal distributions
#' x <- mvrnorm(100, c(0, 0), diag(2))
#' y <- mvrnorm(80, c(0, 0), diag(2))
#' z <- mvrnorm(20, c(5, 5), diag(2))
#' scaleCurve(x, rbind(y, z), name = "N", name_y = "Mixture of N",
#'            depth_params = list(method = "Projection"))
#'
#' @keywords
#' multivariate
#' nonparametric
#' robust
#' depth function
#' scale curve
#'
scaleCurve <- function(x, y = NULL, alpha = seq(0, 1, 0.01), name = "X",
                       name_y = "Y", title = "Scale Curve",
                       depth_params = list(method = "Projection"), ...) {
  x <- na.omit(x)

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (!is.matrix(x)) {
    stop("x must be a matrix or data frame!")
  }
  if (!is.null(y)) {

    if (is.data.frame(y)) {
      y <- as.matrix(y)
    }
    if (!is.matrix(y)) {
      stop("y must be a matrix or data frame!")
    }
  }

  dim_x <- dim(x)[2]

  uxname_list <- list(u = x, X = x)

  depth_est <- do.call(depth, c(uxname_list, depth_params))

  k <- length(alpha)
  vol <- 1:k

  for (i in 1:k) {
    tmp_x <- x[depth_est >= alpha[i], ]
    np <- nrow(as.matrix(tmp_x))

    if (np > dim_x) {
      vol[i] <- convhulln(tmp_x, options = "FA")$vol
    } else {
      vol[i] <- 0
    }
  }

  scale_curve <- new("ScaleCurve", rev(vol), alpha = alpha, depth = depth_est,
                     name = name, title = title)

  if (!is.null(y)) {
    name <- name_y
    sc_tmp <- scaleCurve(x = y, y = NULL, alpha = alpha, name = name,
                         name_y = "Y", depth_params = depth_params)
    scale_curve <- combineDepthCurves(scale_curve, sc_tmp)
  }

  return(scale_curve)
}
