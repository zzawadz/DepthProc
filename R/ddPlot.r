#' @name ddPlot
#' @title Depth versus depth plot
#' @export
#' @description
#' Produces a DD plot which allows to compare two multivariate datasets or to compare a subject dataset with theoretical distribution.
#'
#' @param x The first or only data sample for ddPlot.
#' @param y The second data sample. \code{x} and \code{y} must be of the same space.
#' @param scale logical. determines whether the dispersion is to be aligned.
#' @param location determines whether the location is to be aligned to 0 vector with depth median.
#' @param name name for data set x. It will be passed to drawing function.
#' @param name_y as above for y
#' @param title title of the plot.
#' @param depth_params list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact).
#'
#' @details
#'
#' For two probability distributions \eqn{ F } and \eqn{ G }, both in \eqn{ {{{R}} ^ {d}} }, we can define \code{depth vs. depth} plot being very useful generalization of the one dimensional quantile-quantile plot: \deqn{ DD(F, G) = \left\{\left( D({z}, F), D({z}, G) \right), {z} \in {{{R}} ^ {d}} \right\} }
#' Its sample counterpart calculated for two samples \eqn{ {{{X}} ^ {n}} = \{{{X}_{1}}, ..., {{X}_{n}}\} } from \eqn{ F }, and \eqn{ {{Y} ^ {m}} = \{{{Y}_{1}}, ..., {{Y}_{m}}\} } from \eqn{ G } is defined as \deqn{ DD({{F}_{n}}, {{G}_{m}}) = \left\{\left( D({z}, {{F}_{n}}), D({z}, {{G}_{m}}) \right), {z} \in \{{{{X}} ^ {n}} \cup {{{Y}} ^ {m}}\} \right\} }
#'
#' @references
#' Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 822--831.
#'
#' Liu, R.Y., Singh K. (1993), A Quality Index Based on Data Depth and Multivariate Rank Test, \emph{Journal of the American Statistical Association} vol. 88.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#' library(sn)
#' library(mvtnorm)
#'
#' # EXAMPLE 1: Location difference
#' standard <- mvrnorm(1000, c(0, 0), diag(2))
#' shift <- mvrnorm(1000, c(0.5, 0), diag(2))
#' ddPlot(x = standard, y = shift, title = "Difference in position")
#' ddPlot(x = standard, y = shift, location = TRUE, title = "Location aligned")
#'
#' # EXAMPLE 2: Scale difference
#' standard <- mvrnorm(1000, c(0, 0), diag(2))
#' scale <- mvrnorm(1000, c(0, 0), 4 * diag(2))
#' ddPlot(x = standard, y = scale)
#' ddPlot(x = standard, y = scale, scale = TRUE)
#'
ddPlot <- function(x, y, scale = FALSE, location = FALSE, name = "X",
                   name_y = "Y", title = "Depth vs. depth plot",
                   depth_params = list()) {

  if (ncol(x) != ncol(y)) {
    stop("Wrong dimensions of the datasets! ncol(x) != ncol(y)")
  }
  if (scale) {
    uxname_list_x <- list(u = x, X = x)
    uxname_list_y <- list(u = y, X = y)
    depth_sample_x <- do.call(depth, c(uxname_list_x, depth_params))
    depth_sample_y <- do.call(depth, c(uxname_list_y, depth_params))
    varcovx <- cov(x[which(depth_sample_x >= median(depth_sample_x)), ])
    varcovy <- cov(y[which(depth_sample_y >= median(depth_sample_y)), ])
    x_new <- t(solve(chol(varcovx)) %*% t(x))
    y_new <- t(solve(chol(varcovy)) %*% t(y))
  } else {
    x_new <- x
    y_new <- y
  }
  if (location) {
    medx <- depthMedian(x_new, depth_params)
    medy <- depthMedian(y_new, depth_params)
    x_new <- sweep(x_new, 2, medx, "-")
    y_new <- sweep(y_new, 2, medy, "-")
  }

  data <- rbind(x_new, y_new)
  uxname_list_x_new <- list(u = data, X = x_new)
  uxname_list_y_new <- list(u = data, X = y_new)
  depth_x <- do.call(depth, c(uxname_list_x_new, depth_params))
  depth_y <- do.call(depth, c(uxname_list_y_new, depth_params))

  ddplot <- new("DDPlot", X = depth_x, Y = depth_y, title = title)

  return(ddplot)
}
