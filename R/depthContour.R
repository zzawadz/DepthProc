#' @title Approximate depth contours
#'
#' @description Draws an approximate contours of depth for bivariate data.
#'
#' @param x Bivariate data
#' @param xlim Determines the width of x-axis.
#' @param ylim Determines the width of y-axis.
#' @param n Number of points in each coordinate direction to be used in contour plot.
#' @param pmean Logical. If TRUE mean will be marked.
#' @param mcol Determines the color of lines describing the mean.
#' @param pdmedian Logical. If TRUE depth median will be marked.
#' @param mecol Determines the color of lines describing the depth median.
#' @param legend Logical. If TRUE legend for mean and depth median will be drawn.
#' @param points Logical. If TRUE points from matrix x will be drawn.
#' @param colors function for colors pallete (e.g. gray.colors).
#' @param levels number of levels for color scale.
#' @param depth_params list of parameters for function depth (method, threads, ndir, la, lb, pdim, mean, cov, exact).
#' @param graph_params list of graphical parameters for functions filled.contour and contour (e.g. lwd, lty, main).
#' @param contour_method determines the method used to draw the contour lines. The default value ("auto") tries
#' to determine the best method for given depth function.
#' "convexhull" uses a convex hull algorithm to determine boundaries.
#' "contour" uses the algorithm from \code{\link{filled.contour}}.
#'
#' @details
#'
#' The set of all points that have depth at least \eqn{ \alpha } is called \eqn{ \alpha }-trimmed region. The \eqn{ \alpha }-trimmed region w.r.t. \eqn{ F } is denoted by \eqn{ {D}_{\alpha}(F) }, i.e., \deqn{ {D}_{\alpha}(F) = \left\{ z \in {{{R}} ^ {d}}:D(z, F) \ge \alpha\right\}. }
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @seealso \code{\link{depthPersp}}
#'
#' @examples
#' # EXAMPLE 1
#' set.seed(123)
#' x <- mvrnorm(1000, c(0, 0), diag(2))
#' depthContour(x, colors = gray.colors)
#' # with points
#' depthContour(x, points = TRUE)
#' depthContour(x, points = FALSE, levels = 10)
#'
#' # EXAMPLE 2
#' data(inf.mort, maesles.imm)
#' data1990 <- na.omit(cbind(inf.mort[, 1], maesles.imm[, 1]))
#' depthContour(data1990, n = 50, pmean = TRUE, mcol = "blue",
#'              pdmedian = TRUE, mecol = "brown", legend = TRUE, points = TRUE,
#'              depth_params = list(method = "LP"),
#'              graph_params = list(
#'                xlab = "infant mortality rate per 1000 live birth",
#'                ylab = "against masles immunized percentage",
#'                main = "L2 depth, UN Fourth Goal 2011 year"))
#'
#'
#' #EXAMPLE 3
#' data("france")
#' depthContour(france,
#'     depth_params = list(method = "Tukey"),
#'     points = TRUE
#' )
#'
#'
#' @keywords
#' contour
#' depth
#' @export
#'
depthContour <- function(x, xlim = extendrange(x[, 1], f = 0.1),
                         ylim = extendrange(x[, 2], f = 0.1), n = 50,
                         pmean = TRUE, mcol = "blue", pdmedian = TRUE,
                         mecol = "brown", legend = TRUE, points = FALSE,
                         colors = heat_hcl, levels = 10,
                         depth_params = list(),
                         graph_params = list(),
                         contour_method = c("auto", "convexhull", "contour")
                         ) {
  x_axis <- seq(xlim[1], xlim[2], length.out = n)
  y_axis <- seq(ylim[1], ylim[2], length.out = n)

  xy_surface <- expand.grid(x_axis, y_axis)
  xy_surface <- cbind(xy_surface[, 1], xy_surface[, 2])

  ux_list <- list(u = xy_surface, X = x)

  depth_params_list <- c(ux_list, depth_params)

  depth_surface_raw <- do.call(depth, depth_params_list)
  depth_surface <- matrix(depth_surface_raw, ncol = n)

  if (length(levels) == 1 && is.numeric(levels)) {
    # levels must include 0
    levels <- levels + 1
    levels <- seq(min(depth_surface), max(depth_surface), length.out = levels)
  } else {
    stop("Levels must be numeric vector of length 1.")
  }

  # set contour method
  contour_method <- contour_method[1]
  if(contour_method == "auto") {
    dp_method <- depth_params[["method"]]
    if(!is.null(dp_method) && dp_method == "Tukey") {
      contour_method <- "convexhull"
    } else {
      contour_method <- "contour"
    }
  }

  addConvexHull <- function(data, depth, cutoff, col = "black") {
    idx <- depth >= cutoff
    x <- data[idx,1]
    y <- data[idx,2]
    hpts <- grDevices::chull(x = x, y = y)
    hpts <- c(hpts, hpts[1])
    graphics::polygon(x[hpts], y[hpts], border = 1, col = col)
  }

  do.call(
    filled.contour,
    c(
      list(
        x = x_axis,
        y = y_axis,
        z = depth_surface,
        color.palette = colors,
        levels = levels,
        plot.axes = quote({
          do.call(contour, c(list(x = x_axis, y = y_axis, z = depth_surface,
                                  add = TRUE, drawlabels = FALSE,
                                  levels = levels),
                             graph_params))

          if(contour_method == "convexhull") {
            colors_all <- colors(length(levels))
            for(lvl in seq_along(levels)) {
              addConvexHull(
                xy_surface,
                depth_surface_raw,
                cutoff = levels[lvl],
                col = colors_all[lvl]
              )
            }
          }

          if (points) {
            points(x[, 1], x[, 2], col = "white", pch = 19, cex = 1.2)
            points(x[, 1], x[, 2], cex = 1.3, col = "grey")
          }
          if (pmean) {
            points(mean(x[, 1]), mean(x[, 2]), pch = 4, col = mcol, cex = 1.5,
                   lwd = 2.5)
            # abline(col = mcol, lwd = 1.8)
          }
          if (pdmedian) {
            depth_params$u <- x
            depths <- do.call(depth, depth_params)
            med <- x[depths == max(depths), ]

            if (ncol(x) != length(med)) {
              med <- colMeans(med)
            }

            dmedian <- med
            points(dmedian[1], dmedian[2], pch = 4, col = mecol, cex = 1.5,
                   lwd = 2.5)
            # abline(h = dmedian[2], col = mecol, lwd = 1.8)
          }
          if (legend) {

            if (pmean && pdmedian) {
              legend("topright", c("Mean", "Depth Median"),
                     col = c(mcol, mecol), pch = 4, bg = "white", pt.cex = 1.5,
                     pt.lwd = 2.5)
            }
            if (pmean && (!pdmedian)) {
              legend("topright", c("Mean"), col = mcol, pch = 4,
                     bg = "white", pt.cex = 1.5, pt.lwd = 2.5)
            }
            if (pdmedian && (!pmean)) {
              legend("topright", c("Depth Median"), col = mecol,
                     bg = "white", pt.cex = 1.5, pt.lwd = 2.5)
            }

          }

          axis(1)
          axis(2)
        }) # end quote
      ), # end list
      graph_params
    ) # end c
  ) # end do.call

}
