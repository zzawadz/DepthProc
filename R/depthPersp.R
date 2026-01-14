#' @name depthPersp
#' @title Perspective plot for depth functions
#' @export
#' @importFrom lattice wireframe
#' @importFrom colorspace heat_hcl
#' @description Draws a perspective plot of depth function over x-y plane.
#'
#' @param x bivariate data
#' @param plot_method there are two options "lattice", and "rgl" --- see details
#' @param xlim limits for x-axis
#' @param ylim limits for y-axis
#' @param n number of points that will be used to create plot (\eqn{ n ^ 2 })
#' @param xlab description of x-axis
#' @param ylab description of y-axis
#' @param plot_title plot title (default NULL means paste(depth_params$method, "depth"))
#' @param colors function for colors pallete (e.g. gray.colors).
#' @param depth_params list of parameters for function depth ("method", "threads", "ndir", "la", "lb", "pdim", "mean", "cov", "exact").
#' @param graph_params list of graphical parameters for functions rgl::persp3d and lattice::wireframe.
#'
#' @details
#'
#' plot_method --- rgl package is not in depends list beacuse it may cause problems when OpenGL is not supported. To use plot_method = "rgl" you must load this package on your own.
#'
#' @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#' # EXAMPLE 1
#' x <- MASS::mvrnorm(100, c(0, 0), diag(2))
#' DepthProc::depthPersp(x, depth_params = list(method = "Euclidean"))
#'
#' # EXAMPLE 2
#' data("inf.mort", package = "DepthProc")
#' data("maesles.imm", package = "DepthProc")
#' data1990 <- na.omit(cbind(inf.mort[, 1], maesles.imm[, 1]))
#'
#' \dontrun{
#' DepthProc::depthPersp(data1990, plot_method = "rgl",
#'            depth_params = list(method = "Projection"))
#' }
#'
depthPersp <- function(x, plot_method = "lattice",
                       xlim = extendrange(x[, 1], f = 0.1),
                       ylim = extendrange(x[, 2], f = 0.1), n = 50, xlab = "x",
                       ylab = "y", plot_title = NULL, colors = heat_hcl,
                       depth_params = list(),
                       graph_params = list()) {

  if (dim(x)[2] == 2) {
    axis_x <- seq(xlim[1], xlim[2], length.out = n)
    axis_y <- seq(ylim[1], ylim[2], length.out = n)

    xy_surface <- expand.grid(axis_x, axis_y)
    xy_surface <- matrix(unlist(xy_surface), ncol = 2)

    ux_list <- list(u = xy_surface, X = x)

    depth_params <- c(ux_list, depth_params)

    z_surface <- do.call(depth, depth_params)
    method <- depth_params$method

    if (is.null(plot_title)) {
      plot_title <- paste(method, "depth")

      if(is.null(method)) {
        plot_title <- "Projection depth"
      }
    }

    graph_params <- c(ux_list, graph_params)

    ztmp <- z_surface * 100 / max(z_surface)

    # colors <- rev(rainbow(100, start = 0, end = 1 / 4))
    colors <- colors(100)
    col <- colors[ztmp]

    if (plot_method == "rgl") {

      do.call(rgl::persp3d,
              c(list(x = axis_x, y = axis_y, z = z_surface,
                     color = col, back = "lines", xlab = xlab, ylab = ylab,
                     zlab = plot_title),
                graph_params))
    }
    if (plot_method == "lattice") {
      do.call(lattice::wireframe,
              c(list(z_surface ~ xy_surface[, 1] + xy_surface[, 2],
                     colorkey = TRUE, drape = TRUE, xlab = xlab, ylab = ylab,
                     zlab = "", col.regions = colors, lwd = 0.4,
                     main = plot_title, scales = list(arrows = FALSE)),
                graph_params
              ))
    } else {
      print <- c("Wrong plot.method")
    }
  } else {
    print("Wrong matrix!")
  }
}
