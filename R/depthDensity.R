#' @title Depth weighted density estimator
#'
#' @description Experimental function used to fit depth weighted density estimator.
#'
#' @param x numeric vector
#' @param y numeric vector
#' @param nx the number of equally spaced points at which the density is to be estimated in x-dimension.
#' @param ny the number of equally spaced points at which the density is to be estimated in x-dimension.
#' @param xg vector of point at which the density is to be estimated.
#' @param yg vector of point at which the density is to be estimated.
#' @param ... arguments passed to depthLocal.
#'
#' @references
#'
#' Kosiorowski D. and Zawadzki Z. (2014) Notes on optimality of predictive distribution pseudo-estimators in the CHARME models and automatic trading strategies, FindEcon2014, submitted
#'
#' @export
#' @examples
#'
#' \dontrun{
#' # .sampleData is special function for creating
#' # data for testing conditional denisty estimators
#' data <- DepthProc:::.sampleData(1:5, 100)
#' x <- data[, 1]
#' y <- data[, 2]
#' plot(x, y)
#' dep <- DepthProc::depthDensity(x, y)
#' plot(dep, type = "raw")
#' plot(dep, type = "depth")
#' }
#'
depthDensity <- function(x, y, nx = 5, ny = 32, xg = NULL, yg = NULL, ...) {

  if (is.null(xg)) {
    xy_grid <- .createGrid(x, y, nx, ny)
  }
  if (!is.null(xg)) {
    xy_grid <- .createGrid2(xg, yg)
  }

  xy <- cbind(x, y)

  dens_raw <- np::npudens(xy, edat = xy_grid)$dens
  dens_x <- np::npudens(x, edat = xy_grid[, 1])$dens

  dens <- dens_raw / dens_x

  # depth xy
  d_xy <- depthLocal(xy_grid, xy, ...)
  d_x <- depthLocal(xy_grid[, 1, drop = FALSE], xy[, 1, drop = FALSE], ...)

  dep_scale <- d_xy / d_x

  xg <- unique(xy_grid[, 1])
  yg <- unique(xy_grid[, 2])

  dens_mat <- matrix(dens, ncol = length(xg))
  dep_scale_mat <- matrix(dep_scale, ncol = length(xg))

  methods::new("DepthDensity", dep_scale = dep_scale_mat, density_raw = dens_mat,
      density = dens_mat / dep_scale_mat, xgrid = xg, ygrid = yg)
}

#' @title Plot function for DepthDensity.
#'
#' @description Create plot for DepthDensity. See \code{\link{depthDensity}} for more information.
#'
#' @param x object of class DepthDensity
#' @param type type of density that will be plotted. "depth" is a depth scaled density, and "raw" is denisty without scaling.
#' @param ... graphical arguments.
#'
#' @export
#'
methods::setMethod("plot", "DepthDensity", function(x, type = "depth", ...) {
  den <- x

  if (type == "raw") {
    density <- den@density_raw
  }
  if (type == "depth") {
    density <- den@density
  }

  mden <- max(density) * 1.05
  xg <- den@xgrid
  yg <- den@ygrid
  rn <- max(diff(xg))
  density <- density / max(mden) * rn
  xlim <- range(xg)
  xlim[2] <- xlim[2] + rn
  plot(xlim, range(yg), type = "n", ...)

  for (i in seq_len(length(xg))) {
    lines(xg[i] + density[, i], yg, ...)
  }

  abline(v = xg)
})

##################### Utils Functions ###################
.createGrid <- function(x, y, nx, ny) {
  xg <- seq(min(x), max(x), length.out = nx)
  yg <- seq(min(y), max(y), length.out = ny)

  r <- NULL
  tmp <- lapply(xg, function(i) {
    cbind(i, yg)
  })

  for (i in seq_len(length(tmp))) {
    r <- rbind(r, tmp[[i]])
  }

  r
}

.createGrid2 <- function(xg, yg) {
  r <- NULL
  tmp <- sapply(xg, function(i) {
    cbind(i, yg)
  }, simplify = FALSE)

  for (i in seq_len(length(tmp))) {
    r <- rbind(r, tmp[[i]])
  }

  r
}

.sampleData <- function(x, n) {
  r <- NULL
  tmp <- sapply(x, function(i) {
    cbind(i, rnorm(n, mean = i))
  }, simplify = FALSE)

  for (i in seq_len(length(tmp))) {
    r <- rbind(r, tmp[[i]])
  }

  r
}
