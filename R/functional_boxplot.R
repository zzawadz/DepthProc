#' Functional boxplot based on Modified Band Depth
#' @export
#'
#' @param u data matrix
#' @param X reference set. If null u will be used as reference.
#' @param bands limits for bands
#' @param method depth method
#' @param byrow byrow
#' @param \dots other arguments passed to fncDepth
#'
#' @examples
#'
#' # some data:
#' x <- matrix(rnorm(200), ncol = 10)
#'
#' DepthProc::fncBoxPlot(x, bands = c(0, 0.5, 1), method = "MBD")
#' DepthProc::fncBoxPlot(x, bands = c(0, 0.5, 1), method = "MBD", byrow = FALSE)
#'
#' colnames(x) <- paste0("f", 1:ncol(x))
#' DepthProc::fncBoxPlot(x, bands = c(0, 0.5, 1), method = "MBD")
#'
#' # fncBoxPlot handles zoo and xts objects
#' x <- matrix(rnorm(200), ncol = 10)
#' time <- as.POSIXct(1:ncol(x) * 86400, origin = "1970-01-01")
#' x_xts <- xts::xts(t(x), order.by = time)
#' DepthProc::fncBoxPlot(x_xts, bands = c(0, 0.5, 1), method = "FM")
#'
#' data("katowice.airpollution", package = "DepthProc")
#' pl <- DepthProc::fncBoxPlot(katowice.airpollution, bands = c(0, 0.5, 1), method = "MBD")
#' pl + ggplot2::ggtitle("Air pollution in Katowice") +
#'   ggplot2::labs(y = "pollination ", x = "hour ")
#'
fncBoxPlot <- function(u, X = NULL, bands = c(0, 0.5), method = "MBD",
                       byrow = NULL, ...) {

  depths <- fncDepth(u, X, method = method, byrow = byrow, ...)

  .fncBoxPlotGGPlot(depths, bands)
}

# Create Functional BoxPlot based on ggplot2
.fncBoxPlotGGPlot <- function(obj, bands) {
  bands <- fncGetBandsDataFrame(obj, bands)

  p <- ggplot2::ggplot(bands, ggplot2::aes(x = .data$index, fill = .data$level, color = .data$level))
  p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper))
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::scale_fill_brewer(palette = "Blues") +
    ggplot2::scale_color_brewer(palette = "Blues")

  if (is.factor(bands$fac_index)) {
    labels <- unique(as.character(bands$fac_index))
    index <- unique(bands$index)

    p <- p + ggplot2::scale_x_continuous(breaks = index, labels = labels)
  }

  return(p)
}

############## Other functions ############

fncBand2DataFrame <- function(band) {
  data.frame(index = band@index, lower = band[, 1], upper = band[, 2],
             level = band@level)
}

fncGetBandsDataFrame <- function(obj, bands = c(0.25, 0.75)) {
  bands <- sort(bands, decreasing = TRUE)
  bands_list <- lapply(bands, function(x) {
    fncBand2DataFrame(fncGetBand(obj, x))
  })

  bands <- Reduce(rbind, bands_list)

  levels <- paste0(bands$level * 100, "%")
  bands$level <- factor(levels, levels = unique(levels), ordered = TRUE)

  if (is.factor(bands$index)) {
    bands$fac_index <- bands$index
    bands$index <- as.numeric(bands$index)
  } else {
    bands$fac_index <- bands$index
  }

  bands
}
