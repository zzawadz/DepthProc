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
#' fncBoxPlot(x, bands = c(0, 0.5, 1), method = "FM")
#' fncBoxPlot(x, bands = c(0, 0.5, 1), method = "FM", byrow = FALSE)
#'
#' colnames(x) <- paste0("f", 1:ncol(x))
#' fncBoxPlot(x, bands = c(0, 0.5, 1), method = "FM")
#'
#' # fncBoxPlot handles zoo and xts objects
#' library(xts)
#' x <- matrix(rnorm(200), ncol = 10)
#' time <- as.POSIXct(1:ncol(x) * 86400, origin = "1970-01-01")
#' x_xts <- xts(t(x), order.by = time)
#' fncBoxPlot(x_xts, bands = c(0, 0.5, 1), method = "FM")
#'
#' data("katowice.pollination")
#' pl <- fncBoxPlot(katowice.pollination, bands = c(0, 0.5, 1), method = "MBD")
#' pl + ggtitle("Air pollination in Katowice") + labs(y= "pollination ", x = "hour ")
#'
fncBoxPlot <- function(u, X = NULL, bands = c(0, 0.5), method = "MBD",
                       byrow = NULL, ...) {

  depths <- fncDepth(u, X, method = method, byrow = byrow, ...)

  .fncBoxPlotGGPlot(depths, bands)
}

# Create Functional BoxPlot based on ggplot2
.fncBoxPlotGGPlot <- function(obj, bands) {
  bands <- fncGetBandsDataFrame(obj, bands)

  p <- ggplot(bands, aes_string(x = "index", fill = "level", color = "level"))
  p <- p + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"))
  p <- p + theme_bw()
  p <- p + scale_fill_brewer(palette = "Blues") +
    scale_color_brewer(palette = "Blues")

  if (is.factor(bands$fac_index)) {
    labels <- unique(as.character(bands$fac_index))
    index <- unique(bands$index)

    p <- p + scale_x_continuous(breaks = index, labels = labels)
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
