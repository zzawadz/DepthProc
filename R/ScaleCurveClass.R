#' @rdname getPlot-methods
#' @export
methods::setMethod("getPlot", "ScaleCurveList", function(object) {
  p <- .getPlot(object)
  p <- p + ggplot2::ggtitle(object[[1]]@title)
  p <- p + ggplot2::ylab("Volume")
  p <- p + ggplot2::xlab("1 - alpha")
  return(p)
})
