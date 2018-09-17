#' @rdname getPlot-methods
#' @export
setMethod("getPlot", "AsymmetryCurveList", function(object) {
  p <- .getPlot(object)
  p <- p + ggplot2::ggtitle("Asymmetry Curve")
  p <- p + ggplot2::ylab("")
  p <- p + ggplot2::xlab("Alpha")
  return(p)
})
