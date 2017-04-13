#' @rdname getPlot-methods
#' @export
setMethod("getPlot", "ScaleCurveList", function(object) {
  p <- .getPlot(object)
  p <- p + ggtitle(object[[1]]@title)
  p <- p + ylab("Volume")
  p <- p + xlab("1 - alpha")
  return(p)
})
