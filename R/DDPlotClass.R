#' @rdname plot-methods
#' @export
methods::setMethod("plot", signature = c(x = "DDPlot"), function(x) {
  p <- getPlot(x)
  print(p)
})

#' @rdname getPlot-methods
#' @export
methods::setMethod("getPlot", "DDPlot", function(object) {
  a_est <- data.frame(x = object@X, y = object@Y)

  # An object built without the sample slot - ddMvnorm, or one from before the
  # slot existed - keeps the single-colour plot it has always had.
  by_sample <- length(object@sample) == nrow(a_est) &&
    nlevels(object@sample) > 1L

  p <- ggplot2::ggplot()
  # eval(as.name("x")) - small hack to fix:
  # getPlot, DDPlot: no visible binding for global variable "x"
  # getPlot, DDPlot: no visible binding for global variable "y"
  # I cannot use aes(x, y)
  if (by_sample) {
    a_est$sample <- object@sample
    p <- p + ggplot2::geom_point(data = a_est,
                        ggplot2::aes(eval(as.name("x")), eval(as.name("y")),
                                     color = eval(as.name("sample"))),
                        shape = 1, size = 3)
    p <- p + ggplot2::scale_color_manual(
      name = NULL,
      values = .ddPlotColors(nlevels(object@sample)))
  } else {
    p <- p + ggplot2::geom_point(data = a_est, ggplot2::aes(eval(as.name("x")), eval(as.name("y"))),
                        color = "blue", shape = 1, size = 3)
  }
  p <- p + ggplot2::theme_bw() + .depTheme()
  p <- p + ggplot2::ggtitle(object@title)
  p <- p + ggplot2::xlab(paste(object@name, "depth"))
  p <- p + ggplot2::ylab(paste(object@name_y, "depth"))
  p <- p + ggplot2::ylim(c(0, max(a_est$y)))
  p <- p + ggplot2::xlim(c(0, max(a_est$x)))
  p <- p + ggplot2::geom_abline(color = "grey")

  return(p)
})
methods::setMethod("show", "DDPlot", function(object) {
  cat("DDPlot\n")
  plot(object)
  cat("\nDepth Metohod:\n\t", object@X@method)
})
methods::setGeneric("indexLiu", function(ddplot, gamma) standardGeneric("indexLiu"))
methods::setMethod("indexLiu", signature(ddplot = "DDPlot", gamma = "numeric"),
          function(ddplot, gamma) {
            tmp <- abs(as.vector(ddplot@X - ddplot@Y))
            indLiu <- vapply(gamma, FUN.VALUE = 0, function(x) {
              sum(tmp > x)
            })

            return(indLiu)
          }
)
