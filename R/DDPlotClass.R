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
  p <- ggplot2::ggplot()
  # eval(as.name("x")) - small hack to fix:
  # getPlot, DDPlot: no visible binding for global variable "x"
  # getPlot, DDPlot: no visible binding for global variable "y"
  # I cannot use aes(x, y)
  p <- p + ggplot2::geom_point(data = a_est, ggplot2::aes(eval(as.name("x")), eval(as.name("y"))),
                      color = "blue", shape = 1, size = 3)
  p <- p + ggplot2::theme_bw() + .depTheme()
  p <- p + ggplot2::ggtitle(object@title)
  p <- p + ggplot2::xlab("X depth")
  p <- p + ggplot2::ylab("Y depth")
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
