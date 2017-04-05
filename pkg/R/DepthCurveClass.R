#' @rdname plot-methods
#' @export
setMethod("plot", signature = c(x = "DepthCurve"), function(x) {
  plot(new(paste0(class(x), "List"), x))
})

#' @rdname plot-methods
#' @export
setMethod("plot", signature = c(x = "DepthCurveList"), function(x) {
  p <- getPlot(x)
  print(p)
})
setMethod("initialize", "DepthCurveList", function(.Object, ...) {
  tmp <- list(...)
  n <- length(tmp)
  
  if (n > 0) {
    .Object[[1]] <- tmp[[1]]
    if (n > 1) {
      for (i in 2:length(tmp)) .Object <- combineDepthCurves(.Object, tmp[[i]])
    }
  }
  
  return(.Object)
})

#' @rdname combineDepthCurves-methods
#' @export
setMethod("combineDepthCurves", signature(.list = "list"),
  function(x, y, .list) {
    Reduce(combineDepthCurves, .list)
  }  
)
    
#' @rdname combineDepthCurves-methods
#' @export
setMethod("combineDepthCurves", signature(x = "DepthCurveList", y = "DepthCurve"),
          function(x, y, .list) {
            names <- sapply(x, function(xx) {
              xx@name
            })
            new_name <- y@name
            
            if (any(new_name == names)) {
              warning("Names in DepthCurveList are not unique!")
              k <- 1
              new_name_tmp <- paste0(new_name)
              
              while (any(new_name_tmp == names)) {
                new_name_tmp <- paste0(new_name, k)
                k <- k + 1
              }
              
              y@name <- new_name_tmp
            }
            
            n <- length(x)
            x[[n + 1]] <- y
            
            return(x)
          }
)

#' @rdname combineDepthCurves-methods
#' @export
setMethod("combineDepthCurves", signature(x = "DepthCurve", y = "DepthCurveList"),
          function(x, y, .list) {
            combineDepthCurves(y, x)
          }
)

#' @rdname combineDepthCurves-methods
#' @export
setMethod("combineDepthCurves", signature(x = "DepthCurve", y = "DepthCurve"),
          function(x, y, .list) {
            return(new(paste0(class(x), "List"), x, y))
          }
)
setMethod(".getPlot", "DepthCurveList", function(object) {
  value <- unlist(object)
  alpha <- as.vector(sapply(object, function(x) x@alpha))
  len_alpha <- sapply(object, function(x) length(x@alpha))
  names <- sapply(object, function(x) x@name)
  names <- rep(names, len_alpha)
  
  data <- data.frame(value, alpha, names)
  
  p <- ggplot()
  p <- p + geom_line(data = data, aes(x = alpha, y = value, col = names),
                     size = 1.5)
  p <- p + theme_bw() + .depTheme()
  p <- p + ylim(c(0, max(data$value, na.rm = TRUE)))
  p <- p + xlim(c(0, max(data$alpha, na.rm = TRUE)))
  
  return(p)
})

#' @rdname as.matrix-methods
#' @export
setMethod("as.matrix", signature(x = "DepthCurveList"), function(x) {
  names <- sapply(x, function(x) x@name)
  tmp <- matrix(unlist(x), ncol = length(x))
  colnames(tmp) <- names
  tmp
})
setMethod("show", "DepthCurve", function(object) {
  cat("Object of class:", class(object))
  plot(object)
})
setMethod("show", "DepthCurveList", function(object) {
  cat("Object of class:", class(object))
  print(getPlot(object))
})