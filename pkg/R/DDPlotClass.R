

setMethod("plot", signature = c(x = "DDPlot", y = "missing"), function(x, y = "missing"){
  p = getPlot(x)
  print(p)
})

setMethod("getPlot", "DDPlot", function(object){
  a_est = data.frame(x = object@X, y = object@Y)
  p = ggplot()
  p = p + geom_point(data = a_est, aes(x,y), color = "blue", 
                     shape = 1, size = 3)
  p = p + theme_bw() + .depTheme()
  p = p + ggtitle("Depth vs. depth plot")
  p = p + xlab("X depth")
  p = p + ylab("Y depth")
  p = p + ylim(c(0, max(a_est$y)))
  p = p + xlim(c(0, max(a_est$x)))
  p = p + geom_abline(color = "grey")
  return(p)
})

setMethod("show", "DDPlot", function(object){
  cat("DDPlot\n")
  cat("\nDepth Metohod:\n\t", object@X@method)
})

setGeneric("indexLiu", function(ddplot, gamma) standardGeneric("indexLiu"))
setMethod("indexLiu", signature(ddplot = "DDPlot", gamma = "numeric"),
function(ddplot, gamma)
{
  tmp = abs(as.vector(ddplot@X - ddplot@Y))
  indLiu = sapply(gamma, function(x) sum(tmp>x))
  return(indLiu)
})