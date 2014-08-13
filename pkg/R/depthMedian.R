#' @title Depth median
#' @export
#' 
setGeneric("depthMedian", function(x,...) standardGeneric("depthMedian"))

setMethod("depthMedian", "matrix", function(x,...)
{
  depths = depth(x,x,...)
  med = x[depths == max(depths),]
  if(ncol(x) != length(med)) med = colMeans(med)
  med
})

setMethod("depthMedian", "Depth", function(x)
{
  pos = which(x== max(x))
  med = x@u[pos,]
  if(ncol(x@u) != length(med)) med = colMeans(med)
  med  
})

