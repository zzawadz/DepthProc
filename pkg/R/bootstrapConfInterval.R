#' @title Bootstrap confidence interval for depth median
#' 
#' @importFrom boot boot
#'
medianDepthConfInterval = function(x, method="Projection", R = 1000, ...)
{ 
  depthTMP = function(x, indices, ...) depthMedian(x=x[indices,],...)  
  boot(x,depthTMP, R = R, method = method,...)
}
