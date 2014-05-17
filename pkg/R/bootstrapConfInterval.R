#' @title Bootstrap confidence interval for depth median
#' 
#' @export
#' @importFrom boot boot
#'
medianDepthConfInterval = function(X, method="Projection", R = 1000, ...)
{ 
  boot
  depthTMP = function(X, indices, ...) depthMedian(X=X[indices,],...)  
  boot(X,depthTMP, R = R, method = method,...)
}
