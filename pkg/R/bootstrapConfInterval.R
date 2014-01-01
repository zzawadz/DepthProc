medianDepthConfInterval = function(X, method="Projection", R = 1000, ...)
{ 
  depthTMP = function(x, indices, ...) depthMedian(X=x[indices,],...)  
  boot(x,depthTMP, R = R, method = method,...)
}