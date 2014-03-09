medianDepthConfInterval = function(X, method="Projection", R = 1000, ...)
{ 
  depthTMP = function(X, indices, ...) depthMedian(X=X[indices,],...)  
  boot(X,depthTMP, R = R, method = method,...)
}
