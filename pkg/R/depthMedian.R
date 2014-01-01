depthMedian = function(X, method="Projection",...)
{
  depths = depth(X, X, method = method,...)
  med = X[depths == max(depths),]
  if(ncol(X) != length(med)) med = colMeans(med)
  med
}


