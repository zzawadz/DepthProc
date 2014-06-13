depthMedian = function(x,...)
{
  depths = depth(x,x,...)
  med = x[depths == max(depths),]
  if(ncol(x) != length(med)) med = colMeans(med)
  med
}


