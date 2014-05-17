#'@title 2d Binning
#'@importFrom sm binning
#'@description Binning 2d
#'
#'  @param x bivariate matrix
#'  @param binmethod method for calculation center and dispersion measures. "LocDepth" uses location-scale depth, MAD uses median and MAD in each dimension
#'  @param nbins number of bins in each dimension
#'  @param k scale parameter for dispersion measure
#'  @param remove_borders logical. Include or not marginal bins
#'  @param devel this is only for devel purposes and soon will be removed
#'  @param ... other arguments passed to depthMedian
#'  
#'  
#'  @seealso \code{\link{depth}}
#'  @export
#'  
#'  @examples
#'  
#'  require(MASS)
#'  binningDepth2D(x = mvrnorm(100,rep(0,2),diag(2)))
#'  
#'  @keywords
#'  multivariate
#'  nonparametric
#'  robust
#'  depth function
binningDepth2D = function(x, binmethod = "LocDepth", nbins = 8, k = 1, remove_borders = FALSE, devel = FALSE, ...)
{
  createBin = function(x, nbins, mean = NULL)
  {
    if(binmethod == "LocDepth")
    {
      if(!devel) dep_stat = sample.max.depth(as.numeric(x)) 
      else 
      {
        dep_stat = maxSampleLocScaleDepth(x)
      }
      mean = dep_stat["mu"]
      sigma = k*dep_stat["sigma"]
    }
    if(binmethod == "LP")
    {
      sigma  = mad(x)
      dep_stat = c(mu = mean, sigma = sigma)
    }
    
    range = range(x)
    
    if(nbins == "auto")
    {
      n_upper = 1:ceiling(abs(range[2]-mean)/sigma)*sigma+mean
      n_lower = -(ceiling(abs(range[1]-mean)/sigma):1)*sigma+mean
      breaks = c(n_lower,mean, n_upper)
    } else
    {
      #if(!remove_borders) 
      nbins = nbins-2
      
      if(nbins>0)
      {
        s_bound = 1:(nbins/2)*sigma
      }
      0:nbins*sigma
      breaks = sort(c(-Inf,-s_bound+mean,mean,s_bound+mean,Inf))
    }
    
    cut = cut(x, breaks=breaks)
    midpoints = sapply(2:length(breaks), function(x) mean(breaks[(x-1):x]))
    
    if(nbins != "auto")
    {
      midpoints[1] =  midpoints[2]-2*sigma #mean(x[x<breaks[2]])
      midpoints[length(midpoints)] =  midpoints[(length(midpoints)-1)]+2*sigma #mean(x[x>breaks[length(breaks)-1]])
      if(is.na(midpoints[1])) midpoints[1] = midpoints[2]-2*sigma
      if(is.na(midpoints[length(midpoints)])) midpoints[(length(midpoints))] = midpoints[(length(midpoints)-1)]+2*sigma
    }
    
    res = list(breaks, midpoints, dep_stat)
    return(res)
  }
  
  means = c(0,0)
  if(binmethod == "LP") means = depthMedian(x,method = "LP",...)
  
  tmp1 = createBin(x[,1], nbins, means[1])
  tmp2 = createBin(x[,2], nbins, means[2])
  
  
  b = cbind(tmp1[[1]],tmp2[[1]])
  b[b == Inf] = 1e6
  b[b == -Inf] = -1e6
  tmp = binning(x=x,breaks=b)$table.freq
  
  if(remove_borders == TRUE)
  {
    tmp = tmp[-c(1,nrow(tmp)),-c(1,ncol(tmp))]
    tmp1[[1]] = tmp1[[1]][-c(1,length(tmp1[[1]]))]
    tmp2[[1]] = tmp2[[1]][-c(1,length(tmp2[[1]]))]
    
    tmp1[[2]] = tmp1[[2]][-c(1,length(tmp1[[2]]))]
    tmp2[[2]] = tmp2[[2]][-c(1,length(tmp2[[2]]))]
  }
  

 tmp = matrix(as.vector(tmp), ncol = ncol(tmp))
  new("BinnDepth2d", freq = tmp, mid_x =  tmp1[[2]], mid_y = tmp2[[2]], breaks_x = tmp1[[1]], breaks_y = tmp2[[1]], input_data = x,
      max_depth_x = tmp1[[3]], max_depth_y = tmp2[[3]])
  #return(result)
}

#'@docType methods
#'@title 2d Binning plot
#'
#'@description Binning 2d
#'@export
#'  @seealso \code{\link{depth}}
#'  
#'  @examples
#'  
#'  require(MASS)
#'  tmp = binningDepth2D(x = mvrnorm(100,rep(0,2),diag(2)))
#'  plot(tmp)
#'  @keywords
#'  multivariate
#'  nonparametric
#'  robust
#'  depth function
setMethod("plot", signature = c(x = "BinnDepth2d", y = "missing"), function(x, y = "missing",..., alpha = 0.1, bg_col = "red", add_mid = TRUE){
  
  breaks_y = x@breaks_y
  breaks_x = x@breaks_x
  breaks_x[is.infinite(breaks_x)] = extendrange(x@input_data[,1], f=10)
  breaks_y[is.infinite(breaks_y)] = extendrange(x@input_data[,2], f=10)
  
  
  xlim = extendrange(x@input_data[,1],f=0.1)
  ylim = extendrange(x@input_data[,2],f=0.1)
  plot(x@input_data, xlim = xlim, ylim = ylim, xlab = "", ylab = "")
  rect(xleft=min(breaks_x),xright=max(breaks_x), ybottom=min(breaks_y), ytop = max(breaks_y), col = rgb(1, 0, 0, alpha))
  
  segments(x0=min(breaks_x), x1 = max(breaks_x), y0 = breaks_y, y1= breaks_y, lty = 2)
  segments(x0=breaks_x, x1 = breaks_x, y0 = min(breaks_y), y1= max(breaks_y), lty = 2)
  
  tmp = expand.grid(x@mid_x,x@mid_y)
  if(add_mid) points(tmp[,1], tmp[,2], col = "red", pch = 17)
  #abline(v = breaks_x, lty = 2)
  #abline(h = breaks_y, lty = 2)
})

