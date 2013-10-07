ddmvnorm <-function(x, method = "Projection", size = nrow(x), robust=FALSE, alpha=0.05, plot = TRUE, ...)     
{
  depth_sample <- depth(x, x, method, ...)  
  
  if(robust == TRUE) 
  {
  	varcov <- cov(x[depth_sample>=quantile(depth_sample, alpha),])
  	location <- med(x, method=method, ...)
  } 
  else
  { 
  	location <- apply(x, 2, mean)
  	varcov  <- cov(x) 
  }
  theoretical <- mvrnorm(size, location, varcov)  
  depth_theoretical <- depth(x, theoretical, method, ...)
  ddplot = new("DDPlot",X = depth_sample, Y = depth_theoretical)
  
  if(plot) plot(ddplot)
  return(ddplot)
}