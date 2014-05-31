

.depthLocal = function(u, X, beta, depth1, depth2, ...)
{
  ncol = ncol(X)
  nrow = nrow(X)
  
  ### Fix for dim 1
  tmp = t(apply(X, 1, function(k) 2*u-k))
  if(ncol(tmp) != ncol(X)) tmp = t(tmp)
  
  symDATA = rbind(X, tmp)
  depths = as.numeric(depth(X, symDATA, method=depth1))
  quan = quantile(depths, probs=1-beta)
  Rset = as.matrix(X[ signif(depths,digits=6) >= signif(quan,digits=6), ])
  as.numeric(depth(u, Rset, method=depth2,...))
}


#'@title Local depth
#'@export
#'
#'  @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#'  @param X The data as a matrix, data frame. If it is a matrix or data frame, then each row is viewed as one multivariate observation.
#'  @param beta cutoff value for neighbourhood
#'  @param depth1 depth method for symmetrised data
#'  @param depth2 depth method for calculation depth of given point
#'  @param ... additional parameters passed to depth1 and depth2
#'  
#'  @examples
#'  
#'  require(MASS)
#'  data = mvrnorm(100, c(0,5), diag(2)*5)
#'  #by default depth2 = depth1
#'  depthLocal(data, data, depth1 = "LP")
#'  depthLocal(data, data, depth1 = "LP", depth2 = "Projection")
#'  ## Depthcontour
#'  depthContour(data, method = "Local", depth1 = "LP")
#'  
depthLocal = function(u, X, beta=0.5,
                      depth1="Projection", depth2=depth1,name = "X", ...) {
  depths = 1:nrow(u)
  for(i in 1:nrow(u)) depths[i] = .depthLocal(u[i,,drop = FALSE],X,beta,depth1,depth2)
  return(depths)
}
 

