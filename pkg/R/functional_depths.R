#' @export
fncDepth = function(u, X, method = "MBD", name = "X", threads = -1,...)
{
  if(missing(X) && method == "MBD") return(fncDepthMBD(u,...))
  if(missing(X)) X = u
  
  if(method == "FM") return(fncDepthFM(u, X, ...))
  if(method == "MBD") return(fncDepthMBD(u, X,...))
}


fncDepthFM = function(u, X, ...)
{
  if(missing(X)) X = u
  
  depths = rep(0, nrow(X))
  for(i in 1:ncol(X))
  {
    depths = depths + depth(u[,i], X[,i], ...)
  }
  
  depths = depths/ncol(X)
  return(depths)  
}


#'@title Modified band depth
#'@export
#'@description Computes the modified band depth.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#' @param name for this data set - it will be used on plots from depthproc.
#' @param \dots currently not supported.
#'
#'@examples
#'
#'  x = matrix(rnorm(600), nc = 20)
#'  fncDepthMBD(x)
#'  
fncDepthMBD = function(u, X, name = "X",...)
{
  if(missing(X)) 
  {
    X = u
    depth = modBandDepth(u)
  } else
  {
    depth = modBandDepthRef(u,X)  
  }
  
  new("DepthMBD", as.numeric(depth), u = u, X = X, method = "MBD", name = name)
}
