#' @title Basic function for functional depths
#' 
#' @description Calculates depth functions.
#' @export
#' 
#' @param u data 
#' @param X reference set. If null u will be used as reference.
#' @param method depth method - "MBD" (default), or "FM" (Frainman-Muniz depth)
#' @param name name for data set
#' @param \dots additional arguments passed to fncDepthFM.
#' 
#' @rdname fncDepth
#' @examples
#' 
#' x = matrix(rnorm(60), nc = 20)
#' fncDepth(x, method = "FM", dep1d = "Mahalanobis")
#' fncDepth(x)
#' 
#' # zoo and xts
#' library(xts)
#' data(sample_matrix)
#' sample.xts <- as.xts(sample_matrix, descr='my new xts object')
#' fncDepth(sample.xts) 
#' 
fncDepth = function(u, X = NULL, method = "MBD", name = deparse(substitute(u)), ...)
{
  if(!is.null(X))
  {
    if(class(u) != class(X)) stop("u and X must be the the same class!")
  }
  UseMethod("fncDepth")
}
#' @export
#' @rdname fncDepth
fncDepth.matrix = function(u, X = NULL, method = "MBD", name = deparse(substitute(u)), ...)
{
  
  fast_mbd = FALSE
  if(is.null(X) && method == "MBD") fast_mbd = TRUE
  if(is.null(X)) X = u
  
  if(method == "FM")  
  {
    dept = (fncDepthFM(u, X, ...))
    depth = new("FncDepthFM", dept)
  }
  if(method == "MBD") 
  {
    if(fast_mbd) 
    {
      dept = fncDepthMBD(u) 
    } else
    {
      dept = (fncDepthMBD(u, X))
    }
    depth = new("FncDepthMBD", dept)
  }
  
  depth@u = u
  depth@X = X
  depth@name = name
  depth@method = method
  depth@index = 1:ncol(u)
  return(depth)
}

#' @export
#' @rdname fncDepth
fncDepth.zoo = function(u, X = NULL, method = "MBD", name = deparse(substitute(u)), ...)
{
  if(is.null(X)) X = u
  
  um = t(as.matrix(u))
  Xm = t(as.matrix(X))
  
  depth = fncDepth(um,Xm, method, name, ...)
  depth@name = name
  depth@index = index(u)
  
  depth
}

###################################################################
###################################################################


#' @title FM Depth
#' @export
#' @description Computes Frainman-Muniz depth for functional data.
#' 
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix. If it is a matrix or data frame, then each row is viewed as one multivariate observation.
#' @param dep1d method for depth used in 1d.
#' @param \dots other arguments passed to depth function.
#' 
#' @examples
#' x = matrix(rnorm(60), nc = 20)
#' fncDepthFM(x)
#' 
fncDepthFM = function(u, X, dep1d = "Projection", ...)
{
  if(missing(X)) X = u
  
  depths = rep(0, nrow(X))
  for(i in 1:ncol(X))
  {
    depths = depths + depth(u[,i], X[,i], method = dep1d, ...)
  }
  
  depths = as.numeric(depths/ncol(X))
  return(depths)  
}


#'@title Modified band depth
#'@export
#'@description Computes the modified band depth.
#'
#' @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#' @param X The data as a matrix. If it is a matrix or data frame, then each row is viewed as one multivariate observation.
#'
#'@examples
#'
#'  x = matrix(rnorm(60), nc = 20)
#'  fncDepthMBD(x)
#'  fncDepthMBD(x, x)
#'  
fncDepthMBD = function(u, X)
{
  if(missing(X)) 
  {
    X = u
    depth = modBandDepth(u)
  } else
  {
    depth = modBandDepthRef(u,X)  
  }
  
  as.numeric(depth)
}
