#' @title CovLP
#'
#' @export
#'
setClass("CovDepthWeighted", representation(depth = "character"), contains="CovRobust")

#'@title CovLp
#'
#'@description Claculate covariance matrix
#'
#'  @param x x
#'  @param p p
#'  @param a a
#'  @param b b
#'
#'@details 
#'  
#'  Under construction.
#'  
#'@return
#'
#'  Returns depth weighted covariance matrix.
#'
#'    
#'  
#'  @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'  
#'  @export
#'  @seealso \code{\link{depthContour}} and \code{\link{depthPersp}} for depth graphics.
#'  
#'  @examples
#'  require(MASS)
#'  x = mvrnorm(n = 100, mu = c(0,0), Sigma = 3*diag(2))
#'  cov_x = CovLP(x,1,1,1)
#'  
#'  
#'  @keywords
#'  multivariate
#'  nonparametric
#'  robust
#'  depth function
#'  
CovLP = function(x, p=1, a=1, b=1)
{
  cov = CovLPCPP(x, p, a, b)
  
  method = "Depth Weighted Estimator"
  new("CovDepthWeighted", cov = cov,
      center = colMeans(x),
      det = det(cov),
      n.obs = nrow(x),
      X = x,
      method = method)
}


