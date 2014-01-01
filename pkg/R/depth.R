#'@title Depth calculation
#'
#'@description Computes the depth of a point or vectors of points with respect to a multivariate data set.
#'
#'  @param u Numerical vector or matrix whose depth is to be calculated. Dimension has to be the same as that of the observations.
#'  @param X The data as a matrix, data frame or list. If it is a matrix or data frame, then each row is viewed as one multivariate observation. If it is a list, all components must be numerical vectors of equal length (coordinates of observations).
#'  @param method Character string which determines the depth function. \code{method} can be "Projection" (the default), "Mahalanobis", "Euclidean" or "Tukey". For details see \code{\link{depth}.}
#'  @param ndir Number of random directions used when Projection and Tukey depth is approximated.
#'
#'
#'@details 
#'  
#'  Irrespective of dimension, Projection and Tukey's depth is obtained by approximate calculation. 
#'  
#'  Calculation of Mahalanobis and Euclidean depth is exact.
#'  
#'
#'  
#'  Returns the depth of multivariate point \code{u} with respect to data set \code{X}.
#'  
#'  @references 
#'  
#'  Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 783--858.
#'  
#'  Rousseuw, P.J. and Ruts, I. (1996), AS 307 : Bivariate location depth, \emph{Appl. Stat.-J. Roy. S. C},  \bold{45}, 516--526.
#'  
#'  Rousseeuw, P.J. and Struyf, A. (1998), Computing location depth and regression depth in higher dimensions, \emph{Stat. Comput.}, \bold{8}, 193--203.
#'  
#'  Zuo, Y. amd Serfling, R. (2000), General Notions of Statistical Depth Functions, \emph{Ann. Statist.},  \bold{28}, no. 2, 461--482.
#'  
#'  @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'  
#'  @seealso \code{\link{depthContour}} and \code{\link{depthPersp}} for depth graphics.
#'  
#'  @examples
#'  ## Calculation of Projection depth
#'  data(starsCYG, package = "robustbase")
#'  depth(t(colMeans(starsCYG)), starsCYG)
#'  
#'  #Aslo for matrices
#'  depth(starsCYG, starsCYG)
#'  
#'  ## Projection depth applied to a large bivariate data set
#'  set.seed(356)
#'  x <- matrix(rnorm(9999), nc = 3)
#'  depth(x, x)
#'  
#'  
#'  
#'  @keywords
#'  multivariate
#'  nonparametric
#'  robust
#'  depth function
#'  
#'


depth = function(u, X, method="Projection", ndir=1000, seed = 1, name = "X", a = 1, b = 1, p = 1)
{  
  if(is.data.frame(u)) u = as.matrix(u)
  if(is.data.frame(X)) X = as.matrix(X)
  if(is.vector(X)) X = matrix(X,ncol = 1)
  if(is.vector(u)) u = matrix(u,ncol = dim(X)[2])

  ###################################
  if (method=="Mahalanobis")
  {	
    return(depthMah(u, X, name = name))      
  }
  ####################################
  if (method=="Euclidean")
  {
  		return(depthEuclid(u, X, name = name))
  }
  ####################################
  if(method == "Projection")
  {
    return(depthProjection(u, X, ndir,name = name))
  }
  #######################################################################
  if (method=="Tukey")
  {
    return(depthTukey(u, X, ndir, name = name))
  }
  ########################################################
  if (method=="LP")
  {
    return(depthLP(u, X, ndir, name = name, a=a, b=b, p=p))
  }
}


#########################################################
depthEuclid = function(u, X, name)
{
  n = dim(u)[1]
  center = colMeans(X)
  center = matrix(rep(center,n),nrow=n,byrow=TRUE)
  depth=1/(1+(rowSums((u-center)^2)))  
  
  new("DepthEuclid", depth, u = u, X = X, method = "Euclidean", name = name)
}
#########################################################
depthMah = function(u, X, name)
{
  depth = depthMahCPP(u,X)
  new("DepthMahalanobis", depth, u = u, X = X, method = "Mahalanobis", name = name)
}
#########################################################
depthProjection = function(u, X, ndir, seed = 1, name)
{
  depth = depthProjCPP(u, X, ndir, seed)
  new("DepthProjection", depth, u = u, X = X, method = "Projection", name = name)
}
#########################################################
depthTukey = function(u, X, ndir, seed = 1, name)
{
  tukey1d = function(u,X)
  {
    Xecdf = ecdf(X)
    uecdf = Xecdf(u) 
    uecdf2 = 1-uecdf
    min.ecdf = uecdf>uecdf2
    depth = uecdf 
    depth[min.ecdf]=uecdf2[min.ecdf] 
    depth
  }
  
  if (ncol(X)==1)
  {
    depth= tukey1d(u,X)
  }
  
  #### 
  else  # czyli jesli wymiar d>2
  {
    proj = t(runifsphere(ndir, ncol(X)))
    xut = X%*%proj
    uut = u%*%proj
    
    OD<-matrix(nrow=nrow(uut),ncol=ncol(uut))
    
    for (i in 1:ndir)
    {
      
      OD[,i]=tukey1d(uut[,i],xut[,i])  
    }
    
    depth<-apply(OD,1,min)
  }
  new("DepthTukey", depth, u = u, X = X, method = "Tukey", name = name)
}

######################################################################

depthLP = function(u, X, p, a, b, name, func = NULL)
{
  depth = depthLPCPP(u, X, p, a, b)
  new("DepthLP", depth, u = u, X = X, method = "LP", name = name)
}

