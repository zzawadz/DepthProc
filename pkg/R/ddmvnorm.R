#' @name ddmvnorm
#' @title Normal depth versus depth plot
#' @export
#' 
#' @param x The data sample for DD plot.
#' @param size size of theoretical set
#' @param robust Logical. Dafault \code{FALSE}. If \code{TRUE}, robust measures are used to specify the parameters of theoretical distribution.
#' @param alpha cutoff point for robust measure of covariance
#' @param ... Parameters passed to \code{depth}
#' 
#' @description
#' Produces a  normal DD plot of a multivirate dataset.
#'
#' @details
#' In the first step the location and scale of \code{x} are estimated and theoretical sample of normal distribution with those parameters is generated. The plot presents the depth o empirical points with reference to \code{x} and theoretical sample.  
#' @return
#' Returns the normal depth versus depth plot of multivariate dataset \code{x}. 
#'
#'
#' @references
#' Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 822--831.
#' Liu, R.Y., Singh K. (1993), A Quality Index Based on Data Depth and Multivariate Rank Test, \emph{Journal of the American Statistical Association} vol. 88.
#'
#'  @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#' @seealso \code{\link{ddPlot}} to generate ddPlot to compare to datasets or to compare a dataset with other distributions.
#'
#' @examples
#'
#' norm <- mvrnorm(1000, c(0,0,0), diag(3))
#' con <- mvrnorm(100, c(1,2,5), 3*diag(3))
#' sample <- rbind(norm, con)
#' ddMvnorm(sample, robust=TRUE)
ddMvnorm <-function(x, size = nrow(x), robust=FALSE, alpha=0.05, plot = TRUE,title = "ddMvnorm", ...)     
{
  depth_sample <- depth(x, x,  ...)  
  
  if(robust == TRUE) 
  {
  	varcov <- cov(x[depth_sample>=quantile(depth_sample, alpha),])
  	location <- depthMedian(x, ...)
  } 
  else
  { 
  	location <- apply(x, 2, mean)
  	varcov  <- cov(x) 
  }
  theoretical <- mvrnorm(size, location, varcov)  
  depth_theoretical <- depth(x, theoretical, ...)
  ddplot = new("DDPlot",X = depth_sample, Y = depth_theoretical, title = title)
  
  if(plot) plot(ddplot)
  return(ddplot)
}
