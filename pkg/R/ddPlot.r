#' @name ddPlot
#' @title Depth versus depth plot
#' @export
#' @description
#' Produces a DD plot which allows to compare two multivirate datasets or to compare a subject dataset with theoretical distribution.
#'
#' @param x The first or only data sample for ddPlot.
#' @param y The second data sample. \code{x} and \code{y} must be of the same space.
#' @param scale
#' @param loaction
#' @param plot
#' @param name_x
#' @param name_y
#' @param ... Parameters passed to depth function
#'
#' @details
#'  
#'  When \code{distribution} is "mvnorm" the sample dataset is compared with multivirate normal distribution with given parameters. \code{mean} and \code{varcov} must be specified.
#'  
#'  When \code{distribution} is "t" the sample dataset is compared with multivirate t-Student distribution with given parameters. \code{mean} and \code{S} must be specified. \code{df=Inf} by default.
#'  
#'  When \code{distribution} is "smvnorm" the sample dataset is compared with multivirate skew normal distribution with given parameters. \code{xi}, \code{Omega} and \code{alpha} must be specified.
#'  
#'  When \code{distribution} is "st" the sample dataset is compared with multivirate skew t-Student distribution with given parameters. \code{xi}, \code{Omega} and \code{alpha} must be specified. \code{df=Inf} by default.
#'  
#'
#' @references
#' Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 822--831.
#'           
#'            Liu, R.Y., Singh K. (1993), A Quality Index Based on Data Depth and Multivariate Rank Test, \emph{Journal of the American Statistical Association} vol. 88.
#'  
#'  @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#' 
#' @examples
#' require(MASS)
#' require(sn)
#' require(mvtnorm)
#' 
#'  ## Location difference
#' standard = mvrnorm(1000, c(0,0), diag(2))
#' shift    =  mvrnorm(1000, c(0.5, 0), diag(2))
#' ddPlot(x = standard, y = shift, title = "Difference in position")
#' ddPlot(x = standard, y = shift, location = TRUE, title = "Location aligned")
#' 
#' ## Scale difference
#' standard <- mvrnorm(1000, c(0,0), diag(2))
#' scale <- mvrnorm(1000, c(0,0), 4*diag(2))
#' ddPlot(x=standard, y=scale)
#' ddPlot(x=standard, y=scale, scale=TRUE)
#' 
#' 
#' ## Skewness difference
#' require(MASS)
#' require(sn)
#' standard <- mvrnorm(1000, c(0,0), diag(2))
#' skew <- rmsn(1000, xi=c(0,0), Omega= diag(2), alpha=c(6,1))
#' ddPlot(x=standard, y=skew)    
#'   
#' ## Kurtosis difference
#' require(MASS)
#' require(mnormt)
#' standard <- mvrnorm(1000, c(0,0), diag(2))
#' kurt <-rmt(1000, mean=c(0,0), S=diag(2), df=1)
#' ddPlot(x=standard, y=kurt)

ddPlot <- function (x, y, scale = FALSE, location = FALSE, plot = TRUE, name_x = "X", name_y = "Y", title = "Depth vs. depth plot", ...) 
{
#   if (is.null(y)) {
#     size = nrow(x)
#     d    = ncol(x) 
#     
#     if (distribution == "mvnorm") {
#       theoretical <- mvrnorm(n = size, ...)
#     }
#     else if (distribution == "t") {
#       theoretical <- rmt(n = size, ...)
#     }
#     else if (distribution == "smvnorm") {
#       theoretical <- rmsn(n = size, ...)
#     }
#     else if (distribution == "st") {
#       theoretical <- rmst(n = size, ...)
#     }
#     depth_sample <- depth(x, x, method, name = name_x, ...)
#     depth_theoretical <- depth(x, theoretical, method, name = name_y, ...)
#     ddplot = new("DDPlot", X = depth_sample, Y = depth_theoretical)
#   }
#   else {
    if (ncol(x) != ncol(y)) {
      print("Wrong dimensions of the datasets! ncol(x)!=ncol(y)")
    }
    else {
      if (scale == TRUE) 
      {
        depth_sample_x <- depth(x, x, name = name_x, ...)
        depth_sample_y <- depth(y, y, name = name_y, ...)
        varcovx <- cov(x[which(depth_sample_x >= median(depth_sample_x)), ])
        varcovy <- cov(y[which(depth_sample_y >= median(depth_sample_y)), ])
        x_new <- t(solve(chol(varcovx)) %*% t(x))
        y_new <- t(solve(chol(varcovy)) %*% t(y))
      }
      else 
      {
        x_new <- x
        y_new <- y
      }
    }
    if (location == TRUE) 
    {
        medx <- depthMedian(x_new,  ...)
        medy <- depthMedian(y_new,  ...)
        x_new <- sweep(x_new, 2, medx, "-")
         y_new <- sweep(y_new, 2, medy, "-")
      
    }
    data <- rbind(x_new, y_new)
    depth_x <- depth(data, x_new,  name = name_x, ...)
    depth_y <- depth(data, y_new,  name = name_y, ...)
    
    ddplot = new("DDPlot", X = depth_x, Y = depth_y, title = title)
#  }
  
  if(plot) plot(ddplot)
  return(ddplot)
}
