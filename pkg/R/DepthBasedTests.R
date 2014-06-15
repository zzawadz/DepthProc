
################## Multivariate Wilcoxon Test #########################
## TODO:
## - new object (S3 for compability with other tests)
## - more examples

#' @title Depth based multivariate Wilcoxon test for scale difference
#' @export
#' 
#' @examples
#' require(MASS)
#' x = mvrnorm(100, c(0,0), diag(2))
#' y = mvrnorm(100, c(0,0), diag(2)*1.4)
#' mWilcoxonTest(x,y)
mWilcoxonTest = function(x, y, alternative = "greater")
{
  total = rbind(x, y)
  dep_x  = depth(x,total)
  dep_y  = depth(y,total)
  wilcox.test(dep_x,dep_y, alternative = alternative)
}


################

