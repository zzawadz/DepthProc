#' @name trimProjReg2d
#' @title trimProjReg2d
#' @description Computes projection trimmed regression in 2 dimensions.
#'
#' @param x Independent variable
#' @param y Dependent variable
#' @param alpha Percentage of trimmed observations
#'
#'  @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'
#'
#' @examples
#' 
#'  data(pension)
#'  plot(pension)
#'  abline(lm(Reserves~Income,data = pension), lty = 3, lwd = 2) #lm
#'  abline(trimProjReg2d(pension[,1],pension[,2]), lwd = 2) #trimprojreg2d
#'  legend("bottomright", c("OLS","TrimLS"), lty = 1:2)



trimProjReg2d<-function(x, y, alpha = 0.1)
{

	yX= cbind(y,x)
	depth = depth(yX,yX,method = "Projection")
	cut = quantile(depth,alpha)

	ycut = y[ depth > cut ]	
	xcut = x[ depth > cut ]
	
	fitcut = lm(ycut~xcut)$coeff 
  new("TrimReg2d",coef = fitcut)
  
}
