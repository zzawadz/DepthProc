#'@title Simple deepest regression method.
#'
#'  @description This function calculate estimator of simple regression model y=ax+b
#'
#'  @param x Independent variable.
#'  @param y Dependent variable.
#'
#'  @details 
#'  Function returns object of class DeepReg2d.
#'
#'  @references
#'  Rousseeuw J.P., Hubert M. (1998), Regression Depth, \emph{Journal of The American Statistical Association},  vol.94.
#'  
#'  @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'  
#'  @examples
#'  
#' data(pension)
#' plot(pension)
#' abline(lm(Reserves~Income,data = pension), lty = 3, lwd = 2) #lm
#' abline(deepReg2d(pension[,1],pension[,2]), lwd = 2) #deepreg2d


deepReg2d<-function(x,y)
{
  y <- y[order(x)]
  x <- sort(x)
  tmp = depth2dcpp(x,y)
  new("DeepReg2d", coef = tmp[2:1], depth = tmp[3])
}



