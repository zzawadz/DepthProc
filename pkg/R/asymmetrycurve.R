asymmetryCurve<-function(x, y = NULL, alpha = seq(0,1,0.01), method = "Projection",
	movingmedian = FALSE,plot = TRUE, name = "X", name_y = "Y",...)
{
	if(!is.matrix(x)) stop("X must be a matrix!")
	if(!is.null(y)) if(!is.matrix(y)) stop("Y must be a matrix!")
	
	depth_est <- depth(x,x,method=method, name=name) 
  
  if(!movingmedian) x_est = .asCurve(x, depth_est, alpha, method, ...) #function in as_curve.R
	else x_est = .asCurveMM(x,alpha,method,...)  #function in as_curve_mm.R
	
  print(x_est)
	asc = new("AsymmetryCurve", x_est, depth = depth_est, alpha = alpha)
	return(asc)
}



