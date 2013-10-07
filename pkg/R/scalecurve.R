
scaleCurve<-function(x,y=NULL,alpha = seq(0,1,0.01),method = "Projection",plot = TRUE,
	name = "X", name_y = "Y",...)
{
  if(!is.matrix(x)) stop("x must be a matrix!")
  if(!is.null(y)) if(!is.matrix(y)) stop("y must be a matrix!")
  
  dim_x <- dim(x)[2] 

	depth_est <- depth(x,x,method, name = name) 
 
	k = length(alpha)
	vol = 1:k 

	
	for(i in 1:k)
	{
		tmp_x <- x[depth_est >= alpha[i],]
		np <- nrow(as.matrix(tmp_x))

		if (np > dim_x)
		{ 
			vol[i] <- convhulln(tmp_x,options = "FA")$vol
 		}
		else
		{
			vol[i]=0 
		}
	}
	

	scale_curve = new("ScaleCurve",rev(vol), alpha = alpha, depth = depth_est)


  if(plot) plot(scale_curve)
  return(scale_curve)
	
}