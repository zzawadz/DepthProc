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
