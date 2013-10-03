asymmetrycurve<-function(X,Y = NULL, alpha = seq(0,1,0.01), method = "Projection",
	movingmedian = FALSE,draw = TRUE,title = "Asymmetry curve",nameX = "X", nameY = "Y",...)
{
	if(!is.matrix(X)) stop("X must be a matrix!")
	if(!is.null(Y)) if(!is.matrix(Y)) stop("Y must be a matrix!")
	
  if(!movingmedian) x_est = as_curve(X,alpha,method,...) #function in as_curve.R
	else x_est = as_curve_mm(X,alpha,method,...)  #function in as_curve_mm.R
	
  
  
	if(!is.null(Y)) 
	{
		
		if(!movingmedian) y_est = as_curve(Y,alpha,method)
		else y_est = as_curve_mm(Y,alpha,method)
		xy_est = data.frame(rbind(x_est,y_est),c(rep(nameX,nrow(x_est)),rep(nameY,nrow(y_est))))
		
	}
	else
	{
		xy_est = data.frame(x_est,rep(nameX,nrow(x_est)))
	}

  
	names(xy_est) <- c("alpha","norm","Set")
	
	
			if(draw)
			{
			p = ggplot()
			p = p + geom_line(data = xy_est, aes(alpha,norm,color = Set), size = 1.2)
			p = p + scale_color_manual(values=c("#E41A1C", "#377EB8"))
			p = p + theme_bw()
			p = p + ggtitle(title) 
			p = p + theme(title = element_text(face = "bold",vjust=1, size = 18))
			p = p + xlab("Proportion p")
			p = p + ylab(expression(As[n](p)))
			p = p + ylim(c(0,max(xy_est$norm)))
			p = p + theme(axis.title.x  = element_text(face = "bold",vjust=0, size = 16))
			p = p + theme(axis.title.y  = element_text(face = "bold", angle = 90,vjust=0.4,size = 16))
			p = p + theme(axis.text.x  = element_text(size=14))
			p = p + theme(axis.text.y  = element_text(size=14))
			p
		}
		else xy_est
	
	
	
}
