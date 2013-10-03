depthContour<-function(X,method = "Projection",plot.title = paste(method,"depth"),
	xlim = extendrange(X[,1],f=0.1),ylim = extendrange(X[,2],f=0.1),n=50,pmean = TRUE,mcol = "blue",
	pdmedian = TRUE, mecol = "brown",legend = TRUE,points = FALSE,...)
{
			x_axis = seq(xlim[1],xlim[2],length.out = n)
 			y_axis = seq(ylim[1],ylim[2],length.out = n)
				
			xy_surface = expand.grid(x_axis,y_axis)
			
			xy_surface=cbind(xy_surface[,1],xy_surface[,2])
			depth_surface = depth(xy_surface,X,method = method,...)

			
depth_surface = matrix(depth_surface,ncol = n)

colors = rev(rainbow(100,start=0,end=1/4))


if(max(depth_surface)>0.5)
	{
	levels=seq(0,1,0.1)
	}
else
	{
	levels = seq(0,0.5,0.05)
	}


filled.contour(x_axis, y_axis, depth_surface, 
  color.palette = colorRampPalette(colors,space = "Lab"),levels = levels,
	main = plot.title,
									

	plot.axes = { contour(x_axis, y_axis, depth_surface, add = TRUE,drawlabels=FALSE,lwd=1.3);

				
				
				if(points)
 					{
						points(X[,1],X[,2],col = "white",pch = 19,cex = 1.2);
						points(X[,1],X[,2],cex = 1.3,col = "grey");
					}
				if(pmean)
					{
						points(mean(X[,1]),mean(X[,2]),pch = 4 ,col = mcol,cex = 1.5,lwd = 2.5)
						#abline(, col = mcol,lwd = 1.8)
					}
				if(pdmedian)
					{
						dmedian = med(X,method = method)
						points(dmedian[1],dmedian[2] ,pch = 4,col = mecol, cex = 1.5,lwd = 2.5)
						#abline(h = dmedian[2], col = mecol, lwd = 1.8)
					}
			
				if(legend)
					{
						if(pmean & pdmedian)
						legend("topright", c("Mean","Depth Median"),pch = 4,
 								bg = "white",col = c(mcol,mecol),pt.cex = 1.5,pt.lwd = 2.5)
						
						if(pmean & (!pdmedian))
						legend("topright", c("Mean"),pch = 4 ,pt.cex = 1.5,pt.lwd = 2.5,
 								bg = "white",col = c(mcol))
						
						if(pdmedian & (!pmean))
						legend("topright", c("Depth Median"), pt.cex = 1.5,pt.lwd = 2.5,
 								bg = "white",col = c(mecol))

									}
				
					axis(1);
					axis(2);

                     }	
	
	)

}