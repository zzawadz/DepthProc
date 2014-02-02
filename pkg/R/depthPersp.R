depthPersp<-function(X,method = "Projection",plot_method = "lattice",xlim = extendrange(X[,1],f=0.1),ylim = extendrange(X[,2],f=0.1),n=50,
	xlab = "x", ylab = "y",plot.title=paste(method,"depth"),...)
{
	if(dim(X)[2]==2)
	{
	
			axis_x = seq(xlim[1],xlim[2],length.out = n)
 			axis_y = seq(ylim[1],ylim[2],length.out = n)
				
			xy_surface = expand.grid(axis_x,axis_y)

		
			xy_surface=matrix(unlist(xy_surface),ncol=2)
  
			z_surface = depth(xy_surface,X,method = method,...)

			ztmp = z_surface * 100/max(z_surface)  
    
			colors = rev(rainbow(100,start=0,end=1/4))
			col = colors[ztmp]
			
			if(plot_method == "rgl") 
        {
          require(rgl)
          persp3d(axis_x,axis_y,z_surface,color=col,back = "lines",
				xlab = xlab, ylab = ylab,zlab=plot.title)
			  }
			
			if(plot_method == "lattice") wireframe(z_surface~xy_surface[,1]+xy_surface[,2],colorkey = TRUE,drape = TRUE,
					xlab = xlab, ylab = ylab,zlab="",col.regions = colors,lwd = 0.4,main  = plot.title,scales = list(arrows = FALSE),...)
			else {print=c("Wrong plot.method")}
		}
		else print("Wrong matrix!")
}
