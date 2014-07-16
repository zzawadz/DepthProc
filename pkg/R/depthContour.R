#' @title Approximate depth contours
#' @export
#'  @description Draws an approximate contours of depth for bivariate data.
#'  
#' @param x Bivariate data
#' @param method Character string which determines the depth function. \code{method} can be "Projection" (the default), "Mahalanobis", "Euclidean" or "Tukey". For details see \code{\link{depth}.}
#' @param plot_title Title of the plot
#' @param xlim Determines the width of x-axis.
#' @param ylim Determines the width of y-axis.
#' @param n Number of points in each coordinate direction to be used in contour plot.
#' @param pmean Logical. If TRUE mean will be marked.
#' @param mcol Determines the color of lines describing the mean.
#' @param pdmedian Logical. If TRUE depth median will be marked.
#' @param mecol Determines the color of lines describing the depth median. 
#' @param legend Logical. If TRUE legend for mean and depth median will be drawn.
#' @param points Logical. If TRUE points from matrix x will be drawn.
#' @param \dots Any additional parameters for function depth
#'  
#'  
#'  @details
#'  
#'  The set of all points that have depth at least  \eqn{ \alpha  }  is called { \eqn{ \alpha - }  trimmed region}. The  \eqn{ \alpha - }  trimmed region w.r.t.  \eqn{ F }  is denoted by  \eqn{ {D}_{\alpha }(F) }  , i.e.,  
#'  \deqn{ {D}_{\alpha }(F)=\left\{ z\in {{{R}}^{d}}:D(z,F)\ge \alpha  \right\}}.
#'  
#'  @author Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
#'  
#'  @seealso \code{\link{depthPersp}}
#'  
#'  @examples
#' # EXAMPLE 1
#' require(MASS)
#' x = mvrnorm(1000,c(0,0),diag(2))
#' depthContour(x)
#' #with points
#' depthContour(x, points = TRUE)
#'  
#'  #EXAMPLE 2
#'  data(inf.mort,maesles.imm)
#'  data1990=na.omit(cbind(inf.mort[,1],maesles.imm[,1]))
#'  depthContour(data1990, n = 50, pmean = TRUE, mcol = "blue", pdmedian = TRUE, mecol = "brown", legend = TRUE, points = TRUE,xlab="infant mortality rate per 1000 live birth", ylab="against masles immunized percentage", main='L2 depth, UN Fourth Goal 2011 year',method = "LP")
#'  
#'  
#'  @keywords
#'  contour
#'  depth
depthContour<-function(x, xlim = extendrange(x[,1],f=0.1), ylim = extendrange(x[,2],f=0.1),n=50,pmean = TRUE,mcol = "blue", pdmedian = TRUE, mecol = "brown",legend = TRUE,points = FALSE, ...)
{ 
			x_axis = seq(xlim[1],xlim[2],length.out = n)
 			y_axis = seq(ylim[1],ylim[2],length.out = n)
				
			xy_surface = expand.grid(x_axis,y_axis)
			
			xy_surface=cbind(xy_surface[,1],xy_surface[,2])
      
      
			depth_params = .extractDepthParams(xy_surface,x,...)
			depth_surface = do.call(depth,depth_params)
      method = depth_params$method
			#depth_surface = depth(xy_surface, x,method = method,...)

			
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

graph_params = .removeDepthParams(...)

do.call(filled.contour, c(list(x = x_axis, y = y_axis, z = depth_surface, 
  color.palette = colorRampPalette(colors,space = "Lab"),levels = levels,									
	plot.axes = quote({ 
    
        do.call(contour,c(list(x = x_axis,y = y_axis,z= depth_surface, add = TRUE,drawlabels=FALSE), graph_params))
        #contour(x_axis, y_axis, depth_surface, add = TRUE,drawlabels=FALSE);
				
				if(points)
 					{
						points(x[,1],x[,2],col = "white",pch = 19,cex = 1.2);
						points(x[,1],x[,2],cex = 1.3,col = "grey");
					}
				if(pmean)
					{
						points(mean(x[,1]),mean(x[,2]),pch = 4 ,col = mcol,cex = 1.5,lwd = 2.5)
						#abline(, col = mcol,lwd = 1.8)
					}
				if(pdmedian)
					{
            depth_params$u = x
            depths = do.call(depth,depth_params)
            med = x[depths == max(depths),]
            if(ncol(x) != length(med)) med = colMeans(med)
						dmedian = med
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

                     })), graph_params
  ))

}
