
scalecurve<-function(x,y=NULL,alpha = seq(0,1,0.01),method = "Projection",draw = TRUE,
	nameX = "X", nameY = "Y",...)
{

  if(!is.matrix(x)) stop("x must be a matrix!")
  if(!is.null(y)) if(!is.matrix(y)) stop("y must be a matrix!")
  
  if (is.null(y)) 
  {
	
  dim_x <- dim(x)[2] 

	depth_est <- depth(x,x,method,...) 
 
	k = length(alpha)
	vol = 1:k 

	
		for(i in 1:k)
	{
		tmp_x <- x[depth_est >= alpha[i],]
		np <- dim(as.matrix(tmp_x))[1] 

		if (np > dim_x)
		{ 
			vol[i] <- convhulln(tmp_x,options = "FA")$vol
 		}
		else
		{
			vol[i]=0 

		}
	}
	

	a = c(1-alpha) 
	b = c(vol) 
	Set = c(rep(nameX,length(alpha)))
	xy_est = data.frame(x = a, y = b,Set = Set) 

	
	}
	
	if (!is.null(y))
    	{
       dim_x <- dim(x)[2] 
       dim_y <- dim(y)[2] 

        depth_est_x <- depth(x,x,method,...) 
      	depth_est_y <- depth(y,y,method,...) 
       
      	k = length(alpha)
      	vol_x = 1:k 
       vol_y = 1:k
	
    		for(i in 1:k)
    	{
	    	tmp_x <- x[depth_est_x >= alpha[i],]
		
	    	tmp_y <- y[depth_est_y >= alpha[i],]   
		
	    	np_x <- dim(as.matrix(tmp_x))[1]

    	np_y <- dim(as.matrix(tmp_y))[1] 


	  	if ((np_x > dim_x) && (np_y > dim_y) ){
	    		vol_x[i] <- convhulln(tmp_x,options = "FA")$vol
	    		vol_y[i] <- convhulln(tmp_y,options = "FA")$vol
 	  	}
	  	else
	   	{
	    		vol_x[i]=0 
		    	vol_y[i]=0

    	}

    	}
	

    	x = c(1-alpha,1-alpha) 
    	y = c(vol_x,vol_y) 
    	Set = c(rep(nameX,length(alpha)),rep(nameY,length(alpha)))
    	xy_est = data.frame(x = x, y = y,Set = Set)
	

	}

	names(xy_est) <- c("p","S_n","Set")

	if(draw)
	{
			p = ggplot()
			p = p + geom_line(data = xy_est, aes(x = p,y = S_n,colour = Set), size = 1.2)
			p = p + scale_color_manual(values=c("#E41A1C", "#377EB8"))
			p = p + theme_bw()
			p = p + ggtitle("Scale curve") 
			p = p + theme(title = element_text(face = "bold",vjust=1, size = 18))
			p = p + xlab("1-p")
			p = p + ylab(expression(S[n](p)))
			p = p + ylim(c(0,max(xy_est$S_n)))
			p = p + theme(axis.title.x  = element_text(face = "bold",vjust=0, size = 16))
			p = p + theme(axis.title.y  = element_text(face = "bold", angle = 90,vjust=0.2,size = 16))
			p = p + theme(axis.text.x  = element_text(size=14))
			p = p + theme(axis.text.y  = element_text(size=14))
      
      p
		
		
	}
	
	
	else
	{
		xy_est
	}
	
}