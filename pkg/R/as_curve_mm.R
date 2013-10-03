as_curve_mm<-function(X,alpha = NULL,method,...)
{
  
  dim_X <- dim(X)[2]
  
  depth_est <- depth(X,X,method,...)
  
  if(is.null(alpha)) alpha <- seq(0,1,0.01)
  
  
  k <-length(alpha)
  vol = 1:k 
  alpha_est = 1:k
  means = matrix(nrow = k, ncol = dim_X) 
  medians = matrix(nrow = k, ncol = dim_X)
  
  for(i in 1:k)
  {
    tmp_X <- X[depth_est >= alpha[i],]
    np <- dim(as.matrix(tmp_X))[1] 
    
    if ((np > ((2*(dim_X+2))^2))&(np > 100))
    { 
      vol[i] <- convhulln(tmp_X,options = "FA")$vol
      alpha_est[i] <- alpha[i]
      means[i,] <- colMeans(tmp_X)
      
      tmp_depth_est = depth(tmp_X,tmp_X,method,...)
      
      med = tmp_X[tmp_depth_est==max(tmp_depth_est),]
      
      if((length(med)/dim(X)[2])!=1)
      {
        
        
        
        med = colMeans(med)
        
      }
      medians[i,] = med
      
     
    }
    else
    {
      vol[i]=NA 
      alpha_est[i] <- NA	
    }
  }
  

  vol <- vol[!is.na(vol)]
  alpha_est <- alpha_est[!is.na(alpha_est)]
  means <- matrix(means[!is.na(means)],ncol=dim_X)
  medians <- matrix(medians[!is.na(medians)],ncol=dim_X)
  #koniec usuwani NA
  
  
  k = length(vol)
  kvol = matrix(rep(vol,dim_X),ncol=dim_X) 
  n=(means - medians)#/(kvol^(1/dim_X))
  nn = 2*sqrt(rowSums(n^2))/(vol^(1/dim_X)) 
  matrix(c(rev(1-alpha_est),rev(nn)),ncol=2)
  
}