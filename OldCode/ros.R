# ros<-function(x,y)
# {
# 	xx = sort(x)
# 	yy = y[order(x)]
# 
# 	n = length(xx)
# 		
# 	tmp = 1:(length(xx))
# 	tmp = rev(tmp)
# 	xxx = unlist(mapply(rep,xx,tmp))
# 	yyy = unlist(mapply(rep,yy,tmp))
# 	
# 	tmp_xxx = 1:length(xxx)
# 	tmp_yyy = 1:length(xxx)
# 	
# 	j=1
# 	tn = n
# 	for(i in 1:(n))
# 		{
# 				tmp_xxx[j:(j+tn-1)] = xx[i:n]
# 				tmp_yyy[j:(j+tn-1)] = yy[i:n]
# 				j = j + tn
# 				tn = tn - 1
# 		}
# 	
# 	xxx = cbind(xxx,tmp_xxx)
# 	yyy = cbind(yyy,tmp_yyy)
# 	
# 	yyy = yyy[xxx[,1]!=xxx[,2],]
# 	xxx = xxx[xxx[,1]!=xxx[,2],]
# 	
# 	slopes = (yyy[,2]-yyy[,1])/(xxx[,2]-xxx[,1])
# 	intercepts = (xxx[,2] * yyy[,1] - xxx[,1] * yyy[,2])/(xxx[,2] - xxx[,1])
# 	
# 	
# 
# W<-cbind(intercepts,slopes)
# 
# W
# 
# }
