rdepth<-function(b,xy)
{

	####Funkcja wewnêtrzna#####
			sumident<-function(x,y)

	{
		z<-apply(y[x[1]:x[2], ,drop=F],2,sum)
		z
	}
	############################


	x<-xy[,1]
	y<-xy[,2]
	n = nrow(xy)

	res<- y-b[2]*x-b[1]

	res[abs(res)<10^-7]<-0

	posres<-res>=0

	negres<-res<=0

	dupx<-duplicated(x)

	if(sum(dupx))
	{

		r1<-(1:n)[!dupx]

		if(length(r1)==1) r2<-n-1
		else r2<-c(diff(r1)-1,n-max(r1))

		r1<-cbind(r1,r1+r2)

		res<-apply(r1,1,sumident,cbind(posres,negres))
		posres<-res[1,]
		negres<-res[2,]
		n<-length(posres)
	
	}

	lplus<-cumsum(posres)
	rplus<-lplus[n]-lplus
	lmin<-cumsum(negres)
	rmin<-lmin[n]-lmin
	depth<-pmin(lplus+rmin,rplus+lmin)
	min(depth) 
}