deepreg2d<-function(x,y){

W<-ros(x,y)
M = 1:nrow(W)

x<-as.vector(x)
y<-as.vector(y)
xy<-cbind(x,y)
xy<-xy[order(xy[,1]),,drop=F]

for (i in 1: nrow(W))
M[i]<-rdepth(W[i,],xy)
return(list(depth = max(M)/length(x),coef = c(W[which.max(M),1],W[which.max(M),2])))
}
