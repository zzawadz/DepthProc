deepReg2d_old<-function(x,y){

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


deepReg2d<-function(x,y){
  y <- y[order(x)]
  x <- sort(x)
  
  tmp = depth2dcpp(x,y)
  new("DeepReg2d", coef = tmp[2:1], depth = tmp[3])
}



