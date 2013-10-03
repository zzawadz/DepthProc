tmp_deepreg2d<-function(x,y){
y <- y[order(x)]
x <- sort(x)

depth2dcpp(x,y)

}

