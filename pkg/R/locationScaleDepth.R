# All functions realted with lsd package is here
# Some function were rewritten in C++ for greater efficiency
# and are located in src/LocationDepth.cpp

# Functions names from lsd were translated to harmonize name
# conventions in depthproc (lowerCamelCase for functions)
## - sample.max.depth -> maxSampleLocScaleDepth (CPP)
## - sample.depth.contours -> sampleDepthContours (R)

## TO DO: 
## Write interface to all C++ functions
## Wirte documentation
## Tests!!!
## Refactor code for sampleDepthContours? Proper object function itp.

#' @title Location-Scale depth class
#' @export
setClass("LSDepth", slots = c(max_depth = "numeric",
                              mu = "numeric",
                              sigma = "numeric"))


#' @title Location-Scale depth contour class
#' @export
setClass("LSDepthContour", slots = c(cont_depth = "numeric", sample = "numeric"), contains = "list")


#' @title Calculates the maximum sample location-scale depth
#' @export
maxSampleLocScaleDepth = function(x,iter=100,eps=0.0001,p_length=10)
{
  res = sampleMaxLocScaleDepthCPP(ry=as.numeric(x),iter=iter, eps=eps, p_length)
  res = as.numeric(res)
  
  new("LSDepth", max_depth = res[1], mu = res[2], sigma = res[3])
  #names(res)<-c("max.depth","mu","sigma")
  #return(res)
}


#' @title Calculate sample location scale depth contours
#' @export
sampleDepthContours = function(x, depth = c(0.1,0.2,0.3,0.4), lengthmu=1000)
{
  depth = round(depth*length(x))
  x = sort(x)
  n = length(x)  
  
  dlen = length(depth)
  cont.all = vector("list",length=length(depth))
  
  for(i in 1:dlen)
  {
    d = depth[i]
    mu = getMuLS(x,n,d,lengthmu)
    cont = t(sapply(mu, function(mu) as.numeric(depthproc:::sampleDepthContForMuCPP(d,mu[1],x))))
    colnames(cont) = c("lbound","ubound","tbound","case","M")
    
    if(sum(cont[,"tbound"])>1){
      cont.exist = T
      tbound = as.logical(cont[,"tbound"])
      cont = cont[tbound,]
      lbound = cont[,"lbound"]
      ubound = cont[,"ubound"]
      mubound = mu[tbound]
      cont.all[[i]] = list(depth=d,cont.exist=cont.exist,mubound=mubound,
                           lbound=lbound,ubound=ubound)
    }
    else{
      cont.exist = F
      cat(" No contour for the depth ", d, "\n")
      cont.all[[i]] = list(depth=d,cont.exist=cont.exist)
    }
    
  }
  cont.all
  new("LSDepthContour", cont.all, cont_depth = depth/length(x), sample = x)
}


######################## Utils functions - not exported #####################
getMuLS = function(x,n,d,lengthmu)
{
  if(d>1){
    mu = seq(x[1+d-1],x[n-d+1],length=(lengthmu+1))
  }
  else{
    mu = seq(x[1],x[n],length=(lengthmu+1))
  }
  mu
}


#' @title Generic class for getLSDContour
#' @export
setGeneric("getLSDContour", function(x, cont,...) standardGeneric("getLSDContour"))
#' @title Get location scale contour from LSDepthContour object
#' @export
setMethod("getLSDContour", signature = "LSDepthContour",function(x, cont, ...)
{
  i = which(x@cont_depth == cont)
  if(length(i) > 0) return(x@.Data[[i]])
  sampleDepthContours(x@sample, depth = cont)[[1]] 
})


