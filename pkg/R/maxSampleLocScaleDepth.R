maxSampleLocScaleDepth = function(x,iter=100,eps=0.0001,p_length=10)
{
  res = sampleMaxLocScaleDepthCPP(ry=as.numeric(x),iter=iter, eps=eps, p_length)
  res = as.numeric(res)
  names(res)<-c("max.depth","mu","sigma")
  return(res)
}
