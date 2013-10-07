#### ScaleCurve
setMethod("+", signature(e1 = "ScaleCurve", e2 = "ScaleCurve"), function(e1, e2)
{
  tmp = new("ScaleCurveList")
  tmp[[1]] = e1
  return(tmp+e2)
})

setMethod("plot", signature = c(x = "ScaleCurve", y = "missing"), function(x, y = "missing")
{
  plot(new("ScaleCurveList",x))
})

#### Scale Curve List

#############################################################################
setMethod("initialize","ScaleCurveList", function(.Object, ...)
{
  tmp = list(...)
  n = length(tmp)
  if(n>0)
  {
    .Object[[1]] = tmp[[1]]
    if(n > 1) for(i in 2:length(tmp)) .Object = .Object + tmp[[i]]
  }
  return(.Object)
})
#############################################################################
setMethod("+", signature(e1 = "ScaleCurveList", e2 = "ScaleCurve"), function(e1, e2)
{
  names = sapply(e1,function(x) x@depth@name)
  new_name = e2@depth@name
  if(any(new_name == names))
  {
    warning("Names in ScaleCurveList are not unique!")
    k = 1
    new_name_tmp = paste0(new_name)
    while(any(new_name_tmp == names)) { new_name_tmp = paste0(new_name,k); k = k+1}
    e2@depth@name = new_name_tmp
  }
  
  n = length(e1)
  e1[[n+1]] = e2
  return(e1)
})

######################################################################
setMethod("+", signature(e1 = "ScaleCurve", e2 = "ScaleCurveList"), function(e1, e2)
{
  return(e2+e1)
})

######################################################################
setMethod("as.matrix", signature(x = "ScaleCurveList"), function(x)
{
   names = sapply(object,function(x) x@depth@name)
   tmp = matrix(unlist(object), ncol = length(object))
   colnames(tmp) = names
   tmp
})


##########################################################
setMethod("getPlot", "ScaleCurveList", function(object)
{
  vol = unlist(object)
  alpha = as.vector(sapply(object, function(x) x@alpha))
  len_alpha = sapply(object, function(x) length(x@alpha))
  names = sapply(object,function(x) x@depth@name)
  names = rep(names,len_alpha)
  
  data = data.frame(vol, alpha, names)
  
  p = ggplot()
  p = p + geom_line(data = data, aes(x = alpha,y = vol, col = names), size = 1.5)
  p = p + theme_bw() + .depTheme()
  p = p + ggtitle("Depth vs. depth plot")
  p = p + xlab("Volume")
  p = p + ylab("Alpha")
  p = p + ylim(c(0, max(data$vol)))
  p = p + xlim(c(0, max(data$alpha)))
  return(p)
})

setMethod("plot", signature = c(x = "ScaleCurveList", y = "missing"), function(x, y = "missing")
{
  p = getPlot(x)
  print(p)
})

setMethod("show", "ScaleCurveList", function(object){
})

setMethod("show", "ScaleCurve", function(object){
})
