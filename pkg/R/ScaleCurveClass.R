setMethod("getPlot", "ScaleCurveList", function(object)
{
  p = .getPlot(object)
  p = p + ggtitle("ScaleCurveList")
  p = p + ylab("Volume")
  p = p + xlab("Alpha")
  return(p)
})


setMethod("show", "ScaleCurveList", function(object){
})

setMethod("show", "ScaleCurve", function(object){
})

