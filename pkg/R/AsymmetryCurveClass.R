setMethod("getPlot", "AsymmetryCurveList", function(object)
{
  p = .getPlot(object)
  p = p + ggtitle("ScaleCurveList")
  p = p + ylab("Volume")
  p = p + xlab("Alpha")
  return(p)
})
