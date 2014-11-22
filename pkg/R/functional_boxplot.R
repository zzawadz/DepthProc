#' Functional boxplot based on Modified Band Depth
#' @export
#' 
#' @param x data matrix
#' @param bands limits for bands
#' @param method depth method
#' @param type method used for create plot ("ggplot2" or "base")
#' @param \dots other arguments passed to fncDepth
#'
#' @examples
#' x  = matrix(rnorm(200), ncol = 10)
#' fncBoxPlot(x, bands = c(0, 0.5, 1), method = "FM")
#' fncBoxPlot(x, bands = c(0, 0.5, 1), method = "MBD")
#' 
#' # fncBoxPlot handles zoo and xts objects
#' library(xts)
#' x  = matrix(rnorm(200), ncol = 10)
#' time = as.POSIXct(1:nrow(x) * 86400, origin = "1970-01-01")
#' x = xts(x, order.by = time)
#' fncBoxPlot(x, bands = c(0, 0.5, 1), method = "FM")
#' 
fncBoxPlot = function(x, bands = c(0,0.5), method = "MBD", type = "ggplot2", ...)
{
  depths = fncDepth(x, X = NULL, method = method, ...)
  if(type == "ggplot2") return(.fncBoxPlotGGPlot(depths, bands))
}


# Create Functional BoxPlot based on ggplot2
.fncBoxPlotGGPlot = function(obj, bands)
{
  bands = fncGetBandsDataFrame(obj, bands)
  p = ggplot(bands, aes_string(x = "index", fill = "level", color = "level")) 
  p = p + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"))
  p = p + theme_bw()
  p + scale_fill_brewer(palette = "Blues") + scale_color_brewer(palette = "Blues") 
}


############## Other functions ############


#' @title Functional bands
#' @description Extract bands from functional depth object.
#' @export
#' 
#' @param obj object that inherits from FunctionalDepth.
#' @param band single numeric value.
#' 
#' @examples
#' 
#' x = matrix(rnorm(600), nc = 20)
#' obj = fncDepth(x, method = "FM", dep1d = "Mahalanobis")
fncGetBand = function(obj, band = 0.5)
{
  u = obj@u
  depths = as.numeric(obj)
  bands_q = quantile(obj, 1 - band)
  tmp_u = u[depths >= bands_q, , drop = FALSE]
  bands = t(apply(tmp_u, 2, range))
  new("FncBand", bands, index = obj@index, level = band)
}


fncBand2DataFrame = function(band)
{
  data.frame(index = band@index,lower = band[,1], upper = band[,2], level = band@level)
}

fncGetBandsDataFrame = function(obj, bands = c(0.25, 0.75))
{
  bands = sort(bands, decreasing = TRUE)
  bands_list = lapply(bands, function(x) fncBand2DataFrame(fncGetBand(obj, x)))
  
  bands = Reduce(rbind, bands_list)
  
  levels = paste0(bands$level * 100, "%")
  bands$level = factor(levels, levels = unique(levels),  ordered = TRUE)
  bands
}

