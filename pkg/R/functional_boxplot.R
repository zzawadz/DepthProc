#' Functional boxplot based on Modified Band Depth
#' @export
#' 
#' @param x data matrix
#' @param band_lim limits for bands
#' @param colors color set
#' @param add_lines Logical. If TRUE, all lines from data will be plotted.
#' @param lwd lines width
#' @param alpha transparency for band area.
#' @param \dots other arguments passed to matplot
#'
#' @examples
#' x  = matrix(rnorm(200), ncol = 10)
#' fncBoxPlot(x)
#'
fncBoxPlot = function(x, band_lim = c(0,0.5), colors = NULL, add_lines = TRUE, lwd = 1, alpha = 0.5, ...)
{
  depths = fncDepth(x)
  
  x = t(x)
  
  matplot(x, type = "n", col = "gray90", lty = 1)
  
  
  
  band_lim = quantile(depths, band_lim)
  if(is.null(colors))
  {
    colors = rev(gray.colors(length(band_lim), start = 0.4))
  }
  
  col_lines = colors
  if(alpha != 1) 
  {
    colors = .addAlpha(colors, alpha)
  }
  
  for(i in seq_along(band_lim))
  {
    
    tmp_x = x[,depths >= band_lim[i]]
    range = t(apply(tmp_x, 1, range))
    range[,2] = rev(range[,2])
    xx = c(1:nrow(range), nrow(range):1)
    polygon(xx, range, border = NA, col = colors[i])
    
  }
  
  if(add_lines)
  {
    col_lines
    col = rep(0,ncol(x))
    
    for(i in seq_along(band_lim))
    {
      col = col + as.numeric(depths >= band_lim[i])
    }
    
    sapply(1:ncol(x), function(i) lines(x[,i], col = col_lines[col[i]]))
    lines(x[,which.max(depths)], lwd = lwd*2)
  }
  
}



