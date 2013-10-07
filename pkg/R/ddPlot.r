ddPlot <- function (x, y = NULL, distribution = "mvnorm", method = "Projection", 
          scale = FALSE, location = FALSE, plot = TRUE, name_x = "X", name_y = "Y", ...) 
{
  if (is.null(y)) {
    size = nrow(x)
    d    = ncol(x) 
    
    if (distribution == "mvnorm") {
      theoretical <- mvrnorm(n = size, ...)
    }
    else if (distribution == "t") {
      theoretical <- rmt(n = size, ...)
    }
    else if (distribution == "smvnorm") {
      theoretical <- rmsn(n = size, ...)
    }
    else if (distribution == "st") {
      theoretical <- rmst(n = size, ...)
    }
    depth_sample <- depth(x, x, method, name = name_x)
    depth_theoretical <- depth(x, theoretical, method, name = name_y)
    ddplot = new("DDPlot", X = depth_sample, Y = depth_theoretical)
  }
  else {
    if (ncol(x) != ncol(y)) {
      print("Wrong dimensions of the datasets! ncol(x)!=ncol(y)")
    }
    else {
      if (scale == TRUE) 
      {
        depth_sample_x <- depth(x, x, method, name = name_x)
        depth_sample_y <- depth(y, y, method, name = name_y)
        varcovx <- cov(x[which(depth_sample_x >= median(depth_sample_x)), ])
        varcovy <- cov(y[which(depth_sample_y >= median(depth_sample_y)), ])
        x_new <- t(solve(chol(varcovx)) %*% t(x))
        y_new <- t(solve(chol(varcovy)) %*% t(y))
      }
      else 
      {
        x_new <- x
        y_new <- y
      }
    }
    if (location == TRUE) 
    {
      medx <- med(x_new)
      medy <- med(y_new)
      x_new <- x_new - medx
      y_new <- y_new - medy
    }
    data <- rbind(x_new, y_new)
    depth_x <- depth(data, x_new, method, name = name_x)
    depth_y <- depth(data, y_new, method, name = name_y)
    
    ddplot = new("DDPlot", X = depth_x, Y = depth_y)
  }
  
  if(plot) plot(ddplot)
  return(ddplot)
}