ddPlot <- function (x, y = NULL, distribution = "mvnorm", method = "Projection", 
          scale = FALSE, location = FALSE, gamma=0.2, ...) 
{
  if (is.null(y)) {
    size <- nrow(x)
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
    depth_sample <- depth(x, x, method)
    depth_theoretical <- depth(x, theoretical, method)
    a_est = data.frame(alpha = depth_sample, norm = depth_theoretical)
    
    index_Liu = 0
    for (i in 1:nrow(a_est)) {
      if ((abs(a_est[i,1] - a_est[i,2])>gamma)) { index_Liu = index_Liu + 1 }
    } 
    print(paste("Index Liu: ", index_Liu))
    
    p = ggplot()
    p = p + geom_point(data = a_est, aes(alpha, norm), color = "blue", 
                       shape = 1)
    p = p + theme_bw()
    p = p + ggtitle("DDplot")
    p = p + theme(title = element_text(face = "bold", vjust = 1, 
                                       size = 18))
    p = p + xlab("Theoretical depth")
    p = p + ylab("Sample depth")
    p = p + ylim(c(0, max(a_est$norm)))
    p = p + xlim(c(0, max(a_est$alpha)))
    p = p + theme(axis.title.x = element_text(face = "bold", 
                                              vjust = 0, size = 12))
    p = p + theme(axis.title.y = element_text(face = "bold", 
                                              angle = 90, vjust = 0.2, size = 12))
    p = p + geom_abline(color = "grey")
    p
    
  }
  else {
    if (ncol(x) != ncol(y)) {
      print("Wrong dimensions of the datasets!")
    }
    else {
      if (scale == TRUE) {
        depth_sample_x <- depth(x, x, method, ...)
        depth_sample_y <- depth(y, y, method, ...)
        varcovx <- cov(x[which(depth_sample_x >= median(depth_sample_x)), 
                         ])
        varcovy <- cov(y[which(depth_sample_y >= median(depth_sample_y)), 
                         ])
        x_new <- t(solve(chol(varcovx)) %*% t(x))
        y_new <- t(solve(chol(varcovy)) %*% t(y))
      }
      else {
        x_new <- x
        y_new <- y
      }
    }
    if (location == TRUE) {
      medx <- med(x_new)
      medy <- med(y_new)
      x_new <- x_new - medx
      y_new <- y_new - medy
    }
    data <- rbind(x_new, y_new)
    depth_x <- depth(data, x_new, method, ...)
    depth_y <- depth(data, y_new, method, ...)
    a_est = data.frame(alpha = depth_y, norm = depth_x)
    
    index_Liu = 0
    for (i in 1:nrow(a_est)) {
      if ((abs(a_est[i,1] - a_est[i,2])>gamma)) { index_Liu = index_Liu + 1 }
      } 
    print(paste("Index Liu: ", index_Liu))
   
    medx <- med(x)
    medy <- med(y)
      
    p = ggplot()
    p = p + geom_point(data = a_est, aes(alpha, norm), color = "blue", 
                       shape = 1)
    p = p + theme_bw()
    p = p + ggtitle("Depth vs. depth plot")
    p = p + theme(title = element_text(face = "bold", vjust = 1, 
                                       size = 18))
    p = p + xlab("X depth")
    p = p + ylab("Y depth")
    p = p + ylim(c(0, max(a_est$norm)))
    p = p + xlim(c(0, max(a_est$alpha)))
    p = p + theme(axis.title.x = element_text(face = "bold", 
                                              vjust = 0, size = 16))
    p = p + theme(axis.title.y = element_text(face = "bold", 
                                              angle = 90, vjust = 0.2, size = 16))
    p = p + theme(axis.text.x = element_text(size = 14))
    p = p + theme(axis.text.y = element_text(size = 14))
    p = p + geom_abline(color = "grey")
    p
    
    
    
  }
}