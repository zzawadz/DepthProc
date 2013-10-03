ddmvnorm <-function(x, method = "Projection", robust=FALSE, alpha=0.05, gamma=0.2, ...)     {
     

depth_sample <- depth(x, x, method, ...)

size <- nrow(x)


if (robust == TRUE) 
	{
		varcov <- cov(x[depth_sample>=quantile(depth_sample, alpha),])
		location <- med(x, method=method, ...)
	} 
else
	{ 
		location <- apply(x, 2, mean)
		varcov  <- cov(x) 
	}

theoretical <- mvrnorm(size, location, varcov)  

depth_theoretical <- depth(x, theoretical, method, ...)



	a_est = data.frame(alpha = depth_sample,norm = depth_theoretical)

index_Liu = 0
for (i in 1:nrow(a_est)) {
  if ((abs(a_est[i,1] - a_est[i,2])>gamma)) { index_Liu = index_Liu + 1 }
} 
print(paste("Index Liu: ", index_Liu))
	
	p = ggplot()
			p = p + geom_point(data = a_est, aes(alpha,norm),color = "blue",shape = 1)
			#p = p + scale_color_manual(values=c("#E41A1C", "#377EB8"))
			p = p + theme_bw()
      p = p + ggtitle("DDplot") 
      p = p + theme(title = element_text(face = "bold",vjust=1, size = 18))
			p = p + xlab("Theoretical depth")
			p = p + ylab("Sample depth")
      p = p + ylim(c(0,max(a_est$norm)))
      p = p + xlim(c(0,max(a_est$alpha)))
      p = p + theme(axis.title.x  = element_text(face = "bold",vjust=0, size = 16))
      p = p + theme(axis.title.y  = element_text(face = "bold", angle = 90,vjust=0.2,size = 16))
      p = p + theme(axis.text.x  = element_text(size=14))
      p = p + theme(axis.text.y  = element_text(size=14))
      p + geom_abline(color = "grey")


}