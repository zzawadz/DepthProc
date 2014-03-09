

.depthLocal = function(u, X, beta, depth1, depth2, ...)
{
  ncol = ncol(X)
  nrow = nrow(X)
  symDATA = rbind(X, t(apply(X, 1, function(k) 2*u-k)))
  depths = as.numeric(depth(X, symDATA, method=depth1))
  quan = quantile(depths, probs=1-beta)
  Rset = as.matrix(X[ signif(depths,digits=6) >= signif(quan,digits=6), ])
  as.numeric(depth(u, Rset, method=depth2,...))
}

depthLocal = function(u, X, beta=0.5,
                      depth1="Projection", depth2=depth1,name = "X", ...) {
  depths = 1:nrow(u)
  for(i in 1:nrow(u)) depths[i] = .depthLocal(u[i,,drop = FALSE],X,beta,depth1,depth2)
  return(depths)
}
 
