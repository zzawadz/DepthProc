setClass("CovDepthWeighted", representation(depth = "character"), contains="CovRobust")

CovLP = function(X, p, a, b)
{
  cov = CovLPCPP(X, p, a, b)
  
  method = "Depth Weighted Estimator"
  new("CovDepthWeighted", cov = cov,
      center = colMeans(X),
      det = det(cov),
      n.obs = nrow(X),
      X = X,
      method = method)
}


