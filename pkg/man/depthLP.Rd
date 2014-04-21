\name{depthLP}
\alias{depthLP}
\title{LP Depth}
\usage{
depthLP(u, X, pdim = 1, a = 1, b = 1, name, func = NULL, ...)
}
\arguments{
  \item{u}{Numerical vector or matrix whose depth is to be
  calculated. Dimension has to be the same as that of the
  observations.}

  \item{X}{The data as a matrix, data frame or list. If it
  is a matrix or data frame, then each row is viewed as one
  multivariate observation. If it is a list, all components
  must be numerical vectors of equal length (coordinates of
  observations).}

  \item{pdim}{1}

  \item{a}{1}

  \item{b}{1}

  \item{name}{name for this data set - it will be used on
  plots from depthproc.}
}
\description{
Computes the LP depth of a point or vectors of points with
respect to a multivariate data set.
}
\details{
Returns the depth of multivariate point \code{u} with
respect to data set \code{X}.
}
\examples{
x <- matrix(rnorm(3000), nc = 3)

 #Same results
 depthLP(x, x, ndir = 2000, p = 2)
}
\author{
Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and
Zygmunt Zawadzki from Cracow University of Economics.
}
\keyword{depth}
\keyword{function}
\keyword{multivariate}
\keyword{nonparametric}

