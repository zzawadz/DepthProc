\name{depthPersp}
\alias{depthPersp}
\title{
Perspective plot for depth functions
}
\description{
Draws a perspective plot of depth function over x-y plane.
}
\usage{
depthPersp(X,method = "Projection",plot.method = "rgl",xlim = extendrange(X[,1],f=0.1),ylim = extendrange(X[,2],f=0.1),
n=50,xlab = "x", ylab = "y",plot.title=paste(method,"depth"),...)
}

\arguments{
  \item{X}{
Bivariate data as a matrix
}
 \item{method}{Character string which determines the depth function. \code{method} can be "Projection" (the default), "Mahalanobis", "Euclidean" or "Tukey". For details see \code{\link{depth}.}}
  \item{plot.method}{
Method used to draw the plot. Default - "rgl" draws rotatable plot using package rgl. Another possible method "lattice" uses lattice package for 3d surface plot. 
}
  \item{xlim}{
Determines the width of x-axis.
}
  \item{ylim}{
Determines the width of x-axis.
}
  \item{n}{
Number of points in each coordinate direction to be used in perspective plot.
}
  \item{xlab}{
Title of x-axis.
}
  \item{ylab}{
Title of ylab.
}
  \item{plot.title}{
Title of plot.
}
  \item{\dots}{
Any additional parameters for function depth
}
}
\details{
}

\references{}

\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.}

\seealso{
\code{\link{depthContour}}
}
\examples{
\dontrun{
#Example 1
require(MASS)

x = mvrnorm(100,c(0,0),diag(2))
depthPersp(x)

#Example 2

depthPersp(x,plot.method = "lattice")
}
}
\keyword{plot}
\keyword{depth}
