\name{depthContour}
\alias{depthContour}
\title{
Approximate depth contours
}
\description{
Draws an approximate contours of depth for bivariate data.
}
\usage{
depthContour(X, method = "Projection", plot.title = paste(method,
                 "depth"), xlim = extendrange(X[, 1], f = 0.1), ylim =
                 extendrange(X[, 2], f = 0.1), n = 50, pmean = TRUE,
                 mcol = "blue", pdmedian = TRUE, mecol = "brown",
                 legend = TRUE, points = FALSE, ...)
}

\arguments{
  \item{X}{
Bivariate data as a matrix
}
  \item{method}{Character string which determines the depth function. \code{method} can be "Projection" (the default), "Mahalanobis", "Euclidean" or "Tukey". For details see \code{\link{depth}.}}
  \item{plot.title}{
Title of the plot
}
  \item{xlim}{
Determines the width of x-axis.
}
  \item{ylim}{
Determines the width of x-axis.
}
  \item{n}{
Number of points in each coordinate direction to be used in contour plot.
}
  \item{pmean}{
Logical. If TRUE mean will be marked.
}
  \item{mcol}{
Determines the color of lines describing the mean.
}
  \item{pdmedian}{
Logical. If TRUE depth median will be marked.
}
  \item{mecol}{
Determines the color of lines describing the depth median.
}
   \item{legend}{
Logical. If TRUE legend for mean and depth median will be drawn.
}
  \item{points}{
Logical. If TRUE points from matrix X will be drawn.
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
\code{\link{depthPersp}}
}
\examples{

#Example 1
require(MASS)

x = mvrnorm(1000,c(0,0),diag(2))
depthContour(x)

#with points
depthContour(x, points = TRUE)

}
\keyword{contour}
\keyword{depth}
