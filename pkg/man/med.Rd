\name{med}
\alias{med}
\title{
Depth median
}
\description{
Computes a multivariate depth median.
}
\usage{
med(X, method = "Projection", ...)
}
\arguments{
\item{X}{Multivariate data as a matrix. }
\item{method}{Character string which determines the depth function. \code{method} can be "Projection" (the default), "Mahalanobis", "Euclidean" or "Tukey". For details see \code{\link{depth}.}}
\item{...}{Any additional parameters for function \code{\link{depth}}.}
}

\details{
The minimal dimension of data in X is 2.
}

\value{
Returns the multivariate vector as a multivariate depth median of considered data.
}

\references{Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 783--858.

Chaudhuri, P. (1996), On a Geometric Notion of Quantiles for Multivariate Data, \emph{Journal of the American Statistical Association}, 862--872.

Dyckerhoff, R. (2004), Data Depths Satisfying the Projection Property, \emph{Allgemeines Statistisches Archiv.},  \bold{88}, 163--190.}

\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.}

\seealso{See also \code{\link{depthContour}} and \code{\link{depthPersp}} for other graphics related with depth median.}

\examples{
require(MASS)
x <- mvrnorm(1000, c(0,0,0), diag(3))
med(x)

}

\keyword{median}
\keyword{depth function}
\keyword{multivariate quantiles}