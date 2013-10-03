\name{ddmvnorm}
\alias{ddmvnorm}

\title{Normal depth versus depth plot}
\description{
Produces a  normal DD plot of a multivirate dataset.
}
\usage{
ddmvnorm(x, method = "Projection", robust=FALSE, gamma=0.2, ...)
}
\arguments{
  \item{x}{The data sample for DD plot.}
  \item{method}{Character string which determines the depth function used. \code{method} can be "Projection" (the default), "Mahalanobis", "Tukey" or "Euclidean". For details see \code{\link{depth}}}
  \item{robust}{Logical. Dafault \code{FALSE}. If \code{TRUE}, robust measures are used to specify the parameters of theoretical distribution.}
  \item{gamma}{Parameter used to calculate the \code{index_Liu.}}
  \item{...}{Parameters passed to \code{depth}}
}
\details{ 
In the first step the location and scale of \code{x} are estimated and theoretical sample of normal distribution with those parameters is generated. The plot presents the depth o empirical points with reference to \code{x} and theoretical sample.  
}

\value{
Returns the normal depth versus depth plot of multivariate dataset \code{x}. Also \code{index_Liu} is calculated.
}

\references{Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 822--831.

Liu, R.Y., Singh K. (1993), A Quality Index Based on Data Depth and Multivariate Rank Test, \emph{Journal of the American Statistical Association} vol. 88.}

\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz, Zygmunt Zawadzki from Cracow University of Economics.}

\seealso{\code{\link{ddPlot}} to generate ddPlot to compare to datasets or to compare a dataset with other distributions.}

\examples{
require(MASS)
norm <- mvrnorm(1000, c(0,0,0), diag(3))
con <- mvrnorm(100, c(1,2,5), 3*diag(3))
Sample <- rbind(norm, con)
ddmvnorm(Sample, robust=TRUE)

}

\keyword{multivariate}
\keyword{nonparametric}
\keyword{robust}
\keyword{depth plot}
