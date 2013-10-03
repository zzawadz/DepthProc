\name{ddPlot}
\alias{ddPlot}
\title{Depth versus depth plot}

\description{
Produces a DD plot which allows to compare two multivirate datasets or to compare a subject dataset with theoretical distribution.
}

\usage{
ddPlot(x, y=NULL, distribution = c("mvnorm", "t", "smvnorm",
"st"), method = "Projection", scale = FALSE, location = FALSE, gamma=0.2, ...)
}

\arguments{
  \item{x}{The first or only data sample for ddPlot.}
  \item{y}{The second data sample. \code{x} and \code{y} must be of the same space.}
  \item{distribution}{Character string which determines with which distribution the \code{x} sample should be compared. \code{distribution} can be "mvnorm", "t", "smvnorm", "st".}
  \item{method}{Character string which determines the depth function used.\code{method} can be "Projection" (the default), "Mahalanobis", "Tukey" or "Euclidean". For details see \code{\link{depth}}}
  \item{scale}{Logical. Dafault \code{FALSE}. If \code{TRUE}, centres of the samples are equalized.}
  \item{location}{Logical. Dafault \code{FALSE}. If \code{TRUE}, scales of the samples are equalized.}
  \item{gamma}{Parameter used to calculate the \code{index_Liu.}}
  \item{...}{Parameters passed to \code{rmnorm}, \code{rmt}, \code{dmsn}, \code{rmst} }
}

\details{ 

When \code{distribution} is "mvnorm" the sample dataset is compared with multivirate normal distribution with given parameters. \code{mean} and \code{varcov} must be specified.

When \code{distribution} is "t" the sample dataset is compared with multivirate t-Student distribution with given parameters. \code{mean} and \code{S} must be specified. \code{df=Inf} by default.

When \code{distribution} is "smvnorm" the sample dataset is compared with multivirate skew normal distribution with given parameters. \code{xi}, \code{Omega} and \code{alpha} must be specified.

When \code{distribution} is "st" the sample dataset is compared with multivirate skew t-Student distribution with given parameters. \code{xi}, \code{Omega} and \code{alpha} must be specified. \code{df=Inf} by default.

}

\value{
Returns the Depth versus depth plot of multivariate dataset \code{x} and theoretical sample or of \code{x} and \code{y}. Also \code{index_Liu} is calculated.
}

\references{Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 822--831.

Liu, R.Y., Singh K. (1993), A Quality Index Based on Data Depth and Multivariate Rank Test, \emph{Journal of the American Statistical Association} vol. 88.
}

\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz, Zygmunt Zawadzki from Cracow University of Economics.}

\seealso{\code{\link{ddmvnorm}} to generate DD plot with reference to normal distribution.}

\examples{

## Location difference
require(MASS)
Standard <- mvrnorm(1000, c(0,0), diag(2))
Shift <- mvrnorm(1000, c(0.5, 0), diag(2))
ddPlot(x=Standard, y=Shift)
ddPlot(x=Shift, distribution='mvnorm', mu=c(0,0), Sigma=diag(2))
ddPlot(x=Standard, y=Shift, location=TRUE)

## Scale difference
require(MASS)
Standard <- mvrnorm(1000, c(0,0), diag(2))
Scale <- mvrnorm(1000, c(0,0), 4*diag(2))
ddPlot(x=Standard, y=Scale, scale=TRUE)

## Skewness difference
require(MASS)
require(sn)
Standard <- mvrnorm(1000, c(0,0), diag(2))
Skew <- rmsn(1000, xi=c(0,0), Omega= diag(2), alpha=c(6,1))
ddPlot(x=Standard, y=Skew)

## Kurtosis difference
require(MASS)
require(mnormt)
Standard <- mvrnorm(1000, c(0,0), diag(2))
Kurt <-rmt(1000, mean=c(0,0), S=diag(2), df=1)
ddPlot(x=Standard, y=Kurt)
}

\keyword{multivariate}
\keyword{nonparametric}
\keyword{robust}
\keyword{depth plot}
