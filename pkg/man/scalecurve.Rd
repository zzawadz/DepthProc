\name{scalecurve}
\alias{scalecurve}

\title{Scale curve}
\description{
Draws a scale curve: measure of dispersion.
}
\usage{
scalecurve(x, x = NULL, alpha = seq(0,1,0.01), method = "Projection", draw = TRUE, nameX = "X", nameY = "Y", ...)
}
\arguments{
  \item{x}{Multivariate data as a matrix.}
  \item{y}{Additional matrix with multivariate data.}
  \item{alpha}{Vector with values of central area to be used in computation.}
  \item{method}{Character string which determines the depth function used. \code{method} can be "Projection" (the default), "Mahalanobis", "Euclidean" or "Tukey". For details see \code{\link{depth}.}}
  \item{draw}{Logical. Default TRUE produces scalecurve plot; otherwise, returns a data frame containing the central areas and their volume.}
  \item{nameX}{Name of matrix X used in legend.}
  \item{nameY}{Name of matrix Y used in legend.}
  \item{...}{Any additional parameters for function \code{depth}.}
  }

\details{ The scale curve is a two-dimensional method to describe the dispersion of random vector around the median induced by considered depth function.

Function \code{scalecurve}, when determining the volumes of the convex hull containing subsequent points from alpha central region, uses function \code{convhulln} from \code{geometry} package.

The minimal dimension of data in X or Y is 2.

\code{ggplot2} package is used to draw a plot.

}
\value{
Returns the volume of the convex hull containing subsequent central points of \code{X}.
}
\references{Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 783--858.

Chaudhuri, P. (1996), On a Geometric Notion of Quantiles for Multivariate Data, \emph{Journal of the American Statistical Association}, 862--872.

Dyckerhoff, R. (2004), Data Depths Satisfying the Projection Property, \emph{Allgemeines Statistisches Archiv.},  \bold{88}, 163--190.}

\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.}

\seealso{\code{\link{ddplot}} and \code{\link{asymmetrycurve}} for other depth graphics.}
\examples{## comparing of two scale curves - normal distribution and Student (df=2)

require(MASS)
require(mvtnorm)
x = mvrnorm(n = 100, mu = c(0,0), Sigma = 3*diag(2))
y = rmvt(n = 100, sigma = diag(2), df = 2)
scalecurve(x, y, method = "Projection", draw = TRUE)


## comparing of two scale curves - normal distribution and mixture of normal distributions

require(MASS)
x = mvrnorm(100, c(0,0), diag(2))
y = mvrnorm(80, c(0,0), diag(2))
z = mvrnorm(20, c(5,5), diag(2))
scalecurve(x, rbind(y,z), method = "Projection", draw = TRUE, nameX = "N", nameY = "Mixture of N")


}

\keyword{scale curve}
\keyword{depth}
\keyword{multivariate quantiles}
