\name{asymmetrycurve}
\alias{asymmetrycurve}
\title{ Asymmetry curve based on depths }
\description{
Draws a asymmetry curve estimated from.
}
\usage{
asymmetrycurve(X,Y = NULL, alpha = seq(0,1,0.01), method = "Projection",
  movingmedian = FALSE,draw = TRUE,nameX = "X", nameY = "Y",,...)
}
\arguments{
  \item{X}{Multivariate data as a matrix. }
  \item{Y}{Additional matrix with multivariate data.}
  \item{alpha}{Vector with values of central area to be used in computing assymetry norm}
  \item{method}{#tu mozna przekopiowac method z opisu depth}
  \item{movingmedian}{Logical. For default FALSE only one depth median is used to compute asymmetry norm. If TRUE - for every central area new depth median will be used - this approach needs much more time.}
  \item{draw}{Logical. Default TRUE produces aassymetry curve plot; otherwise, returns a data frame containing the central areas and asymmetry norm values over these points.}
  \item{nameX}{Name of set X - used in plot legend}
  \item{nameY}{Name of set Y - used in plot legend}
  \item{...}{Any additional parameters for function depth}
}
\details{\code{ggplot2} package is used to draw a plot.

Asymmetrycurve takes advantage of function \code{convhulln} to compute volume of central area's convex hull

}
\value{Default \code{draw = TRUE} yields a asymmetry curve; otherwise the function returns a data frame with components:
  \item{alpha}{Central area used to compute assymetry norm}
  \item{norm}{Compute Asymmetry norm for certain central area}
  \item{Set}{Factor that determines the set of data ("X" or "Y")}
}
\references{Serfling  R. J.  Multivariate Symmetry and Asymmetry, \emph{Encyclopedia of Statistical Science}, S Kotz, C.B. Read, N. Balakrishnan, B. Vidakovic (eds), 2nd, ed., John Wiley.

Liu, R.Y., Parelius, J.M. and Singh, K. (1999), Multivariate analysis by data depth: Descriptive statistics, graphics and inference (with discussion), \emph{Ann. Statist.}, \bold{27}, 783--858.

Chaudhuri, P. (1996), On a Geometric Notion of Quantiles for Multivariate Data, \emph{Journal of the American Statistical Association}, 862--872.

Dyckerhoff, R. (2004), Data Depths Satisfying the Projection Property, \emph{Allgemeines Statistisches Archiv.},  \bold{88}, 163--190.}


\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.}

\seealso{\code{\link{scalecurve}}, \code{\link{depth}} }
\examples{

  require(sn)
  require(MASS)
  
  xi = c(0,0)
  alpha <- c(2,-5)
	Omega <- diag(2)*5
	
	n = 1000

	X = mvrnorm(n, xi, Omega)  # normal distribution
	Y = rmst(n, xi, Omega, alpha, df=Inf)  # skew normal distribution
	Y1 = rmst(n, xi, Omega, alpha, df=1)
	asymmetrycurve(X,Y1,nameX = "NORM",nameY = "S_T(2,-5,10)")
}

\keyword{multivariate}
\keyword{nonparametric}
\keyword{depth}
\keyword{asymmetry}
