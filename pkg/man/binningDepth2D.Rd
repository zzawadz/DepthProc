\name{binningDepth2D}
\alias{binningDepth2D}
\title{2d Binning}
\usage{
binningDepth2D(x, binmethod = "LocDepth", nbins = 8, k = 1,
  remove_borders = FALSE, devel = FALSE, ...)
}
\arguments{
  \item{x}{bivariate matrix}

  \item{binmethod}{method for calculation center and
  dispersion measures. "LocDepth" uses location-scale
  depth, MAD uses median and MAD in each dimension}

  \item{nbins}{number of bins in each dimension}

  \item{k}{scale parameter for dispersion measure}

  \item{remove_borders}{logical. Include or not marginal
  bins}

  \item{devel}{this is only for devel purposes and soon
  will be removed}

  \item{...}{other arguments passed to depthMedian}
}
\description{
Binning 2d
}
\examples{
binningDepth2D(x = mvrnorm(100,rep(0,2),diag(2)))
}
\seealso{
\code{\link{depth}}
}
\keyword{depth}
\keyword{function}
\keyword{multivariate}
\keyword{nonparametric}
\keyword{robust}

