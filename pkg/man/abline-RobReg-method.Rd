\docType{methods}
\name{abline,RobReg-method}
\alias{abline,RobReg-method}
\title{Add line to plot}
\usage{
\S4method{abline}{RobReg}(a, ...)
}
\arguments{
  \item{a}{the intercept and slope, single values.}

  \item{...}{\link{graphical parameters} such as
  \code{col}, \code{lty} and \code{lwd} (possibly as
  vectors: see \sQuote{Details}) and \code{xpd} and the
  line characteristics \code{lend}, \code{ljoin} and
  \code{lmitre}.}
}
\description{
Add fitted line to a plot. This is overloaded function for
robust regression methods from package depthproc.
}

