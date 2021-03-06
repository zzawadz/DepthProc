% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{DepthCurve-class}
\alias{DepthCurve-class}
\title{DepthCurve}
\description{
This page describes mechanism behavior of ScaleCurve and AsymmetryCurve
}
\details{
DepthCurve is a virtual class that contains methods (getPlot(...) and plot(...)) for rendering single curve such as ScaleCurve or AsymmetryCurve. Such object can be combined by overloaded operator '%+%'. This 'addition' create DepthCurveList that can be used for rendering plot with multiple curves. Sample session (using ScaleCurve) is shown in Examples section.
}
\section{Slots}{

\describe{
\item{\code{depth}}{object of \link{Depth-class}}

\item{\code{name}}{name of dataset used on plot}

\item{\code{title}}{title of a plot}

\item{\code{alpha}}{central area values}
}}

\examples{
library(mvtnorm)
x <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 2 * diag(2))
y <- rmvt(n = 100, sigma = diag(2), df = 4)
s1 <- scaleCurve(x, depth_params = list(method = "Projection"))
s2 <- scaleCurve(y, depth_params = list(method = "Projection"), name = "Set2")

sc_list <- combineDepthCurves(s1, s2) # Add one curve to another

plot(sc_list) # Draw plot with two curves

z <- mvrnorm(n = 100, mu = c(0, 0), Sigma = 1 * diag(2))
s3 <- scaleCurve(z, depth_params = list(method = "Projection"))
plot(combineDepthCurves(sc_list, s3)) # Add third curve and draw a plot

}
