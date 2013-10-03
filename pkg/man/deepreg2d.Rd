\name{deepreg2d}
\alias{deepreg2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Under construction
}
\description{
Computes 2d regression model based on regression depth.
}
\usage{
deepreg2d(x, y)
}

\arguments{
  \item{x}{Independent variable}
  \item{y}{Dependent variable}
}

\details{
}
\value{
Function returns list of two objects:
\item{depth}{Value of regression depth}
\item{coef}{Vector of coefficients}
}
\references{
Rousseeuw J.P., Hubert M. (1998), Regression Depth, \emph{Journal of The American Statistical Association},  vol.94.
}
\author{
Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.
}


\examples{
 data(pension)
 plot(pension)
 abline(lm(Reserves~Income,data = pension), lty = 3, lwd = 2) #lm
 abline(deepreg2d(pension[,1],pension[,2])$coef, lwd = 2) #deepreg2d
}
