\name{mregdepth}
\alias{mregdepth}

\title{
mregdepth
}
\description{
Computes single and multivariate regression depth for specific coefficients.
}
\usage{
mregdepth(coef, x, y, ndir = 1000)
}

\arguments{
  \item{coef}{Coefficients for certain fit. First element is a intercept}
  \item{x}{Matrix with independent variables stored in columns}
  \item{y}{Vector or matrix with dependent variable}
  \item{ndir}{Number of random directions used when depth is approximated.}
  }
\details{

}
\value{
Returns computed regression depth.
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
 
 lmfit = lm(Reserves~Income,data = pension)$coef
 deepfit = deepreg2d(pension[,1],pension[,2])$coef
 trimmfit =  trimprojreg2d(pension[,1],pension[,2])
 
 abline(lmfit, lty = 3, lwd = 2) #lm
 abline(deepfit, lwd = 2) #deepreg2d
 abline(trimmfit, lty = 2, lwd = 2) #trimprojreg2d
 
 tp = as.matrix(pension)
 
 mregdepth(lmfit, tp[,1], tp[,2], ndir = 1000)
 mregdepth(deepfit, tp[,1], tp[,2], ndir = 1000)
 mregdepth(trimmfit, tp[,1], tp[,2], ndir = 1000)
}
