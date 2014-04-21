\name{trimProjReg2d}
\alias{trimProjReg2d}
\title{trimProjReg2d}
\usage{
trimProjReg2d(x, y, alpha = 0.1)
}
\arguments{
  \item{x}{Independent variable}

  \item{y}{Dependent variable}

  \item{alpha}{Percentage of trimmed observations}
}
\description{
Computes projection trimmed regression in 2 dimensions.
}
\examples{
data(pension)
 plot(pension)
 abline(lm(Reserves~Income,data = pension), lty = 3, lwd = 2) #lm
 abline(trimProjReg2d(pension[,1],pension[,2]), lwd = 2) #trimprojreg2d
 legend("bottomright", c("OLS","TrimLS"), lty = 1:2)
}
\author{
Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and
Zygmunt Zawadzki from Cracow University of Economics.
}

