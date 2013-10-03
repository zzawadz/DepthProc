\name{trimprojreg2d}
\alias{trimprojreg2d}
\title{
trimprojreg2d
}
\description{
Computes projection trimmed regression in 2 dimensions.
}
\usage{
trimprojreg2d(x, y, alpha = 0.1)
}

\arguments{
  \item{x}{Independent variable}
  \item{y}{Dependent variable}
  \item{alpha}{Percentage of trimmed observations}
}
\details{

}
\value{
Function returns regression parameters.
}
\references{

}

\author{Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and Zygmunt Zawadzki from Cracow University of Economics.}



\examples{
data(pension)
plot(pension)
abline(lm(Reserves~Income,data = pension), lty = 3, lwd = 2) #lm
abline(trimprojreg2d(pension[,1],pension[,2]), lwd = 2) #trimprojreg2d
}

