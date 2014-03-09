\name{runifsphere}
\alias{runifsphere}
\title{Random number generation from unit sphere.}
\usage{
runifsphere(n, p = 2, seed = -1)
}
\arguments{
  \item{n}{number of random samples.}

  \item{p}{dimension of the unit sphere.}

  \item{seed}{seed for C++ random number generator.}
}
\description{
This function generates random numbers from p-dimensional
unit sphere.
}
\details{
seed is passed to function std::srand(seed); Value -1 means
that seed is not set in c++ code.

This function uses function randn() from Armadillo C++
Library.
}
\examples{
x = runifsphere(n=100)
 plot(x)
}
\author{
Daniel Kosiorowski, Mateusz Bocian, Anna Wegrzynkiewicz and
Zygmunt Zawadzki from Cracow University of Economics.
}

