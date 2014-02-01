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
seed is passed to function std::srand(seed);

This function uses function randn() from Armadillo C++
Library.
}
\examples{
x = runifsphere(n=100)
 plot(x)
}

