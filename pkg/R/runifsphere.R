#'@title Random number generation from unit sphere.
#'
#'  @description This function generates random numbers from p-dimensional unit sphere.
#'
#'  @param n number of random samples.
#'  @param p dimension of the unit sphere.
#'  @param seed seed for C++ random number generator.
#'
#'  @details 
#'  
#'  seed is passed to function std::srand(seed); Value -1 means that seed is not set in c++ code.
#'  
#'  This function uses function randn() from Armadillo C++ Library. 
#'
#'  @examples
#'  
#'  x = runifsphere(n=100)
#'  plot(x)


runifsphere = function(n, p = 2, seed = -1)
{
  runifsphereCPP(n,p,seed)
}