#ifndef UTILS_INCLUDES
#define UTILS_INCLUDES

#include "RobCovLibConfig.h"
#include "armadillo"

namespace Utils
{
	__declspec(dllexport) arma::mat runifsphere(size_t n, size_t p);

}

#endif