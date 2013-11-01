#include "Utils.h"
#include <limits>

namespace Utils
{
	arma::mat runifsphere(size_t n, size_t p, int seed = -1)
	{
		if(seed >= 0) std::srand(seed);

		arma::mat X(n, p);
		X.randn();
		arma::vec norm = arma::sum(X % X, 1);
		norm = arma::sqrt(norm);
		X.each_col() /= norm;
		return X;
	}

}
