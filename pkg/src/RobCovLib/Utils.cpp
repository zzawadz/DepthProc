#include "Utils.h"

namespace Utils
{
	arma::mat runifsphere(size_t n, size_t p)
	{
		arma::mat X(n, p);
		X.randn();
		arma::vec norm = arma::sum(X % X, 1);
		norm = arma::sqrt(norm);
		X.each_col() /= norm;
		return X;
	}

}