#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;

// [[Rcpp::export]]
double cpp_ordinal_loglik(const NumericMatrix& X,
                          const IntegerVector& y,
                          const NumericVector& beta,
                          const int n_classes) {
  if (n_classes != 2) {
    stop("cpp_ordinal_loglik currently supports 2 classes only.");
  }
  if (X.nrow() != y.size()) {
    stop("X and y have incompatible dimensions.");
  }
  if (X.ncol() != beta.size()) {
    stop("X and beta have incompatible dimensions.");
  }

  double total = 0.0;
  const int p = X.ncol();

  for (int i = 0; i < X.nrow(); ++i) {
    double eta = 0.0;
    for (int j = 0; j < p; ++j) {
      eta += X(i, j) * beta[j];
    }
    const double p1 = 1.0 / (1.0 + std::exp(-eta));
    const double p2 = 1.0 - p1;
    if (y[i] == 1) {
      total += std::log(std::max(p1, 1e-12));
    } else {
      total += std::log(std::max(p2, 1e-12));
    }
  }

  return total;
}
