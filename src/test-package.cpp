#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector convolve_density(NumericVector f1, NumericVector f2) {
  int n = f1.size();
  NumericVector result(2 * n - 1);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      result[i + j] += f1[i] * f2[j];
    }
  }
  return result;
}
