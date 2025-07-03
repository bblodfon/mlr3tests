#include <Rcpp.h>
using namespace Rcpp;

// distr6 function
// [[Rcpp::export]]
NumericVector C_EmpiricalMVCdf(NumericMatrix x, NumericMatrix data) {

  float n = data.nrow();
  float vbj;
  int vk;
  NumericVector ret(x.nrow());

  for (int i = 0; i < x.nrow(); i++) {
    for (int k = 0; k < n; k++) {
      vbj = 0;
      vk = 1;

      for (int j = 0; j < x.ncol(); j++) {
        vk = vk && (data(k, j) <=  x(i, j));
      }

      vbj += vk;

      ret[i] += (vbj/n);
    }
  }

  return ret;
}

// optimized via chatGTP
// [[Rcpp::export]]
NumericVector C_EmpiricalMVCdf(NumericMatrix x, NumericMatrix data) {
  // x => test, data => train
  int n = data.nrow();
  int m = x.nrow();
  int d = x.ncol();
  
  NumericVector ret(m, 0.0);  // Initialize return vector with zeros
  
  for (int i = 0; i < m; i++) {
    int count = 0; // Count of valid training samples
    
    for (int k = 0; k < n; k++) {
      bool isValid = true;  // Flag to track if row k is valid
      
      for (int j = 0; j < d; j++) {
        if (data(k, j) > x(i, j)) {
          isValid = false;
          break;  // Early exit (short-circuiting)
        }
      }
      
      if (isValid) count++; // Count only valid samples
    }
    
    ret[i] = static_cast<double>(count) / n; // Normalize to probability
  }
  
  return ret;
}


# Example Data
x_train <- matrix(c(0.2, 0.4, 0.1,
                    0.5, 0.7, 0.3,
                    0.8, 0.6, 0.2,
                    0.3, 0.5, 0.9,
                    0.9, 0.1, 0.4), ncol = 3, byrow = TRUE)

x_test <- matrix(c(0.5, 0.6, 0.2,
                   0.7, 0.5, 0.3), ncol = 3, byrow = TRUE)

# Compute empirical multivariate CDF
C_EmpiricalMVCdf(x_test, x_train)

