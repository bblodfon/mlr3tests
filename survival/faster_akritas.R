#include <Rcpp.h>
#include <algorithm>  // For binary search
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix C_Akritas(NumericMatrix truth, NumericVector predict_times,
                         NumericVector unique_times, NumericVector FX_train,
                         NumericVector FX_predict, double lambda) {

  int pred_length = FX_predict.size();
  int train_length = FX_train.size();
  int new_times_length = predict_times.size();
  int unique_times_length = unique_times.size();

  NumericMatrix surv(pred_length, new_times_length);

  // Sort FX_train once (IMPORTANT)
  NumericVector sorted_FX_train = clone(FX_train);
  std::sort(sorted_FX_train.begin(), sorted_FX_train.end());

  // Loop over test samples
  for (int n = 0; n < pred_length; n++) {
    double FXn = FX_predict[n];

    // Find range of training samples within lambda
    auto lower = std::lower_bound(sorted_FX_train.begin(), sorted_FX_train.end(), FXn - lambda);
    auto upper = std::upper_bound(sorted_FX_train.begin(), sorted_FX_train.end(), FXn + lambda);
    
    int start_idx = lower - sorted_FX_train.begin();
    int end_idx = upper - sorted_FX_train.begin();

    // Loop over prediction times
    for (int i = 0; i < new_times_length; i++) {
      double prod = 1.0;
      double current_time = predict_times[i];

      for (int t = 0; t < unique_times_length && unique_times[t] <= current_time; t++) {
        double unique_time = unique_times[t];
        double num = 0.0, den = 0.0;

        // Iterate only over relevant training points (not all of them)
        for (int l = start_idx; l < end_idx; l++) {
          double true_time = truth(l, 0);
          int true_status = truth(l, 1);

          if (true_time < unique_time) break; // Data is sorted

          num += (true_time == unique_time) * true_status;
          den += 1.0;
        }

        if (den > 0 && num > 0) {
          prod *= (1.0 - num / den);
        }
      }

      surv(n, i) = prod;
    }
  }

  return surv;
}

