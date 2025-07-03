library(mlr3proba)
library(R6)
library(paradox)

# Functions and classes copied from mlr3proba@0.8.1

# S(t)/f(t) ESTIMATION/INTERPOLATION FUNCTIONS

# Linearly interpolate (and extrapolate) a survival curve at arbitrary time points.
# @param surv_data `survfit` object or a `list` with 2 elemnts:
# `surv` (survival probabilities) and corresponding `time` (time points)
# @param eval_times vector of times (unordered, possibly duplicated)
# @param method type of interpolation to use - `linear` (default) or `constant`
# @return interpolated S(t) values
.interp_surv = function(surv_data, eval_times, method = "linear") {
  checkmate::assert_choice(method, c("linear", "constant"))

  # constant interpolation is easy
  if (method == "constant") {
    surv = surv_data$surv
    times = surv_data$time

    return(stats::approx(x = times, y = surv, xout = eval_times, yleft = 1,
                         method = "constant", rule = 2)$y)
  }

  # remove constant-interpolated values from S(t)
  unique_surv_idx = !duplicated(surv_data$surv)
  surv = surv_data$surv[unique_surv_idx] # decreasing
  times = surv_data$time[unique_surv_idx] # ordered

  # Edge case: constant survival
  if (all(surv == surv[1])) {
    return(rep(surv[1], length(eval_times)))
  }

  # linear interpolation (at least two S(t) values here)
  interp_surv = stats::approx(x = times, y = surv, xout = eval_times,
                              yleft = 1, method = "linear", rule = 2)$y

  # Extrapolate manually if needed
  min_time = min(times)
  max_time = max(times)

  # Precompute slopes for extrapolation
  slope_left = (surv[1L] - 1) / min_time
  slope_right = (surv[length(surv)] - surv[length(surv) - 1L]) / (max_time - times[length(times) - 1L])

  idx_left = eval_times < min_time
  idx_right = eval_times > max_time

  # Linear extrapolation considering that S(t = 0) = 1
  if (any(idx_left) && surv[1L] < 1) {
    interp_surv[idx_left] = 1 + slope_left * eval_times[idx_left]
  }

  # Linear extrapolation using the last time interval
  if (any(idx_right) && surv[length(surv)] > 0) {
    extrap_value = surv[length(surv)] + slope_right * (eval_times[idx_right] - max_time)
    interp_surv[idx_right] = pmax(0, extrap_value) # force S >= 0
  }

  interp_surv
}

# PDF estimation from a survival curve
# @param surv_data `survfit` object or a `list` with 2 elemnts:
# `surv` (survival probabilities) and corresponding `time` (time points)
# @param eval_times numeric vector (unordered, duplicated allowed)
# @return numeric vector of density values f(t)
.interp_pdf = function(surv_data, eval_times) {
  # keep all unique sorted times (predicted and requested) for pdf
  utimes = sort(unique(c(surv_data$time, eval_times)))

  # Create a mapping of `eval_times` to `utimes`
  indx = match(eval_times, utimes)

  # Linearly interpolate survival function (to avoid pdf = 0 problems)
  surv = .interp_surv(surv_data, utimes, method = "linear")

  # CDF = 1 - S
  cdf = 1 - surv

  # Numerical derivative: f = dF/dt = -dS/dt
  dt = diff(utimes)
  dF = diff(cdf)

  # Density (finite difference)
  dens = dF / dt

  # For timepoints exactly at utimes, align left
  dens_full = c(dens[1], dens) # replicate first slope for first point

  # return density at `eval_times`, clip any negatives to 0
  pmax(dens_full[indx], 0)
}

# RCLL with linear interpolation of S(t)
MeasureSurvRCLLInterp = R6Class("MeasureSurvRCLLInterp",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(eps = p_dbl(0, 1, default = 1e-6))
      ps$set_values(eps = 1e-6)

      super$initialize(
        id = "surv.rclli",
        minimize = TRUE,
        predict_type = "distr",
        label = "Right-Censored Log-Likelihood",
        man = "mlr3proba::mlr_measures_surv.rclli",
        range = c(0, Inf),
        param_set = ps
      )

      invisible(self)
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      pv = self$param_set$values

      truth = prediction$truth
      n_obs = length(truth)
      test_times = truth[, 1L]
      test_status = truth[, 2L]

      # get survival matrix
      surv_mat = prediction$data$distr
      pred_times = as.numeric(colnames(surv_mat))

      res = vapply(seq_len(n_obs), function(obs_index) {
        # event time or censoring time
        outcome_time = test_times[obs_index]

        # predicted survival curve for observation
        surv_pred = list(surv = surv_mat[obs_index, ], time = pred_times)

        if (test_status[obs_index] == 1) {
          # event => use f(t)
          .interp_pdf(surv_pred, outcome_time)
        } else {
          # censored => use S(t)
          .interp_surv(surv_pred, outcome_time)
        }
      }, numeric(1))

      mean(-log(pmax(pv$eps, res)))
    }
  )
)

MeasureSurvLoglossInterp = R6Class("MeasureSurvLoglossInterp",
  inherit = MeasureSurv,
  public = list(
   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
     ps = ps(eps = p_dbl(0, 1, default = 1e-6))
     ps$set_values(eps = 1e-6)

     super$initialize(
       id = "surv.loglossi",
       range = c(0, Inf),
       minimize = TRUE,
       predict_type = "distr",
       label = "Negative Log-Likelihood",
       man = "mlr3proba::mlr_measures_surv.loglossi",
       param_set = ps
     )

     invisible(self)
   }
  ),

  private = list(
   .score = function(prediction, task, train_set, ...) {
     pv = self$param_set$values

     truth = prediction$truth
     n_obs = length(truth)
     test_times = truth[, 1L]

     # get survival matrix
     surv_mat = prediction$data$distr
     pred_times = as.numeric(colnames(surv_mat))

     res = vapply(seq_len(n_obs), function(obs_index) {
       # event time or censoring time
       outcome_time = test_times[obs_index]

       # predicted survival curve for observation
       surv_pred = list(surv = surv_mat[obs_index, ], time = pred_times)

       # predicted pdf at observed time
       .interp_pdf(surv_pred, outcome_time)
     }, numeric(1))

     mean(-log(pmax(pv$eps, res)))
   }
  )
)

# Add measure for easy use with `msr()`
mlr_measures = utils::getFromNamespace("mlr_measures", ns = "mlr3")
mlr_measures$add("surv.rclli", MeasureSurvRCLLInterp)
mlr_measures$add("surv.loglossi", MeasureSurvLoglossInterp)

# simple test on a BenchmarkResult
bmr = readRDS("mlr3proba/other/meas.rds")
res = bmr$score(measures = msrs(c("surv.loglossi", "surv.rclli",
                                  "surv.cindex", "surv.graf")))
