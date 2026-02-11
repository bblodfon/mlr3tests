# Survival data simulation functions
# NOTE: PH is fine and easy, rest I have written better over time, but should still be fine for
# quickly checking things
library(data.table)
library(checkmate)

# helper functions
# finds lambda_C numerically (exponential distributions for C and T assumed)
lambdaC_exact = function(lambdaT, target_r) {
  f = function(lambdaC) {
    mean(lambdaC / (lambdaC + lambdaT)) - target_r
  }
  uniroot(f, interval = c(1e-8, 1e4))$root
}

#' Simulate survival data with proportional hazards
#'
#' Generates survival data following a Cox proportional hazards model.
#' The hazard is: h(t|X) = h0(t) * exp(X * beta)
#'
#' @param n Sample size
#' @param p Number of features
#' @param censoring_rate Target censoring rate
#' @param n_informative Number of informative features. By default the first
#'  `n_informative` features are chosen to be informative, the rest are assigned
#'  zero coefficients.
#' @param beta_strength Coefficient strength (controls effect size)
#' @return data.table with features, time, and status
sim_ph = function(n = 500, p = 10, censoring_rate = 0.3, n_informative = 5, beta_strength = 1) {
  n = assert_numeric(as.integer(n), lower = 1)
  p = assert_numeric(as.integer(p), lower = 1)

  # Generate features from standard normal
  X = matrix(rnorm(n * p), nrow = n, ncol = p)

  # True coefficients
  n_informative = assert_numeric(as.integer(n_informative), lower = 1, upper = p)
  beta = c(
    seq(beta_strength, -beta_strength, length.out = n_informative),
    rep(0, p - n_informative)
  )

  # Name features: add "_inf" suffix for the informative ones
  if (n_informative < p) {
    feature_names = c(
      paste0("x", seq_len(n_informative), "_inf"),  # informative features
      paste0("x", (n_informative + 1):p)            # noise features
    )
    colnames(X) = feature_names
  } else {
    colnames(X) = paste0("x", seq_len(n_informative), "_inf")
  }

  # Linear predictor
  eta = X %*% beta |> as.vector()

  # Generate true survival times from exponential distr. Subject-specific rate,
  # Larger eta => larger rate => shorter expected time.
  lambdaT = exp(eta) # lp_i = lambda_Ti
  time = rexp(n, rate = lambdaT) # T_i ~ Exp(rate = lp_i)

  # Censoring times from exponential to achieve desired censoring rate
  # expected censoring rate c = P(C<T) = lambda_C / (lambda_C + lambda_T) = E[T] / (E[T] + E(C))
  # For heterogeneous lambda_T, we use the approximation
  # lambda_C ≈ c / ((1-c) * mean_event_time), where mean_event_time = mean(T_i).
  # mean_event_time = mean(time) # E[T] = 1 / lambda_T
  # lambdaC = censoring_rate / ((1 - censoring_rate) * mean_event_time)
  # censoring_time = rexp(n, rate = lambdaC) # C ~ Exp(lambda_C)

  # more accurate way to get the censoring times for a specified censoring rate
  lambdaC = lambdaC_exact(lambdaT, censoring_rate)
  censoring_time = rexp(n, rate = lambdaC)

  # Observed time and status
  observed_time = pmin(time, censoring_time) # T <= C
  status = as.numeric(time <= censoring_time) # 1 = event observed, 0 = censored

  # Combine into data.table
  dt = data.table::data.table(
    time = observed_time,
    event = status,
    X
  )

  dt
}

#' Simulate survival data with non-proportional hazards
#'
#' Generates survival data where the proportional hazards assumption is violated.
#' Uses time-varying coefficients: beta(t) = beta0 * (1 + gamma * log(t))
#' Survival times are generated via inverse transform sampling on a discretized
#' cumulative hazard, and censoring is applied to achieve the target rate.
#'
#' @param n Sample size
#' @param p Number of features
#' @param censoring_rate Target censoring rate
#' @param n_informative Number of informative features. By default the first
#'  `n_informative` features are chosen to be informative, the rest are assigned
#'  zero coefficients.
#' @param beta_strength Coefficient strength. Determines the magnitude of the effect
#'  of the informative features on the hazard.
#' @param time_varying_strength Strength of time-varying effect (gamma).
#'  0 = PH, >0 = non-PH.
#' @param max_time Maximum follow-up time for discretization. Subjects who do not
#'  reach the event before `max_time` are assigned `time = max_time`.
#' @param time_step Time step for discretizing the cumulative hazard. Smaller values
#'  give more precise simulation at the cost of speed.
#' @return data.table with features, time, and status
sim_nph = function(n = 500, p = 10, censoring_rate = 0.3, n_informative = 5,
                   beta_strength = 1, time_varying_strength = 0.5, max_time = 10,
                   time_step = 0.01) {
  n = assert_numeric(as.integer(n), lower = 1)
  p = assert_numeric(as.integer(p), lower = 1)

  # Generate features
  X = matrix(rnorm(n * p), nrow = n, ncol = p)

  # True coefficients
  n_informative = assert_numeric(as.integer(n_informative), lower = 1, upper = p)
  beta = c(
    seq(beta_strength, -beta_strength, length.out = n_informative),
    rep(0, p - n_informative)
  )

  # Name features: add "_inf" suffix for the informative ones
  if (n_informative < p) {
    feature_names = c(
      paste0("x", seq_len(n_informative), "_inf"),  # informative features
      paste0("x", (n_informative + 1):p)            # noise features
    )
    colnames(X) = feature_names
  } else {
    colnames(X) = paste0("x", seq_len(n_informative), "_inf")
  }

  # Simulate survival times using inverse transform sampling with time-varying hazard
  # This is an approximation using discretization
  time_grid = seq(0, max_time, by = time_step)

  U = runif(n)
  cumhaz_mat = matrix(0, nrow = n, ncol = length(time_grid))
  for (j in seq_along(time_grid)[-1]) {
    t = time_grid[j]
    beta_t = beta * (1 + time_varying_strength * log(t + 1))
    eta = X %*% beta_t
    cumhaz_mat[, j] = cumhaz_mat[, j-1] + exp(eta) * time_step
  }
  # find first t where cumhaz >= -log(U) for each subject
  time = numeric(n)
  for (i in seq_len(n)) {
    t_idx = which(cumhaz_mat[i, ] >= -log(U[i]))[1]
    if (is.na(t_idx)) time[i] = max_time else time[i] = time_grid[t_idx]
  }

  # Compute subject-specific hazard rates
  lambdaT = 1 / pmax(time, 1e-8) # Ti ~ Exp(1 / Ti_mean)
  # find lambda_C numerically to get target censoring rate
  lambdaC = lambdaC_exact(lambdaT, censoring_rate)
  censoring_time = rexp(n, rate = lambdaC)

  # Observed time and status
  observed_time = pmin(time, censoring_time) # T <= C
  status = as.numeric(time <= censoring_time) # 1 = event observed, 0 = censored

  # Combine into data.table
  dt = data.table::data.table(
    time = observed_time,
    event = status,
    X
  )

  dt
}

#' Simulate survival data from a Weibull AFT model with interaction
#'
#' Survival times follow a Weibull AFT model:
#'   log(T) = (1/shape) * (log(scale) + noise)
#'
#' where:
#'   scale = exp(-eta / shape)
#'   eta = beta1 * x1 + beta2 * x2 + interaction_strength * x1 * x2
#'
#' The shape parameter varies by x2 (non-proportional hazards). Censoring can be
#' either random exponential or administratively capped, and optionally computed
#' to achieve a desired censoring proportion.
#'
#' @param n Sample size
#' @param beta1 Effect of x1
#' @param beta2 Effect of x2
#' @param interaction_strength Coefficient for x1*x2 interaction (default: no interaction)
#' @param max_followup Numeric. Maximum follow-up time; censoring times above this are truncated
#' @param censoring_rate Target censoring proportion (0-1). If NULL, lambda_C is used directly.
#' @param lambda_C Exponential censoring rate (ignored if `censoring_rate` is provided.
#' @param shape_x2_1 Numeric. Weibull shape for x2 = 1
#' @param shape_x2_0 Numeric. Weibull shape for x2 = 0
#' @return data.table with columns: id, x1, x2, x3, time, event
#' @return data.table with columns:
#'   - id: subject identifier
#'   - x1, x2, x3: covariates
#'   - time: observed survival or censoring time
#'   - status: event indicator (1 = event, 0 = censored)
sim_weib = function(
    n = 500,
    beta1 = 1,
    beta2 = -0.5,
    interaction_strength = 0,
    max_followup = Inf,
    censoring_rate = NULL,
    lambda_C = 0.1,
    shape_x2_1 = 1.0,
    shape_x2_0 = 3.0
  ) {
  dt = data.table::data.table(
    id = 1:n,
    time = NA,
    status = NA,
    x1 = rnorm(n, 0, 1), # normal continuous
    x2 = rbinom(n, 1, 0.5), # bernouli(0.5)
    x3 = runif(n, -1, 1) # noise
  )

  # ---- Linear predictor and Weibull parameters ---------------------------------
  dt[, eta := beta1 * x1 + beta2 * x2 + interaction_strength * x1 * x2]
  dt[, shape := ifelse(x2 == 1, shape_x2_1, shape_x2_0)]
  dt[, scale := exp(-eta / shape)]  # AFT parametrization

  # ---- Simulate survival times --------------------------------------------------
  u = runif(n) # use inverse-transform uniform sampling
  dt[, survival_time := scale * (-log(u))^(1 / shape)]

  # ---- Determine lambda_C to target censoring proportion -----------------------
  if (!is.null(censoring_rate)) {
    # Numerical root-finding to get lambda_C such that mean(event indicator) ~ 1 - censoring_rate
    target_fn = function(lc) {
      c_times = rexp(n, rate = lc)
      if (is.finite(max_followup)) c_times = pmin(c_times, max_followup)
      mean(dt$survival_time <= c_times) - (1 - censoring_rate)
    }
    # Use uniroot to solve for lambda_C
    lambda_C = uniroot(target_fn, lower = 1e-6, upper = 100)$root
  }

  # ---- Generate censoring times -------------------------------------------------
  censoring_time = rexp(n, rate = lambda_C)
  if (is.finite(max_followup)) censoring_time = pmin(censoring_time, max_followup)

  # ---- Observed times and event indicator --------------------------------------
  dt[, time := pmin(survival_time, censoring_time)]
  dt[, event := as.integer(survival_time <= censoring_time)]

  # ---- Return final data --------------------------------------------------------
  dt[, .(id, x1, x2, x3, time, event)]
}
