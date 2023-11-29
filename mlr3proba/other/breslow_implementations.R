# C060 ----
#' response is `Surv()`
basesurv <- function (response, lp, times.eval = NULL) {
  if (is.null(times.eval)) times.eval <- sort(unique(response[,1]))

  t.unique <- sort(unique(response[,1][response[,2] == 1]))
  alpha    <- length(t.unique)

  for (i in 1:length(t.unique)) {
    alpha[i] <- sum(response[,1][response[,2] == 1] == t.unique[i])/sum(exp(lp[response[,1] >=  t.unique[i]]))
  }

  obj   <- approx(t.unique, cumsum(alpha), yleft=0, xout = times.eval, rule=2)

  obj$z <- exp(-obj$y)

  names(obj) <- c("times","cumBaseHaz","BaseSurv")
  return(obj)
}

predictProb = function (object, response, x, times, complexity,  ...) {
  # lp_test
  lp       <- as.numeric(predict(object, newx=data.matrix(x),s=complexity, type="link"))
  # times in baseline hazard
  basesurv <- basesurv(object$response, object$linear.predictor, sort(unique(times)))
  p        <- exp(exp(lp) %*% -t(basesurv$cumBaseHaz))

  if (NROW(p) != NROW(x) || NCOL(p) != length(times))
    stop("Prediction failed")
  p
}

# gbm ----
#' Baseline hazard function
#'
#' Computes the Breslow estimator of the baseline hazard function for a
#' proportional hazard regression model.
#'
#' The proportional hazard model assumes h(t|x)=lambda(t)*exp(f(x)).
#' \code{\link{gbm}} can estimate the f(x) component via partial likelihood.
#' After estimating f(x), \code{basehaz.gbm} can compute the a nonparametric
#' estimate of lambda(t).
#'
#' @param t The survival times.
#' @param delta The censoring indicator.
#' @param f.x The predicted values of the regression model on the log hazard
#'   scale.
#' @param t.eval Values at which the baseline hazard will be evaluated.
#' @param smooth If \code{TRUE} \code{basehaz.gbm} will smooth the estimated
#'   baseline hazard using Friedman's super smoother \code{\link{supsmu}}.
#' @param cumulative If \code{TRUE} the cumulative survival function will be
#'   computed.
#' @return A vector of length equal to the length of t (or of length
#'   \code{t.eval} if \code{t.eval} is not \code{NULL}) containing the baseline
#'   hazard evaluated at t (or at \code{t.eval} if \code{t.eval} is not
#'  \code{NULL}). If \code{cumulative} is set to \code{TRUE} then the returned
#'   vector evaluates the cumulative hazard function at those values.
#' @author Greg Ridgeway \email{gregridgeway@@gmail.com}
#' @seealso \code{\link[survival]{survfit}}, \code{\link{gbm}}
#' @references
#' N. Breslow (1972). "Discussion of `Regression Models and
#' Life-Tables' by D.R. Cox," Journal of the Royal Statistical Society, Series
#' B, 34(2):216-217.
#'
#' N. Breslow (1974). "Covariance analysis of censored survival data,"
#' Biometrics 30:89-99.
#' @keywords methods survival
#' @export
#' `t` => times (train data)
#' `delta` => status (train data)
#' `f.x` => lp (train data)
#' `t.eval` => if NULL uses train times `t`
#' always `cumulative = TRUE`!!!
basehaz.gbm <- function(t, delta, f.x, t.eval = NULL, smooth = FALSE,
  cumulative = TRUE) {
  # check: t, delta, f.x same length

  t.unique <- sort(unique(t[delta==1]))
  alpha <- length(t.unique) # wrong
  # alpha <- numeric(length(t.unique)) # correct
  for(i in 1:length(t.unique)) {
    #browser()
    alpha[i] <- sum(t[delta==1]==t.unique[i])/
      sum(exp(f.x[t>=t.unique[i]]))
  }

  if(!smooth && !cumulative) {
    if(!is.null(t.eval)) {
      stop("Cannot evaluate unsmoothed baseline hazard at t.eval.")
    }
  } else {
    if(smooth && !cumulative) {
      lambda.smooth <- supsmu(t.unique,alpha)
    } else {
      if(smooth && cumulative)
      {
        lambda.smooth <- supsmu(t.unique, cumsum(alpha))
      } else {  # (!smooth && cumulative) - THE DEFAULT
        lambda.smooth <- list(x = t.unique, y = cumsum(alpha))
      }
    }
  }

  # gbm way
  # obj <- if(!is.null(t.eval)) {
  #   approx(lambda.smooth$x, lambda.smooth$y, xout = t.eval)$y
  # } else {
  #   approx(lambda.smooth$x, lambda.smooth$y, xout = t)$y
  # }

  # C060 way
  # yleft=0, xout = times.eval, rule=2)
  obj <- if(!is.null(t.eval)) {
    approx(lambda.smooth$x, lambda.smooth$y, yleft = 0, xout = t.eval, rule = 2)$y
  } else {
    approx(lambda.smooth$x, lambda.smooth$y, yleft = 0, xout = t, rule = 2)$y
  }

  return(obj)
}

# xgboost.surv (jeager) ----
# https://github.com/bcjaeger/xgboost.surv/blob/master/R/predict.R
#' Baseline Hazard
#'
#' @description This function is a wrapper for the
#'   [gbm::basehaz.gbm()]. The function
#'   computes the Breslow estimator of the baseline
#'   hazard function for a proportional hazard
#'   regression model.
#'
#' @param sgb_booster an object of class `sgb_booster` (see [sgb_fit]).
#'
#' @param eval_times Values at which the baseline hazard will
#'   be evaluated.
#'
#' @param smooth If `TRUE` `sgb_bhaz` will smooth the estimated
#'   baseline hazard using Friedman's super smoother
#'   [supsmu][stats::supsmu()].
#'
#' @param cumulative If `TRUE` the cumulative survival function
#'   will be computed.
#'
#' @return A vector with a baseline hazard value corresponding to each
#'   time in `eval_times`  If cumulative is `TRUE`, the returned vector
#'   evaluates the cumulative hazard function at those values.
#'
#' @export
#'

sgb_bhaz = function(sgb_booster, eval_times = NULL, smooth = FALSE, cumulative = TRUE){
  eval_times_okay <- all(diff(eval_times)>0)
  stopifnot(is.logical(smooth))
  stopifnot(is.logical(cumulative))
  stopifnot(is_sgb_booster(sgb_booster))

  if(!eval_times_okay){
    stop(
      "eval_times should be a monotonically increasing sequence", call.=FALSE
    )
  }

  gbm::basehaz.gbm(
    t = get_time(sgb_booster$label),
    delta = get_status(sgb_booster$label),
    f.x = sgb_booster$training_predictions,
    t.eval = eval_times,
    smooth = smooth,
    cumulative = cumulative
  )

}


#' Boosting Predictions
#'
#' @param object a `sgb_booster` object
#' @param new_data data to compute predictions for.
#' @param eval_times numeric vector of times to compute survival probabilities.
#' @param ... Additional arguments passed to other functions.
#'   - **smooth:** if `TRUE`, smooth the estimated baseline hazard
#'   using Friedman's super smoother [supsmu][stats::supsmu].
#'   - **ntreelimit:** limit the number of model's trees or boosting
#'   iterations used in prediction. If unspecified, all trees will be used.
#'
#' @return a `matrix` with number of columns equal to the number of
#'  `eval_times` and number of rows equal to the number of rows in
#'  `new_data`. Additionally, `eval_times` are attached to the output
#'  as an attribute.
#' @export
#'
#' @examples
#'
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' s  <- as.numeric(x1 + x2 + rnorm(100) > 0)
#' t  <- runif(100, min=1, max=10)
#'
#' df = data.frame(time=t, status=s, x1=x1, x2=x2)
#'
#' df = as_sgb_data(df, time=time, status=status)
#'
#' sgb_booster <- sgb_fit(
#'   sgb_df = df,
#'   params = sgb_params(max_depth=1),
#'   nrounds = 10,
#'   verbose = FALSE
#' )
#'
#' sgb_probs <- predict(sgb_booster, new_data = df)
#'
#'
predict.sgb_booster <- function(object, new_data, eval_times = NULL, ...){

  .dots <- list(...) %>%
    check_dots(valid_args = c('smooth', 'ntreelimit'))

  smooth <- if('smooth' %in% names(.dots))
    .dots$smooth
  else
    FALSE

  ntreelimit <- if('ntreelimit' %in% names(.dots))
    .dots$ntreelimit
  else
    NULL

  #' if `eval_times = NULL` returns the second input, otherwise the first
  eval_times <- eval_times %||% object$eval_times

  base_haz <- sgb_bhaz(
    sgb_booster = object,
    eval_times = eval_times, # from trained model or new
    smooth = smooth,
    cumulative = TRUE
  )

  predictions <- stats::predict(
    object = object$fit,
    newdata = if (is_sgb_data(new_data)) new_data$data else new_data,
    outputmargin = TRUE,
    ntreelimit = ntreelimit
  )

  prb <- matrix(
    data = NA_real_,
    nrow = length(predictions),
    ncol = length(eval_times)
  )

  for( i in seq_along(base_haz) ) {
    prb[,i] <- exp(-exp(predictions) * (base_haz[i]))
  }

  attr(prb, 'eval_times') <- eval_times
  #class(prb) <- c('sgb_probs', 'matrix')

  prb
}

# example ----
library(mlr3proba)
task = tsk("rats")
part = partition(task, ratio = 0.8)
length(part$train) # 240
length(part$test) # 60
l = lrn("surv.coxph")
truth_train = task$truth(part$train)
truth_test  = task$truth(part$test)
l$train(task, part$train)

p_train = l$predict(task, part$train)
p_test  = l$predict(task, part$test)

# lp same
expect_equal(unname(p_train$lp), l$model$linear.predictors)
eval_times  = sort(unique(truth_train[,1])) # from TRAIN set, unique and ordered

# testing stuff
H0 = basehaz.gbm(t = truth_train[,1], delta = truth_train[,2], f.x = p_train$lp,
  t.eval = eval_times) # smooth = FALSE, cumulative = TRUE

test_lp = p_test$lp # lp predictions from test set

prb = matrix(
  data = NA_real_,
  nrow = length(test_lp),
  ncol = length(eval_times)
)

# xgboost.surv way
for (i in seq_along(H0)) {
  prb[,i] <- exp(-exp(test_lp) * (H0[i]))
}
colnames(prb) = eval_times
rownames(prb) = 1:nrow(prb)

dim(prb)
dim(p_test$data$distr)
expect_equal(prb, p_test$data$distr) # nop! average diff: 0.00253
expect_equal(prb, p_test$data$distr, tolerance = 0.003) # okay

prb2 = exp(exp(test_lp) %*% -t(H0)) # MUCH FASTER than xgboost.surv for-loop!!!
colnames(prb2) = eval_times
rownames(prb2) = 1:nrow(prb2)
dim(prb2)

all(prb2 == prb) # TRUE, same!