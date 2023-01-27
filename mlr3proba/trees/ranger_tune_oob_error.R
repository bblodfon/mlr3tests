library(mlr3verse)
library(mlr3proba)
library(survival)
library(ranger)
library(tidyverse)
library(survmob)
library(mlr3mbo)
library(mlr3misc)
library(progressr)

# Progress bars ----
options(progressr.enable = TRUE)
handlers(global = TRUE)
handlers('progress')

# Logging ----
lgr::get_logger("mlr3")$set_threshold("info")
lgr::get_logger("bbotk")$set_threshold("info")

# Task Lung ----
task = tsk('lung')
preprocess = po('encode', method = 'treatment') %>>% po('imputelearner', lrn('regr.rpart'))
task = preprocess$train(task)[[1]]
task$missings()

# RSF learner ----
s = SurvLPS$new(nthreads_rsf = 8, ids = 'rsf_logrank')
dt = s$lrn_tbl()
dt$learner[[1L]] # model
dt$param_set[[1L]] # parameter space for tuning

# Data Split ----
set.seed(42)
split = mlr3::partition(task, ratio = 0.8)

# CV-tuning (works always) ----
rsf_at = AutoTuner$new(
  learner = dt$learner[[1L]],
  search_space = dt$param_set[[1L]],
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'), # any measure here except `oob_error`
  terminator = trm('evals', n_evals = 40), # 10 - 100
  tuner = tnr('mbo')
  #tuner = tnr('random_search')
)
rsf_at$train(task, row_ids = split$train)
rsf_at$timings
as.data.table(rsf_at$archive)$surv.cindex # Mean-CV C-index results are less biased

rsf_at$archive$best()$x_domain
rsf_at$learner
rsf_at$learner$oob_error() # exists

rsf_at$learner$predict(task, row_ids = split$test)$score() # different

autoplot(rsf_at$tuning_instance, type = 'performance')

# Insample-tuning (works always) ----

## Overestimates performance
rsf_at2 = AutoTuner$new(
  learner = dt$learner[[1L]],
  search_space = dt$param_set[[1L]],
  resampling = rsmp('insample'), # TRAIN == TEST
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 10), # 10 - 100
  tuner = tnr('random_search')
)
rsf_at2$train(task, row_ids = split$train)

as.data.table(rsf_at2$archive)$surv.cindex # overestimates C-index
rsf_at2$archive$best()$x_domain
rsf_at2$learner
rsf_at2$learner$oob_error() # exists

1-rsf_at2$learner$predict(task, row_ids = split$test)$score()

autoplot(rsf_at2$tuning_instance, type = 'performance')

# OOB survival error (own) ----
#' DON'T USE THIS!!!
#' Made our own measure to tackle the following issues
#' https://github.com/mlr-org/mlr3proba/issues/275
#' https://github.com/mlr-org/mlr3/issues/816

#' @title Out-of-bag Survival Error Measure
#'
#' @name mlr_measures_oob_error
#' @include Measure.R
#'
#' @description
#' Returns the out-of-bag error of the [Learner] for learners that support it
#' (learners with property `"oob_error"`).
#' Returns `NA` for unsupported learners.
#'
#' @templateVar id oob_error
#' @template measure
#'
#' @template seealso_measure
#' @export
MeasureSurvOOBError = R6::R6Class("MeasureSurvOOBError",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.oob_error",
        task_type = NA_character_,
        properties = c("na_score", "requires_learner"),
        predict_type = "crank", # every survival learner should have this
        range = c(-Inf, Inf),
        minimize = TRUE,
        label = "Out-of-bag Error"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      learner = learner$base_learner()
      if ("oob_error" %nin% learner$properties) {
        return(NA_real_)
      }

      # whatever the learner supports (e.g. 1-Cindex or some kind of loss)
      return(learner$oob_error())
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("surv.oob_error", function() MeasureSurvOOBError$new())

## Tune ----
rsf_at3 = AutoTuner$new(
  learner = dt$learner[[1L]],
  search_space = dt$param_set[[1L]],
  resampling = rsmp('insample'), # TRAIN == TEST
  measure = msr('surv.oob_error'),
  #measure = MeasureSurvOOBError$new(),
  terminator = trm('evals', n_evals = 40), # 10 - 100
  #tuner = tnr('random_search'),
  tuner = tnr('mbo'),
  store_models = TRUE # You need this for the oob_error to work
)
rsf_at3$train(task, row_ids = split$train)
rsf_at3$timings
rsf_at3$tuning_instance

rsf_at3$learner$predict(task, row_ids = split$train)$score() # c-index
rsf_at3$learner$predict(task, row_ids = split$test)$score() # c-index
1 - rsf_at3$learner$predict(task, row_ids = split$test)$score() # as error

autoplot(rsf_at3$tuning_instance, type = 'performance')

# OOB error (fixed!!!) ----
rsf_at4 = AutoTuner$new(
  learner = dt$learner[[1L]],
  search_space = dt$param_set[[1L]],
  resampling = rsmp('insample'), # TRAIN == TEST
  measure = msr('oob_error'), # OOB error
  terminator = trm('evals', n_evals = 40),
  #tuner = tnr('random_search'),
  tuner = tnr('mbo'),
  store_models = TRUE # NEEDS THIS to get the oob_error!
)
rsf_at4$train(task, row_ids = split$train)
rsf_at4$tuning_result

1 - as.data.table(rsf_at4$archive)$oob_error # OOB C-index - seems pretty unbiased! YAY

rsf_at4$archive$best()$x_domain
rsf_at4$learner
rsf_at4$learner$oob_error() # exists

rsf_at4$learner$predict(task, row_ids = split$test)$score()

autoplot(rsf_at4$tuning_instance, type = 'performance')
