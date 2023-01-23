library(mlr3verse)
library(mlr3proba)
library(mlr3misc)
library(mlr3extralearners)

lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

# OOB-Measure that works with Survival prediction types
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
        predict_type = "crank", # every surv learner has this
        range = c(-Inf, Inf),
        minimize = TRUE,
        label = "Out-of-bag Error"
        #man = "mlr3::mlr_measures_oob_error"
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

l = lrn("surv.ranger", num.threads = 8)
t = tsk("rats")
l$train(t)
p = l$predict(t)
m = msr("surv.oob_error")
m
p$score(m, learner = l)
l$oob_error() # same, ok!

# Tuning example
ps = paradox::ps(
  num.trees = p_int(10, 50),
  mtry.ratio = p_dbl(0.1, 0.9),
  min.node.size = p_int(1, 20)
)

rsf_at = AutoTuner$new(
  learner = l,
  search_space = ps,
  resampling = rsmp('insample'), # TRAIN == TEST
  measure = msr('surv.oob_error'),
  terminator = trm('evals', n_evals = 10),
  tuner = tnr('random_search'),
  store_models = TRUE # needs this, otherwise it fails!
)
rsf_at$train(t, row_ids = 1:250)
rsf_at$tuning_instance
rsf_at$archive$best()
1-rsf_at$learner$oob_error()
p2 = rsf_at$predict(t, row_ids = 251:300)
p2$score()
