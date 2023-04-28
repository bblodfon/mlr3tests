library(mlr3verse)
library(tidyverse)
library(mlr3mbo) # needs mlr3learners, paradox and DiceKriging and rgenoud
set.seed(42)

# some further info on surrogate used
# https://github.com/mlr-org/mlr3mbo/issues/77

lgr::get_logger('bbotk')$set_threshold('warn')

task = tsk("pima")

# 2 hyperconfigs
learner = lrn("classif.rpart",
  minsplit = to_tune(1, 100),
  cp = to_tune(lower = 1e-04, upper = 0.1, logscale = TRUE))

# TuningInstanceSingleCrit ----
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals= 50)
)
default_surrogate(instance)

tuner = tnr("mbo")
tuner$optimize(instance)

tuner$acq_function$surrogate$model$model # DiceKriging => Gaussian Process
tuner$acq_function$surrogate$model$encapsulate
tuner$acq_function$surrogate$model$errors
tuner$acq_function$surrogate$model$fallback # Random Forest

instance$archive$data %>% as_tibble() %>% arrange(classif.ce)
instance$result
inst = instance$clone(deep = TRUE)
instance$clear()

autoplot(inst, type = 'performance')
autoplot(inst, type = 'surface', trafo = TRUE)

# compare with random search
tuner2 = tnr("random_search")

tuner2$optimize(instance)
autoplot(instance, type = 'surface', trafo = TRUE)

# tune() ----
# DONT USE THIS
instance = tune(
  method = "mbo",
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 15
)

autoplot(instance, type = 'surface', trafo = TRUE)

# AutoTuner() ----
at = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 50),
  tuner = tnr("mbo")
)

at$train(task, row_ids = 1:200)

# how much time did it took?
at$timings['train']

at$archive$data %>% as_tibble() %>% arrange(classif.ce)
at$learner # best learner, ready for prediction on a new test dataset

autoplot(at$tuning_instance, type = 'surface', trafo = TRUE)
autoplot(at$tuning_instance, type = 'marginal', cols_x = 'x_domain_cp')
autoplot(at$tuning_instance, type = 'marginal', cols_x = 'x_domain_minsplit')
autoplot(at$tuning_instance, type = 'parallel')
