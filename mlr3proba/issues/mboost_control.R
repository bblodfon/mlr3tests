# NOT an issue it seems, I was tripping :)
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(tidyverse)
library(mlr3mbo)
library(survival)

# Logging (less)
# lgr::get_logger('bbotk')$set_threshold('warn')
# lgr::get_logger('mlr3')$set_threshold('warn')

# Veteran task ----
task = as_task_surv(x = survival::veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]
task

set.seed(42)
train_indxs = sample(seq_len(task$nrow), 100)
test_indxs  = setdiff(seq_len(task$nrow), train_indxs)

glmboost_lrn = lrn('surv.glmboost',
  family = to_tune(p_fct(c('coxph', 'weibull', 'loglog', 'lognormal',
    'gehan', 'cindex'))),
  sigma = to_tune(p_dbl(0.1, 0.5)),
  mstop = to_tune(p_int(50, 500)), # boosting iterations
  nu = to_tune(p_dbl(1e-04, 1, logscale = TRUE))
)

glmboost_at = AutoTuner$new(
  learner = glmboost_lrn,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 42), # 10 - 100
  tuner = tnr('mbo')
)
glmboost_at$train(task, train_indxs)

# check if chosen learner has the hps it should (NO!!!)
glmboost_at$learner$model
glmboost_at$tuning_result

# remake learner yourself to test if model is the same
learner = lrn('surv.glmboost')
parameters = glmboost_at$tuning_result$x_domain[[1L]]
parameters
learner$param_set$values =
  mlr3misc::insert_named(learner$param_set$values, parameters)
learner
learner$train(task, train_indxs)
learner$model
glmboost_at$learner$model
