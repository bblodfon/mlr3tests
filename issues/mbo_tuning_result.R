# I have reasons to believe BO doesn't store the best configuration!
# https://github.com/mlr-org/mlr3mbo/issues/84 (SOLVED)
# MBO assigns the final result by making use of the ?result_by_surrogate_design function
?result_by_surrogate_design
library(mlr3verse)
library(mlr3proba)
library(xgboost)
library(tidyverse)
library(mlr3mbo)
library(survival)

# Less logging
lgr::get_logger('bbotk')$set_threshold('warn')
lgr::get_logger('mlr3')$set_threshold('warn')

set.seed(42)
train_indxs = sample(seq_len(nrow(veteran)), 100)
test_indxs = setdiff(seq_len(nrow(veteran)), train_indxs)
task = as_task_surv(x = veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]
task

ncores = 4
learner = lrn('surv.xgboost',
  nthread = ncores, booster = 'gbtree',
  nrounds = to_tune(50, 1000),
  eta = to_tune(p_dbl(1e-04, 1, logscale = TRUE)),
  max_depth = to_tune(2, 10))

# Bayesian Optimization ----
xgboost_at_bo = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 30),
  tuner = tnr('mbo')
)

set.seed(42)
xgboost_at_bo$train(task, row_ids = train_indxs)

xgboost_at_bo$tuning_result
xgboost_at_bo$tuning_instance$result
xgboost_at_bo$archive$best() # should be different

# if you want to pass the best hp param configuration to a learner
best_hpc = xgboost_at_bo$archive$best()$x_domain[[1L]]
learner$param_set$values = mlr3misc::insert_named(learner$param_set$values, best_hpc)

# evaluation on ALL training data (much higher!)
# which is different than the CV-average!
xgboost_at_bo$predict(task, row_ids = train_indxs)$score()
xgboost_at_bo$predict(task, row_ids = test_indxs)$score()

xgboost_at_bo$archive$data %>%
  as_tibble() %>%
  select(nrounds, eta, max_depth, surv.cindex, uhash) %>%
  arrange(desc(surv.cindex)) %>% # note that the surv.cindex is the CV average
  slice(n = 1:5)

hash = xgboost_at_bo$archive$data %>%
  as_tibble() %>%
  select(nrounds, eta, max_depth, surv.cindex, uhash) %>%
  arrange(desc(surv.cindex)) %>% # note that the surv.cindex is the CV average
  slice(n = 1) %>% # get best
  pull(uhash)

rs = xgboost_at_bo$archive$resample_result(uhash = hash)
rs$score()
rs$aggregate() # same: CV-average

# Change how the result hp configuration is chosen from the archive ----
## Assigned by default
?result_by_surrogate_design

## Assign the best target value as the result (single crit optimization)
default_result = function(inst, optimizer_mbo) {
  res = inst$archive$best()
  xdt = res[, inst$search_space$ids(), with = FALSE]
  y = unlist(res[, inst$archive$cols_y, with = FALSE])
  inst$assign_result(xdt, y)
  invisible(NULL)
}

tuner = tnr('mbo')
tuner$result_function = default_result

xgboost_at_bo2 = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 30),
  tuner = tuner
)

set.seed(42)
xgboost_at_bo2$train(task, row_ids = train_indxs)

xgboost_at_bo2$tuning_result
xgboost_at_bo2$tuning_instance$result
xgboost_at_bo2$archive$best() # SAME AS ABOVE!

# Random Search ----
xgboost_at_rs = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 30),
  tuner = tnr('random_search')
)
xgboost_at_rs$train(task, row_ids = train_indxs)

# same
xgboost_at_rs$tuning_result
xgboost_at_rs$archive$best()
bind_rows(xgboost_at_rs$tuning_instance$result_learner_param_vals)

xgboost_at_rs$archive$data %>%
  as_tibble() %>%
  arrange(desc(surv.cindex)) %>% print(n = 5)
xgboost_at_rs$learner
xgboost_at_rs$predict(task, row_ids = train_indxs)$score() # higher
