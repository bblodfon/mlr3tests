# https://stackoverflow.com/questions/73527213/setting-early-stopping-rounds-in-xgboost-learner-using-mlr3
library(mlr3verse)
library(tidyverse)
library(mlr3proba)
library(survival)

# Veteran task ----
as_tibble(veteran)
set.seed(42)
train_indxs = sample(seq_len(nrow(veteran)), 100)
test_indxs  = setdiff(seq_len(nrow(veteran)), train_indxs)
intersect(train_indxs, test_indxs)

task = as_task_surv(x = veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]
task

# doesn't work
learner = lrn('surv.xgboost',
  nrounds = to_tune(50, 5000),
  early_stopping_rounds = to_tune(ps(
    early_stopping_rounds = p_int(1,5000),
    .extra_trafo = function(x, param_set) {
      list(early_stopping_rounds = ceiling(0.1 * x$nrounds))
  }, .allow_dangling_dependencies = TRUE)))

# works
pam = ps(z = p_int(-3,3), x = p_int(0,10),
  .extra_trafo = function(x, param_set) {
    x$z = 2*(x$x) # overwrite z as 2*x
    x
  }
)

dplyr::bind_rows(generate_design_random(pam, 5)$transpose())

# SOLUTION
search_space = ps(
  nrounds = p_int(lower = 50, upper = 5000),
  .extra_trafo = function(x, param_set) {
    x$early_stopping_rounds = as.integer(ceiling(0.1 * x$nrounds))
    x
  }
)

task = tsk("iris")
learner = lrn("classif.xgboost")
terminator = trm("evals", n_evals = 10)
tuner = tnr("random_search")

at = AutoTuner$new(
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  search_space = search_space,
  terminator = terminator,
  tuner = tuner
)

at$train(task)

at$archive
data.table::rbindlist(at$archive$data$x_domain)
