# https://github.com/mlr-org/mlr3tuning/issues/376
library(mlr3proba)
library(mlr3extralearners)
library(mlr3tuning)
library(survmob)

s = SurvLPS$new(ids = 'xgboost_cox_early')
dt = s$lrn_tbl()
grlrn = dt$learner[[1L]] # XGBoost Cox with distr prediction graph learner
grlrn
param_set = dt$param_set[[1L]]
param_set

task = tsk('lung')

xgb_at = AutoTuner$new(
  learner = grlrn,
  resampling = rsmp('holdout'),
  measure = msr('surv.brier'),
  search_space = param_set,
  terminator = trm('evals', n_evals = 5),
  tuner = tnr('random_search'),
  callbacks = clbk("mlr3tuning.early_stopping")
)
xgb_at$train(task)
