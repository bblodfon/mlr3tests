# https://github.com/dmlc/xgboost/issues/9979
library(mlr3)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3extralearners)

learner = lrn(
  "surv.xgboost",
  tree_method = "hist",
  booster = "gbtree",
  objective = "survival:cox",
  nrounds = 57,
  eta = 0.9687533,
  max_depth = 2
)

graph_learner =
  ppl("distrcompositor", learner = learner, form = "ph", estimator = "kaplan") |>
  as_learner()

set.seed(96)
resampling = rsmp("holdout")
resampling$instantiate(tsk("grace"))

graph_learner$train(tsk("grace"), row_ids = resampling$instance$train)
graph_learner$predict(tsk("grace"), row_ids = resampling$instance$test)
