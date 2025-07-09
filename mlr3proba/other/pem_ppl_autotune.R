library(pammtools)
library(xgboost)
library(mlr3proba)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)

# task
task = tsk("gbcs")
part = partition(task)

# learner for pem
#lrn_xgb = lrn("regr.xgboost", nrounds = 1000, objective = "count:poisson")
# to tune learner
#lrn_xgb = lrn("regr.xgboost", nrounds = to_tune(1, 1000), objective = "count:poisson")
# same but use search_space:
#lrn_xgb = lrn("regr.xgboost", objective = "count:poisson")
#search_space = ps(regr.xgboost.nrounds = p_int(lower = 1, upper = 1000))
# with early stopping:
lrn_xgb = lrn("regr.xgboost", nrounds = 1000, early_stopping_rounds = 42,
              eval_metric = "poisson-nloglik",
              # "logloss" for binary classif
              objective = "count:poisson")
search_space = ps(
  regr.xgboost.eta = p_dbl(0, 1),
  regr.xgboost.nrounds = p_int(upper = 1000,
                               tags = "internal_tuning",
                               aggr = function(x) as.integer(mean(unlist(x))))
)

grlrn = ppl("survtoregr_pem", learner = lrn_xgb, cut = 15, graph_learner = TRUE)
grlrn # this is practically like a survival learner

# simple train/test
set_validate(grlrn, validate = 0.2)
grlrn$train(task)

# early stopping was used, great!
grlrn$model$regr.xgboost$internal_tuned_values
grlrn$model$regr.xgboost$model$best_iteration
grlrn$model$regr.xgboost$model$evaluation_log

# make an AutoTuner
set_validate(grlrn, validate = "test", ids = "regr.xgboost")
at = auto_tuner(
  learner = grlrn$reset()$clone(deep = TRUE),
  search_space = search_space,
  resampling = rsmp("cv", folds = 3),
  measure = msr("surv.cindex"),
  term_evals = 5,
  tuner = tnr("random_search"),
  #tuner = tnr("internal"),
  store_models = TRUE
)

at$train(task, part$train)
at$archive$data$internal_tuned_values
at$model$learner #YAY
