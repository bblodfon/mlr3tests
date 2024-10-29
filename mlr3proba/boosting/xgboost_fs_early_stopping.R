library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(paradox)
library(mlr3fselect)

task = tsk("grace")
set.seed(42)
part = partition(task, ratio = 0.7)

internal_search_space = ps(
  nrounds = p_int(upper = 500, aggr = function(x) as.integer(mean(unlist(x))))
)

learner = lrn("surv.xgboost.cox", id = "xgb_cox", booster = "gbtree", tree_method = "hist",
              eta = 0.1, max_depth = 6, nrounds = 500, early_stopping_rounds = 42, validate = "test")

autofs = auto_fselector(
  fselector = fs("rfe", subset_sizes = c(6, 4, 2)),
  learner = learner,
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.cindex"),
  terminator = trm("none"),
  #store_models = TRUE,
  callbacks = list(
    clbk("mlr3fselect.internal_tuning", internal_search_space = internal_search_space),
    clbk("mlr3fselect.one_se_rule")
  )
)
autofs$train(task, part$train)
autofs$fselect_result$features
autofs$fselect_result$n_features
autofs$model$learner

p = autofs$predict(task, part$test)
p$score()

cox = lrn("surv.coxph")
p = cox$train(task, part$train)$predict(task, part$test)
p$score()
