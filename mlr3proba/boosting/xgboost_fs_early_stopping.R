library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(paradox)
library(mlr3fselect)
library(mlr3viz)
library(ggplot2)

task = tsk("gbcs")
task
autoplot(task)
task$cens_prop()

set.seed(42)
part = partition(task, ratio = 0.7)

xgb = lrn("surv.xgboost.cox", id = "xgb_cox", booster = "gbtree",
          tree_method = "hist", eta = 0.1, max_depth = 6, nrounds = 1000)

xgb_early = xgb$clone(deep = TRUE)
xgb_early$id = "xgb_cox_early"
xgb_early$validate = 1/3 # 1/3 of training data for validation
xgb_early$param_set$set_values(
  .values = list(early_stopping_rounds = 42)
)

# compare two learners
xgb$train(task, row_ids = part$train)
xgb_early$train(task, row_ids = part$train)

# boosting rounds of trained model
xgb$model$model$niter
xgb_early$model$model$niter

xgb_early$model$model$evaluation_log |>
  ggplot(aes(x = iter, y = test_cox_nloglik)) +
  geom_line()

# performance on test set
p = xgb$predict(task, row_ids = part$test)
p_early = xgb_early$predict(task, row_ids = part$test)
p_cox =
  lrn("surv.coxph")$
  train(task, part$train)$
  predict(task, part$test)

p$score() # xgboost
p_early$score() # xgboost_early
p_cox$score() # cox

p$score(msr("surv.brier", p_max = 0.8))
p_early$score(msr("surv.brier", p_max = 0.8))
p_cox$score(msr("surv.brier", p_max = 0.8))

# performance on a resampling - 5 x 5-fold CV
xgb_early$validate = "test" # use the test fold!

bgrid = benchmark_grid(
  tasks = task,
  learners = list(cox, xgb, xgb_early),
  resamplings = rsmp("repeated_cv", repeats = 5, folds = 5)
)

bm = benchmark(design = bgrid)
autoplot(bm, type = "boxplot") # same performance
autoplot(bm, type = "boxplot", measure = msr("time_train"))
bm$aggregate(msr("time_train")) # save time with early stopping

# xgboost + RFE
# which subsets sizes?
n = task$n_features
feature_fraction = 0.8
n_features = 2
subset_sizes = unique(floor(cumprod(c(n, rep(feature_fraction, log(n_features / n) / log(feature_fraction))))))
subset_sizes

fsel = fs("rfe", feature_fraction = 0.8, n_features = 2)

afs = auto_fselector(
  fselector = fsel,
  learner = xgb,
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.cindex"),
  terminator = trm("none")
)
set.seed(42)
afs$train(task, part$train)

dt = as.data.table(afs$archive)
dt[, .(n_features, features, surv.cindex)]
afs$fselect_result
afs$learner # final model
afs$learner$model$model # less features used
afs$timings

afs$predict(task, part$test)$score()

# xgboost + RFE + 1se rule
afs2 = auto_fselector(
  fselector = fsel,
  learner = xgb,
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.cindex"),
  terminator = trm("none"),
  callbacks = list(
    clbk("mlr3fselect.one_se_rule")
  )
)
set.seed(42)
afs2$train(task, part$train)
as.data.table(afs2$archive)[, .(n_features, features, surv.cindex)]
afs2$fselect_result
afs2$learner # final model
afs2$learner$model$model # uses even less features!
afs2$timings

afs2$predict(task, part$test)$score()

# xgboost + RFE + early_stopping + 1se rule
internal_search_space = ps(
  nrounds = p_int(upper = 1000, aggr = function(x) as.integer(mean(unlist(x))))
)

afs3 = auto_fselector(
  fselector = fsel,
  learner = xgb_early, # on the test sets of each CV
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.cindex"),
  terminator = trm("none"),
  callbacks = list(
    clbk("mlr3fselect.one_se_rule"),
    clbk("mlr3fselect.internal_tuning", internal_search_space = internal_search_space)
  )
)
set.seed(42)
afs3$train(task, part$train)
as.data.table(afs3$archive)[, .(n_features, features, surv.cindex)]
afs3$fselect_result
afs3$learner # final model, how to show the nrounds is the average here?
afs3$learner$model$model # uses even less features, rounds is also small
afs3$timings

afs3$predict(task, part$test)$score()
