library(mlr3verse)
library(mlr3proba)
library(xgboost)
library(tidyverse)
library(mlr3mbo)
library(survival)  # for task veteran

# Hyperparameters ----
# https://xgboost.readthedocs.io/en/latest/parameter.html
?xgboost::xgb.train()

objective = c('survival:cox', 'survival:aft')
# next is set automatically based on the `objective`
eval_metric = c('cox-nloglik','aft-nloglik')
# AFT params to tune (see [Barnwal2022])
aft_loss_distribution = c('normal', 'logistic', 'extreme')
aft_loss_distribution_scale # to_tune(0.5, 2.0)

nthread = 1 # to set for parallelization

# Use `gbtree` (Gradient Boosted Trees)
# Dart might overfit less and generalize better but has more parameters to tune
# DART => Dropouts meet Multiple Additive Regression Trees
booster = c('gbtree', 'gblinear', 'dart')
# for `gbtree` (default)
learner$param_set$default$booster

nrounds = 150 # to_tune(1, 1000) # less? 500?
max_depth = 6 # to_tune(3,10), the higher it is, larger trees are build, more overfitting may happen
eta = 0.3 # to_tune(p_dbl(1e-03, 1, logscale = TRUE))
alpha = 0 # L1 regularization (Can be used in case of very high dimensionality so that the algorithm runs faster when implemented)
lambda = 1 # L2 regularization (use to reduce overfitting)
gamma = 0 # min loss to make a further partition, [0, Inf)
# usually ok to leave at 0 (DON'T TUNE), only after alpha and lambda are tuned
# Tune for shallower trees (max_depth small) - [0, 5] seem reasonable values
colsample_bytree = 1 # fraction of features to be sampled in each tree [0.5, 1]
subsample = 1 # fraction or rows/samples to be sampled in each tree

# Stop boosting if performance doesn't improve after k rounds (when training
# with a validation dataset)
early_stopping_rounds = 10 # nice!
print_every_n = 1 # useful when verbose >= 1
# watchlist = list(validation1 = mat1, validation2 = mat2)

# use CPU or GPU for predictions (default => CPU)
predictor = c('cpu_predictor', 'gpu_predictor')

## GPU (tree_method) ----
# It's recommended to try `hist` and `gpu_hist` for higher performance
# With tree_method = 'gpu_hist' you will get an error if XGBoost is not compiled with GPU support.
# with large dataset but better leave it to 'auto' for now
tree_method = c("auto", "exact", "approx", "hist", "gpu_hist")
# `approx` was using all cores irrespective of `nthreads` (DONT USE - doc says `hist` is also faster)
# Use `hist`/`gpu_hist` when dataset has > 100K observations?

# See https://xgboost.readthedocs.io/en/latest/treemethod.html
# See https://xgboost.readthedocs.io/en/latest/gpu/index.html
# `survival:cox` is not GPU-supported (Aug 2022), `survival:aft` is

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

# Native call ----
## Cox example ----
# xgboost::xgb.train() or xgboost::xgboost() => use matrix
data = task$data(cols = task$feature_names)
target = task$data(cols = task$target_names)
label = target$time
status = target$status
# strange: right censored data becomes negative for xgboost
label[status == 0] = -1L * label[status == 0]

train_data  = as.matrix(data[train_indxs])
train_label = label[train_indxs]
test_data   = as.matrix(data[test_indxs])

fit = xgboost::xgboost(train_data, train_label, nrounds = 150, eta = 0.01,
  max_depth = 8, objective = 'survival:cox', verbose = 2, #tree_method = 'hist',
  early_stopping_rounds = 10)
fit

?predict.xgb.Booster
preds = predict(fit, newdata = test_data)
lps = log(preds)
lps # linear predictors

## AFT example ----
label = target$time
status = target$status
y_lower_bound = y_upper_bound = label
y_upper_bound[status == 0] = Inf

data = task$data(cols = task$feature_names)
data = xgboost::xgb.DMatrix(as.matrix(data[train_indxs]))
xgboost::setinfo(data, "label_lower_bound", y_lower_bound[train_indxs])
xgboost::setinfo(data, "label_upper_bound", y_upper_bound[train_indxs])
xgboost::getinfo(data, "label_upper_bound")

fit2 = xgboost::xgb.train(data = data, nrounds = 150, eta = 0.01,
  max_depth = 8, objective = 'survival:aft', aft_loss_distribution = 'normal',
  aft_loss_distribution_scale = 0.8, verbose = 1)
fit2

preds1 = predict(fit2, newdata = test_data)
preds1 # exp(lp)

# mlr3proba example ----
## Cox example ----
learner = lrn('surv.xgboost', nrounds = 150, eta = 0.01, max_depth = 8,
  nthread = 4, verbose = 1)

learner$train(task, row_ids = train_indxs)
learner$model

preds2 = learner$predict(task, row_ids = test_indxs)
preds2 # lp = crank when objective = 'survival:cox'
all(preds2$lp == lps) # same
preds2$score(msr('surv.cindex'))
preds2$score(msr('surv.cindex', weight_meth = 'G2'), task = task, train_set = train_indxs)

## AFT example ----
learner_aft = lrn('surv.xgboost', objective = 'survival:aft',
  aft_loss_distribution = 'normal',
  aft_loss_distribution_scale = 0.8,
  nrounds = 150, eta = 0.01, max_depth = 8,
  nthread = 4, verbose = 1)
learner_aft$train(task, row_ids = train_indxs)
learner_aft$model

preds3 = learner_aft$predict(task, row_ids = test_indxs)
preds3
all(preds3$lp == -log(preds1))

## Importance ----
imp_mat = xgboost::xgb.importance(
  #feature_names = learner$model$feature_names,
  model = learner$model
)

# Gain is the important
?xgboost::xgb.importance
all(learner$importance() == imp_mat$Gain)

# simple importance barplot
xgboost::xgb.ggplot.importance(importance_matrix = imp_mat)

## SHAP values ----
# per feature
pred_contr1 = predict(learner$model, newdata = test_data,
  predcontrib = TRUE)
# feature interactions
pred_contr2 = predict(learner$model, newdata = test_data,
  predinteraction = TRUE)

xgboost::xgb.plot.shap(data = test_data,
  shap_contrib = pred_contr1, model = learner$model, top_n = 3)
# SHAP SUMMARY (NICE)
xgboost::xgb.ggplot.shap.summary(data = test_data,
  shap_contrib = pred_contr1, model = learner$model)
res$labels$colour = 'Feature Values'
res

# baseline CoxPH ----
lrn('surv.coxph')$
  train(task, row_ids = train_indxs)$
  predict(task, row_ids = test_indxs)$
  score() # ~0.74

# survmob example ----
#' Can't get importance from a graph learner:
#' https://github.com/mlr-org/mlr3pipelines/issues/291
library(survmob)

lps = SurvLPS$new(ids = "surv_tree")
tree = lps$lrns()[[1]]

tree$train(task, row_ids = train_indxs)
tree$graph_model$pipeops$SurvivalTree$learner_model$importance()
# tree$importance() # doesn't work

lps = SurvLPS$new(ids = "xgboost_cox")
xgb = lps$lrns()[[1]]

xgb$train(task, row_ids = train_indxs)
xgb$model$XGBoostCox$model
xgb$graph_model$pipeops$XGBoostCox$learner_model$importance()
# xgb$importance() # doesn't work

# Tune (Cox, 3 HPs) ----
ncores = 4
learner = lrn('surv.xgboost',
  nthread = ncores, booster = 'gbtree',
  nrounds = to_tune(50, 5000),
  early_stopping_rounds = 10,
  eta = to_tune(p_dbl(1e-04, 1, logscale = TRUE)),
  max_depth = to_tune(2, 10))

data.table::rbindlist(
  generate_design_random(learner$param_set$search_space(), 5)$transpose()
)

xgboost_at = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 10), # 10 - 100
  tuner = tnr('mbo')
)
xgboost_at$train(task, row_ids = train_indxs)

# check if chosen learner has the hps it should (OK!)
xgboost_at$learner$model
xgboost_at$tuning_result
xgboost_at$archive$data %>% as_tibble() %>% arrange(desc(surv.cindex))

xgboost_at$timings['train']
p = xgboost_at$predict(task, row_ids = test_indxs)
p$score() # 0.75 - 0.82

autoplot(xgboost_at$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(xgboost_at$tuning_instance, type = 'surface',
  cols_x = c('nrounds', 'x_domain_eta'), trafo = TRUE) +
  labs(y = 'learning rate (eta)')

# Tune (Cox, 3 HPs, BO) ----
xgboost_at_bo = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 100),
  tuner = tnr('mbo')
)
xgboost_at_bo$train(task, row_ids = train_indxs)
xgboost_at_bo$predict(task, row_ids = test_indxs)$score()

autoplot(xgboost_at_bo$tuning_instance, type = 'parameter')
autoplot(xgboost_at_bo$tuning_instance, type = 'surface',
  cols_x = c('nrounds', 'x_domain_eta')) +
  labs(y = 'learning rate (eta)')

# Tune (Cox, 6 HPs, BO) ----
learner = lrn('surv.xgboost',
  nthread = ncores, booster = 'gbtree', early_stopping_rounds = 10,
  nrounds = to_tune(50, 5000),
  eta = to_tune(p_dbl(1e-04, 1, logscale = TRUE)),
  max_depth = to_tune(2, 10),
  min_child_weight = to_tune(1, 100, logscale = TRUE),
  alpha  = to_tune(1e-03, 10, logscale = TRUE),
  lambda = to_tune(1e-03, 10, logscale = TRUE)) # 6 HPs

xgboost_at_bo2 = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 50),
  tuner = tnr('mbo')
)
xgboost_at_bo2$train(task, row_ids = train_indxs)
xgboost_at_bo2$timings['train']
xgboost_at_bo2$predict(task, row_ids = test_indxs)$score() # 0.77
xgboost_at_bo2$tuner$surrogate$model # KM (Gaussian Process as surrogate)

autoplot(xgboost_at_bo2$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(xgboost_at_bo2$tuning_instance, type = 'surface',
  cols_x = c('nrounds', 'x_domain_eta')) +
  labs(y = 'learning rate (eta)')

# Tune (AFT, 8 HPs, BO) ----
learner = lrn('surv.xgboost',
  objective = 'survival:aft', # change objective
  nthread = ncores, booster = 'gbtree', early_stopping_rounds = 10,
  nrounds = to_tune(50, 5000),
  eta = to_tune(p_dbl(1e-04, 1, logscale = TRUE)),
  max_depth = to_tune(2, 10),
  min_child_weight = to_tune(1, 100, logscale = TRUE),
  alpha  = to_tune(1e-03, 10, logscale = TRUE),
  lambda = to_tune(1e-03, 10, logscale = TRUE),
  # add AFT-specific HPs
  aft_loss_distribution = to_tune(c('normal', 'logistic', 'extreme')),
  aft_loss_distribution_scale = to_tune(0.5, 2.0))

xgboost_at_bo3 = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 50),
  tuner = tnr('mbo')
)
xgboost_at_bo3$train(task, row_ids = train_indxs)
xgboost_at_bo3$timings['train']
xgboost_at_bo3$predict(task, row_ids = test_indxs)$score() # 0.79
xgboost_at_bo3$tuner$surrogate$model # ranger (there exists a categorical HP)

autoplot(xgboost_at_bo3$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(xgboost_at_bo3$tuning_instance, type = 'performance')
autoplot(xgboost_at_bo3$tuning_instance, type = 'surface',
  cols_x = c('nrounds', 'x_domain_eta')) +
  labs(y = 'learning rate (eta)')

# Tune Full ----
## `early_stopping_rounds` => set at 10% of `nrounds`
ncores = 4
learner = lrn('surv.xgboost', nthread = ncores, booster = 'gbtree',
  objective = 'survival:aft')

search_space = ps(
  nrounds = p_int(50, 500), # upper = 5000
  eta = p_dbl(1e-03, 1, logscale = TRUE), # low = 1e-04
  max_depth = p_int(2, 10),
  min_child_weight = p_dbl(1, 100, logscale = TRUE),
  alpha  = p_dbl(1e-03, 10, logscale = TRUE),
  lambda = p_dbl(1e-03, 10, logscale = TRUE),
  gamma  = p_dbl(0, 5),
  aft_loss_distribution = p_fct(c('normal', 'logistic', 'extreme')),
  aft_loss_distribution_scale = p_dbl(0.5, 2.0),
  .extra_trafo = function(x, param_set) {
    x$early_stopping_rounds = as.integer(ceiling(0.1 * x$nrounds))
    x
  }
)

data.table::rbindlist(
  generate_design_random(search_space, 5)$transpose()
)

xgboost_at_bo3 = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  search_space = search_space,
  terminator = trm('evals', n_evals = 50),
  tuner = tnr('mbo')
)

xgboost_at_bo3$train(task, row_ids = train_indxs)
xgboost_at_bo3$predict(task, row_ids = train_indxs)$score() # 0.75
xgboost_at_bo3$predict(task, row_ids = test_indxs)$score() # 0.78

xgboost_at_bo3$archive$best()$x_domain[[1L]]
xgboost_at_bo3$tuning_result$learner_param_vals[[1]] # different!
xgboost_at_bo3$learner # different!

# let's train the learner with the best HP from the tuning
best_hpc = xgboost_at_bo3$archive$best()$x_domain[[1L]]
learner # empty, untrained xgboost learner
learner$param_set$values =
  mlr3misc::insert_named(learner$param_set$values, best_hpc)

learner$train(task, row_ids = train_indxs)
learner$model

learner$predict(task, row_ids = train_indxs)$score() # 0.76
learner$predict(task, row_ids = test_indxs)$score() # 0.75
