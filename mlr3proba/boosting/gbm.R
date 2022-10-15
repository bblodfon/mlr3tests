library(mlr3verse)
library(mlr3proba)
library(tidyverse)
library(mlr3mbo)
library(survival)  # for task veteran
library(gbm)

# Hyperparameters ----
learner = lrn('surv.gbm')
?gbm::gbm() # few HPs
learner$help()

## already set
learner$param_set$default$distribution
distribution = 'coxph'
learner$param_set$values$keep.data # FALSE
keep.data =	FALSE
learner$param_set$default$verbose
verbose	= FALSE

## to tune or set
n.trees	= to_tune(50, 5000) # `nrounds` (xgboost)
interaction.depth	= to_tune(2, 10) # `max_depth` (xgboost)
shrinkage	=	to_tune(p_dbl(1e-04, 1, logscale = TRUE)) # `eta` (xgboost)

n.minobsinnode = 10	# TUNE? NO => maybe lower for smaller datasets e.g. (5)
# c(5, 10, 15)
learner$param_set$default$bag.fraction # 0.5
bag.fraction = 1 # row sampling => CHANGE!
# No stochastic gradient boosting for small datasets (like `subsample` in xgboost)

## others
learner$param_set$default$train.fraction
train.fraction = 1 # use all training dataset [0,1] => no out-of-sample error estimates
learner$param_set$default$cv.folds
cv.folds	= 0 # no CV from gbm required :)
n.cores	=	1 # CV parallelization only!!!
var.monotone # predictor vector indicating which variables have a monotone increasing (+1), decreasing (-1), or arbitrary (0) relationship with the outcome. (WHAT?!)
learner$param_set$default$single.tree
single.tree	= FALSE # for prediction => FALSE means it aggregates predictions as boosting should do, so don't change

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
fit = gbm::gbm(formula = task$formula(), data = task$data()[train_indxs],
  distribution = 'coxph', keep.data = FALSE,
  n.trees = 500, interaction.depth = 8, shrinkage = 0.01,
  bag.fraction = 1, verbose = TRUE, n.cores = 4)
fit

?predict.gbm
# default use all trees
preds = predict(fit, newdata = task$data()[test_indxs], single.tree = FALSE)
preds # lps!

# mlr3proba example ----
learner = lrn('surv.gbm', n.trees = 500, interaction.depth = 8,
  shrinkage = 0.01, bag.fraction = 1, verbose = TRUE)
bind_rows(learner$param_set$values)

learner$train(task, row_ids = train_indxs)
learner$model

preds2 = learner$predict(task, row_ids = test_indxs)
preds2 # lp = crank
all(preds2$lp == preds) # same
preds2$score()

## Importance ----
learner$importance()

## baseline CoxPH ----
lrn('surv.coxph')$
  train(task, row_ids = train_indxs)$
  predict(task, row_ids = test_indxs)$
  score() # 0.76

# Tune GBM (3 HPs) ----
learner = lrn('surv.gbm', verbose = FALSE,
  n.trees = to_tune(50, 5000),
  shrinkage = to_tune(p_dbl(1e-04, 1, logscale = TRUE)),
  interaction.depth = to_tune(2, 10),
  n.minobsinnode = 5, # just set it
  bag.fraction = 1)

data.table::rbindlist(
  generate_design_random(learner$param_set$search_space(), 5)$transpose()
)

gbm_at = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 50), # 10 - 100
  tuner = tnr('mbo')
)
gbm_at$train(task, row_ids = train_indxs)

gbm_at$timings['train']

p2 = gbm_at$predict(task, row_ids = test_indxs)
p2 # crank, lp
p2$score()

# check surrogate
gbm_at$tuner$surrogate$model

autoplot(gbm_at$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(gbm_at$tuning_instance, type = 'performance')
