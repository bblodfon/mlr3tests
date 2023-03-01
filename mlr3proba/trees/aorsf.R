# Accelerated Oblique Random Survival Forest Learner
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(survival)
library(aorsf)
library(tidyverse)
library(mlr3mbo)

# Task Lung ----
task = tsk('lung')
pre = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task = pre$train(task)[[1]]

# mlr3-aorsf ----
#' Oblique Random Forest
set.seed(42)
orsf = lrn('surv.aorsf',
  importance = 'none',
  oobag_pred_type = 'surv',
  control_type = 'fast',
  attach_data = FALSE) # NEEDED for importance (but in general I wouldn't keep it)
orsf

orsf$train(task, row_ids = 1:200)
orsf
orsf$model
orsf$model$pred_oobag # OOB-predictions for each observation
orsf$model$eval_oobag # OOB right here (as C-index)!!!
orsf$importance()

p = orsf$predict(task, row_ids = 201:228)
p
p$score()
rcll = msr('surv.rcll')
p$score(rcll)
oob = p$score(msr('oob_error'), learner = orsf)
oob
stopifnot(oob == 1 - orsf$model$eval_oobag$stat_values)

# Hyperparams ----
#' no parallelization yet implemented - author wants to get a grant to implement it
orsf$param_set
orsf$param_set$ids()
orsf$help()
?aorsf::orsf()

# Leaf nodes
leaf_min_events =	1 # minimum number of events in a leaf node (OK)
leaf_min_obs = to_tune(3, 20) # minimum number of observations in a leaf node
#' `min.node.size` in `ranger()`

# Any tree node
split_min_events = 5 # minimum number of events required in a node to consider splitting it
split_min_obs = 10 # minimum number of observations required in a node to consider splitting it (DEFAULT is 10)

# these are okay, more about the splitting process itself
n_split =	5	# the number of cut-points assessed when splitting a node in decision trees (from linear predictors, see 2019 paper)
n_retry = 3	# try up to 3 times to find a linear combination of predictors to split

# TUNE separately or as one?
#' Author's answer: "it certainly wouldn't hurt to think `control_type` as a tuning parameter. Looking at the individual datasets where we ran benchmarks in the arxiv paper, I do see some cases where one orsf method outperformed the others (although they were very similar overall)."
#' `orsf_control_net` much slower than the other two
control_type = to_tune(p_fct('fast', 'cph', 'net'))

# keeps the defaults below
control_fast_do_scale	=	TRUE
control_cph_method = 'efron'
control_cph_eps =	1e-09
control_cph_iter_max = 20 # could tune this => 1 is `fast` method!
control_net_df_target =	NULL # Preferred number of variables used in a linear combination.

#' TO TUNE??? => alpha in glmnet()
control_net_alpha = p_dbl(0, 1) # Default is 0.5

#' keep the rest
split_min_stat = 3.841459 # test_stat of the log-rank test (equivalent to p = 0.05)
oobag_pred_type =	'surv' # c(none, surv, risk, chf) LEAVE IT AT THAT
# 'risk' : predict the probability of having an event at or before oobag_pred_horizon.
# 'surv' : 1 - risk. (they both results in the same OOB error/C-index)
importance = 'permute' # c(none, anova (default), negate, permute)
oobag_pred_horizon = NULL # Default is the median of the observed times, i.e., oobag_pred_horizon = median(time), I don't know if its the same as ranger but it seems like a good choice to get surv prob and use them for ranking and getting C-index
oobag_eval_every = NULL # Default is oobag_eval_every = n_tree (which is BEST)
attach_data = FALSE # keep a copy of the training data in the output.
# needed for `importance`, `orsf_pd_oob` or `orsf_summarize_uni`.

# mRNA test ----
# ~10000 features
tasks = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')
task_mRNA = tasks$mRNA
task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/task_mRNA_flt.rds') # 1000 features

dsplit = mlr3::partition(task_mRNA, ratio = 0.8)
train_indxs = dsplit$train
test_indxs  = dsplit$test

orsf = lrn('surv.aorsf',
  control_type = 'cph',
  #control_cph_iter_max = 1, # exactly the same as `fast`
  #control_net_alpha = 0,
  importance = 'none', # anova is magnitudes faster
  oobag_pred_type = 'surv',
  attach_data = TRUE, # needs this for prediction!?
  verbose_progress = TRUE,
  n_tree = 100
)

# attach_data = TRUE, importance = 'permute' => gets stacked !!!

set.seed(42)
orsf$train(task = task_mRNA, row_ids = train_indxs)
orsf$model
orsf$model$eval_oobag
p = orsf$predict(task = task_mRNA, row_ids = test_indxs)

p$score()
p$score(rcll)

## compare with ranger ----
ranger_lrn = lrn('surv.ranger', num.threads = 1,
  num.trees = 100,
  min.node.size = 3, # default for survival (RSF)
  importance = 'permutation'
)
ranger_lrn$train(task = task_mRNA, row_ids = train_indxs) # super fast with permute!
ranger_lrn$model
head(ranger_lrn$importance())
ranger_lrn$oob_error()
p = ranger_lrn$predict(task = task_mRNA, row_ids = test_indxs)
p$score()

# Tune lung ----
set.seed(42)
dsplit = mlr3::partition(task, ratio = 0.8)
train = dsplit$train
test = dsplit$test
intersect(train, test)

orsf = lrn('surv.aorsf',
  control_type = 'fast',
  n_tree = to_tune(p_int(10, 150)),
  mtry_ratio = to_tune(p_dbl(0.1, 0.9)),
  leaf_min_obs = to_tune(p_int(3, 20)),
  importance = 'anova', # anova (fastest, default), permute (slowwww)
  oobag_pred_type = 'surv', # default
  attach_data = TRUE # needs this for prediction right now (definitely for importance)
)

dplyr::bind_rows(
  generate_design_random(orsf$param_set$search_space(), 20)$transpose()
)

orsf_at = AutoTuner$new(
  learner = orsf,
  #resampling = rsmp('insample'), # TRAIN == TEST
  #measure = msr('oob_error'), # OOB error (can be non-deterministic!!!)
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 40), # 10 - 100
  tuner = tnr('mbo'),
  #store_models = TRUE # for oob_error
)
orsf_at$train(task, row_ids = train)

# time?
orsf_at$timings['train'] # 84

# check if chosen learner has the hps it should (OK!)
orsf_at$learner
orsf_at$learner$model
orsf_at$learner$model$eval_oobag$stat_values
# orsf_at$tuning_result
orsf_at$archive$best()

# get best learner
best_hpc = orsf_at$archive$best()$x_domain[[1L]]
trained_learner = orsf_at$learner$clone(deep = TRUE)
trained_learner$param_set$values = mlr3misc::insert_named(
  trained_learner$param_set$values, best_hpc)
trained_learner$train(task, train)
1 - trained_learner$model$eval_oobag$stat_values # same as below
trained_learner$oob_error()

orsf_at$tuner$surrogate$model # DiceKriging model

p = orsf_at$predict(task, row_ids = test)
p$score()

autoplot(orsf_at$tuning_instance, type = 'performance')


