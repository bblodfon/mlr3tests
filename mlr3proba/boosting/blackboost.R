# mboost: model-based/component-based boosting
# practical tutorial/paper => vignette(package = 'mboost', 'mboost_tutorial')
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(tidyverse)
library(mlr3mbo)
library(survival) # for task veteran

# Veteran task ----
task = as_task_surv(x = survival::veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]
task

set.seed(42)
train_indx = sample(seq_len(task$nrow), 100)
test_indx  = setdiff(seq_len(task$nrow), train_indxs)

# Details ----
#' blackboost implements the ‘classical’ gradient boosting utilizing regression
#' trees as base-learners (as gbm). Supports arbitrary loss functions and
#' base-learners are conditional inference trees (ctrees, more flexible,
#' based on p-values for splits and variable selection)
#' The name comes from being a black-box learner (as it was thought at the
#' time I guess, e.g. xgboost returns importance scores now)

# Hyperparameters ----
learner = lrn('surv.blackboost')
learner$help()
?mboost::blackboost()

## General => change: no centering is required!
center = FALSE # blackboost has trees, doesn't need centering
trace = FALSE # default

## Family => TUNE!
# 'coxph' => returns lp and distr (Breslow)
# SOS => all other families are like AFT (return -lp)
# Gehan loss function (for AFT models) is from this paper:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3148798/
family = to_tune(p_fct(c('coxph', 'weibull', 'loglog', 'lognormal', 'gehan', 'cindex')))
# sigma and ipcw for cindex family only
sigma = to_tune(p_dbl(0.1, 0.5)) # 0.1 (per paper), higher values 'stretch' the sigmoid
# Paper [Mayr2014]: While too large values will lead to a poor approximation of the indicator functions in (7), too small values of sigma might overfit the data (and might therefore result in a decreased prediction accuracy
ipcw = 1 # default, don't change

# TO TUNE => boosting hpcs
mstop = to_tune(p_int(50, 500)) # boosting iterations
nu = to_tune(p_dbl(1e-03, 1, logscale = TRUE)) # learning rate

# DON'T TOUCH!
risk = 'inbag' # default, don't change => risks are computed for the learning sample ('inbag')
stopintern = FALSE # default, has to do with risk = 'out-of-bag'
oobweights # also has to do with risk = 'out-of-bag', don't specify
nuirange # for other families => not of interest to us

## ctree_control()
?partykit::ctree()
?partykit::ctree_control()

## TO TUNE
#' Doc details => The arguments `teststat`, `testtype` and `mincriterion`
#' determine how the global null hypothesis of independence between all
#' input variables and the response is tested
teststat = to_tune(p_fct(c('quadratic', 'maximum'))) # for variable selection
testtype = to_tune(p_fct(c('Bonferroni', 'Univariate', 'Teststatistic', 'MonteCarlo')))
# first 3 use p-values, the last uses the raw test statistic as criterion
nresample	=	9999 # number of permutations for testtype = "MonteCarlo",
# MonteCarlo takes time and the above value is kinda standard to get
# stable results, so consider to not include it!

## DON'T CHANGE
splitstat	= to_tune(p_fct(c('quadratic', 'maximum'))) # splitpoint selection, didn't seem to produce different results though?
splittest	=	FALSE # default, a logical changing linear (the default => FALSE)
# to maximally selected statistics for variable selection. Currently needs
# testtype = "MonteCarlo" (seems a special hp, DON'T CHANGE)

#' CAN ONLY TUNE `logmincriterion` due to issue with sign. levels for split
# SO LEAVE AT Default 0.05 and 0.95
alpha	=	0.05 # significance level for variable selection
mincriterion = 0.95	# for the split (1 - alpha)
logmincriterion	=	log(0.95) # log(mincriterion)

## issue
saved_ctrl = partykit::ctree_control()
'alpha' %in% names(saved_ctrl)
'mincriterion' %in% names(saved_ctrl)
'logmincriterion' %in% names(saved_ctrl) # YES
learner = lrn('surv.blackboost', alpha = 0.01)
learner$train(task)
learner = lrn('surv.blackboost', logmincriterion = log(0.95))
learner$train(task)
learner$model
learner$predict(task)$score()

#' ok different output with different `logmicriterion`, so it accepts it
learner = lrn('surv.blackboost', logmincriterion = log(0.99))
learner$train(task)
learner$model
learner$predict(task)$score()

# SPECIFY max_depth!
max_depth = to_tune(p_int(2, 10)) # like xgboost
?rpart::rpart.control

minsplit = 20 # minimum sum of weights (obs) in a node in order to be considered for splitting
minbucket =	7 # minimum sum of weights (obs) in a terminal/leaf node (minsplit/3)

minsplit = to_tune(p_int(1, 100, logscale = TRUE))
# and set "minbucket = minsplit/3" as in rpart

# DON'T TOUCH!
## Parameters for the computation of multivariate normal probabilities
maxpts = 25000 # defaults
abseps = 0.001
releps = 0
nmax # from doc => "Highly experimental, use at your own risk"
minprob =	0.01 # AT LEAST (0.01 * n_rows) are needed proportion of observations needed to establish a terminal node, for 100 rows train data, at least 1 obs is needed to create a leaf node
stump	= FALSE # don't do stump, rather larger trees
lookahead	= FALSE # a logical determining whether a split is implemented only after checking if tests in both daughter nodes can be performed
MIA	=	FALSE # a logical determining the treatment of NA as a category in split, see Twala et al. (2008)
tol	=	sqrt(.Machine$double.eps) # 1.490116e-08
maxsurrogate = 0 # number of surrogate splits to evaluate. rpart uses 5 (?)
mtry # algorithm puts 'Inf' here, meaning try all variables that you have
# and NOT do a random selection of 'mtry' input variables in each node (like in RFs)
multiway = FALSE # something about factors and splits
splittry = 2 # try 2 more variables if best split doesn't meet sample size constrains
intersplit = FALSE # no intersplit, i.e. use x <= a to make a split
majority = FALSE # randomly assign obs that you can't place in a daughter node
caseweights	= FALSE

# Examples ----
learner = lrn('surv.blackboost')
learner$train(task, train_indxs)
learner$model

preds = learner$predict(task, test_indxs)
preds # include `distr` since it was CoxPH
preds$score()

# Another family (no distr)
learner$reset()
learner$param_set$values = list(family = 'loglog')
learner$train(task, train_indxs)
learner$model
preds2 = learner$predict(task, test_indxs)
preds2 # no distr
preds2$score()

# try some diff. params
learner = lrn('surv.blackboost', center = FALSE,
  family = 'gehan', # tune, sigma for 'cindex' family as well
  mstop = 80, nu = 0.05, # tune
  minbucket = 5, minsplit = 25, # tune two first if possible
  maxdepth = 10,
  teststat = 'maximum', # quadratic, maximum
  testtype = 'Teststatistic' # 'Bonferroni', 'Univariate', 'Teststatistic'
)
learner$train(task, train_indxs)
learner$model
preds3 = learner$predict(task, test_indxs)
preds3$score()

# mRNA train ----
## does a simple train run okay? does it have memory or other issues? NO, ALL COOL, but takes a loooong time (never saw it finished!)
task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA

learner = lrn('surv.blackboost', center = FALSE, family = 'coxph',
  mstop = 100, nu = 0.05, maxdepth = 10, minsplit = 11,
  teststat = 'maximum', testtype = 'Univariate', minbucket = 4)
learner$train(task_mRNA)
learner$predict(task_mRNA)$score()

learner$timings

# Tune ----
## CoxPH (7 HPs) ----
blackboost_lrn = lrn('surv.blackboost', family = 'coxph',
  center = FALSE, maxdepth = 30)

search_space_cox = ps(
  mstop = p_int(50, 500), # boosting iterations
  nu = p_dbl(1e-03, 1, logscale = TRUE), # learning rate
  minsplit = p_int(1, 100, logscale = TRUE),
  max_depth = p_int(2, 10),
  teststat = p_fct(c('quadratic', 'maximum')),
  testtype = p_fct(c('Bonferroni', 'Univariate', 'Teststatistic')),
  .extra_trafo = function(x, param_set) {
    x$minbucket = as.integer(ceiling(x$minsplit/3))
    x
  }
)

data.table::rbindlist(paradox::generate_design_random(search_space, 10)$transpose())

blackboost_at = AutoTuner$new(
  learner = blackboost_lrn,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 20), # 10 - 100
  tuner = tnr('mbo'),
  search_space = search_space_cox
)
blackboost_at$train(task, row_ids = train_indx)

# took ~8 min !!!
blackboost_at$timings['train']/60
blackboost_at$archive
data.table::rbindlist(blackboost_at$archive$data$x_domain)

blackboost_at$learner$model
data.table::rbindlist(blackboost_at$tuning_result$x_domain)

blackboost_at$predict(task, train_indx)$score()
blackboost_at$predict(task, test_indx)$score()

blackboost_at$archive$best()$x_domain[[1L]] # best HPs different from the ones set by BO
blackboost_at$tuning_result$learner_param_vals[[1]]

print('Performance on test set of best hpc')
best_hpc = blackboost_at$archive$best()$x_domain[[1L]]
blackboost_lrn$param_set$values = mlr3misc::insert_named(blackboost_lrn$param_set$values, best_hpc)
blackboost_lrn$train(task, train_indx)
blackboost_lrn$predict(task, test_indx)$score()

## Other families ----
# see `glmboost.R`