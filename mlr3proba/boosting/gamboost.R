# mboost: model-based/component-based boosting
# practical tutorial/paper => vignette(package = "mboost", "mboost_tutorial")
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(tidyverse)
library(mlr3mbo)
library(survival) # for task veteran

# mRNA task ----
task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA

# Veteran task ----
task = as_task_surv(x = survival::veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]
task

set.seed(42)
train_indxs = sample(seq_len(task$nrow), 100)
test_indxs  = setdiff(seq_len(task$nrow), train_indxs)

# Gamboost learner (additive) ----
learner = lrn('surv.gamboost')
learner$help()
?mboost::gamboost()

## In gamboost centering is only needed if bols() base-learners are specified without intercept

## HPs ----
if (FALSE) {
# 'coxph' => returns lp and distr (Breslow)
# SOS => all other families are like AFT (return "-lp")
# Gehan loss function (for AFT models) is from this paper
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3148798/
family = to_tune(p_fct(c('coxph', 'weibull', 'loglog', 'lognormal', 'gehan', 'cindex')))
# sigma and ipcw for cindex family only
sigma = to_tune(p_dbl(0.1, 0.5)) # 0.1 (per paper), higher values 'stretch' the sigmoid
# Paper [Mayr2014]: While too large values will lead to a poor approximation of the indicator functions in (7), too small values of sigma might overfit the data (and might therefore result in a decreased prediction accuracy
ipcw = 1 # default, don't change

center = TRUE # default, may need to change to `FALSE` if using already centered data
trace = FALSE # default

# TO TUNE => boosting hpcs
mstop = to_tune(p_int(50, 500)) # boosting iterations
nu = to_tune(p_dbl(1e-03, 1, logscale = TRUE)) # learning rate/step

# TO TUNE => base learners
?mboost::baselearners
baselearner	= to_tune(p_fct('bbs', 'bols', 'btree'))
# bols => linear base-learner (can have a ridge penalty if df is supplied => get lambda from that)
# bbs => penalized regression splines (smooth P-splines) as base-learner
# btree => tree-based (stumps => maxdepth = 1) base learner, maybe better to use mboost::blackboost() as trees there can be larger?
dfbase = to_tune(p_int(2,4)) # default is 4, ONLY FOR bbs()
# the degrees of freedom for P-spline base-learners (bbs) globally.
# [Buhlmann2007] says 4 is okay (low var, high bias), tune `mstop` to reduce bias

# DON'T CHANGE
risk = 'inbag' # default, don't change => risks are computed for the learning sample ('inbag')
stopintern = FALSE # default, has to do with risk = 'out-of-bag'
oobweights # also has to do with risk = 'out-of-bag', don't specify
nuirange # for other families not of interest to us
offset # intercept?
}

## bols() ----
learner = lrn('surv.gamboost', baselearner = 'bols')
learner$train(task, train_indxs)
learner$model
preds = learner$predict(task, test_indxs)
preds # includes `distr` since it was CoxPH
preds$score()

learner = lrn('surv.gamboost', baselearner = 'bols', family = 'lognormal')
learner$train(task, train_indxs)
learner$model
preds = learner$predict(task, test_indxs)
preds # no `distr`
preds$score()

learner = lrn('surv.gamboost', baselearner = 'bols', family = 'gehan')
learner$train(task, train_indxs)
learner$model
preds = learner$predict(task, test_indxs)
preds # no `distr`
preds$score()

learner = lrn('surv.gamboost', baselearner = 'bols', family = 'cindex')
learner$train(task, train_indxs)
learner$model
preds = learner$predict(task, test_indxs)
preds # no `distr`
preds$score()

## bbs() ----
## Doesn't work at all, no matter the `center` or `dfbase`!!!
if (FALSE) {
learner = lrn('surv.gamboost', center = FALSE, baselearner = 'bbs', dfbase = 2)
learner$train(task, train_indxs)

learner$param_set$values = list(family = 'loglog')
learner$train(task, train_indxs)
}

## btree() ----
learner = lrn('surv.gamboost', baselearner = 'btree')
learner$train(task, train_indxs)
learner
learner$model
preds = learner$predict(task, test_indxs)
preds # includes `distr` since it was CoxPH
preds$score()

learner = lrn('surv.gamboost', baselearner = 'btree', family = 'weibull')
learner$encapsulate = c(train = "evaluate", predict = "evaluate")
learner$train(task, train_indxs)
learner
learner$log
learner$model

preds = learner$predict(task, test_indxs)
preds # includes `distr` since it was CoxPH
preds$score()

learner$importance()
learner$selected_features()

## mRNA train ----
## does a simple train run okay? does it have memory or other issues? ISSUES!!!
learner = lrn('surv.gamboost', baselearner = 'bols') # R session ends!
# Error: C stack usage  7977652 is too close to the limit
learner$train(task_mRNA)

# also recursive/memory issue
learner = lrn('surv.gamboost', baselearner = 'bbs')
learner$train(task_mRNA)
# also recursive/memory issue
learner = lrn('surv.gamboost', baselearner = 'bbs')
learner$train(task_mRNA)

# No Tuning attempted since it doesn't work on the mRNA dataset,
# so high-dimensionality is not a pro feature
