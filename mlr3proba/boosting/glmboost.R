# mboost: model-based/component-based boosting
# practical tutorial/paper => vignette(package = "mboost", "mboost_tutorial")
# glmboost default = gamboost with baselearner=bols = mboost with baselearner=bols
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
train_indxs = sample(seq_len(task$nrow), 100)
test_indxs  = setdiff(seq_len(task$nrow), train_indxs)

# Hyperparameters ----
learner = lrn('surv.glmboost')
learner$help()
?mboost::glmboost()

# 'coxph' => returns lp and distr (Breslow)
# SOS => all other families are like AFT (return -lp)
# Gehan loss function (for AFT models) is from this paper:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3148798/ (rank-based boosting)
family = to_tune(p_fct(c('coxph', 'weibull', 'loglog', 'lognormal', 'gehan', 'cindex')))
# sigma and ipcw for cindex family only
sigma = to_tune(p_dbl(0.1, 0.5)) # 0.1 (per paper), higher values 'stretch' the sigmoid
# Paper [Mayr2014]: While too large values will lead to a poor approximation of the indicator functions in (7), too small values of sigma might overfit the data (and might therefore result in a decreased prediction accuracy
ipcw = 1 # default, don't change

center = TRUE # default, change to `FALSE` if using already scaled data
trace = FALSE # default

# TO TUNE => boosting hpcs
mstop = to_tune(p_int(50, 500)) # boosting iterations
nu = to_tune(p_dbl(1e-03, 1, logscale = TRUE)) # learning rate

# Don't do anything with these
risk = 'inbag' # default, don't change => risks are computed for the learning sample ('inbag')
stopintern = FALSE # default, has to do with risk = 'out-of-bag'
oobweights # also has to do with risk = 'out-of-bag', don't specify
nuirange # for other families not of interest to us

# mlr3proba example ----
learner = lrn('surv.glmboost', trace = TRUE, nu = 0.01, mstop = 500)
learner$train(task, train_indxs)
learner$model
preds = learner$predict(task, test_indxs)
preds # include `distr` since it was CoxPH
preds$score()
preds$score(msr('surv.graf'))

## Prediction mboost::survFit()
## `distr` uses the Breslow estimator (only for CoxPH family)
res = mboost::survFit(learner$model, newdata = task$data(rows = test_indxs))
surv = t(res$surv)
dim(surv) # patients x times (train set)
length(test_indxs) # patients
length(task$unique_event_times(rows = train_indxs)) # train event times used in the matrix
length(colnames(preds$data$distr)) # same as above
length(res$time) # same

all(preds$data$distr == surv)

# check another family distr output
learner$reset()
learner$param_set$values = list(family = 'loglog')
learner$train(task, train_indxs)
learner$model
preds2 = learner$predict(task, test_indxs)
preds2 # no distr!!!
preds2$score()

# importance ----
sort(mboost::varimp(learner$model)[-1], decreasing = TRUE)
plot(mboost::varimp(learner$model))

# selected_features ----
sel = unique(names(learner$model$model.frame())[learner$model$xselect()])
sel = sel[!(sel %in% "(Intercept)")]
sel

# mRNA train ----
## does a simple train run okay? does it have memory or other issues? NO, ALL COOL
task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA

learner = lrn('surv.glmboost', trace = TRUE, family = 'coxph', mstop = 50)
learner$train(task_mRNA)
learner$timings['train'] # ~ 1min
learner$predict(task_mRNA)$score()

learner = lrn('surv.glmboost', trace = TRUE, family = 'lognormal', mstop = 50)
learner$train(task_mRNA)
learner$predict(task_mRNA)$score()

learner = lrn('surv.glmboost', trace = TRUE, family = 'cindex', mstop = 50)
learner$train(task_mRNA)
learner$predict(task_mRNA)$score(msr('surv.cindex', weight_meth = 'G2'), task = task_mRNA, train_set = seq_len(task_mRNA$nrow)) # Uno's C-index, just for fun

# Tune ----
## CoxPH ----
glmboost_lrn = lrn('surv.glmboost',
  family = 'coxph',
  #center = TRUE, # default, change to `FALSE` if using already scaled data
  mstop = to_tune(p_int(50, 500)), # boosting iterations
  nu = to_tune(p_dbl(1e-03, 1, logscale = TRUE))
)

data.table::rbindlist(
  generate_design_random(glmboost_lrn$param_set$search_space(), 5)$transpose()
)

## AFT ----
# separate Gehan or not? (NO)
glmboost_lrn = lrn('surv.glmboost',
  family = to_tune(p_fct(c('weibull', 'loglog', 'lognormal', 'gehan'))),
  #center = TRUE, # default, change to `FALSE` if using already scaled data
  mstop = to_tune(p_int(50, 500)), # boosting iterations
  nu = to_tune(p_dbl(1e-03, 1, logscale = TRUE)) # learning rate
)

data.table::rbindlist(
  generate_design_random(glmboost_lrn$param_set$search_space(), 5)$transpose()
)

## Cindex (Uno) ----
## produces risk = -lp (like AFT models)
glmboost_lrn = lrn('surv.glmboost',
  family = 'cindex',
  sigma = to_tune(p_dbl(0.1, 0.5)),
  #center = TRUE, # default, change to `FALSE` if using already scaled data
  mstop = to_tune(p_int(50, 500)), # boosting iterations
  nu = to_tune(p_dbl(1e-03, 1, logscale = TRUE)) # learning rate
)

data.table::rbindlist(generate_design_random(glmboost_lrn$param_set$search_space(), 20)$transpose(), fill = TRUE)

glmboost_at = AutoTuner$new(
  learner = glmboost_lrn,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 20),
  tuner = tnr('mbo')
)
glmboost_at$train(task, train_indxs)

glmboost_at$archive$data %>% as_tibble()

# check if chosen learner has the hps it should (seems an issue at first?)
glmboost_at$learner$model
glmboost_at$tuning_result$x_domain

glmboost_at$predict(task, train_indxs)$score()
glmboost_at$predict(task, test_indxs)$score()

glmboost_at$archive$best()$x_domain[[1L]]
glmboost_at$tuning_result$learner_param_vals[[1]] # different!
glmboost_at$learner # different!

# let's train the learner with the best HP from the tuning
best_hpc = glmboost_at$archive$best()$x_domain[[1L]]
learner = lrn('surv.glmboost') # empty, untrained glmboost learner
learner$param_set$values =
  mlr3misc::insert_named(learner$param_set$values, best_hpc)
learner

learner$train(task, train_indxs)
learner$model

learner$predict(task, row_ids = train_indxs)$score() # 0.76
learner$predict(task, row_ids = test_indxs)$score() # 0.75

# some plots
autoplot(glmboost_at$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(glmboost_at$tuning_instance, type = 'performance')
