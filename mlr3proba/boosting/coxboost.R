# CoxBoost is likelihood Based Boosting not gradient-based
# CoxBoost fits a Cox proportional hazards model
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(tidyverse)
library(mlr3mbo)
library(survival) # for task veteran
library(CoxBoost) # binderh/CoxBoost@1dc47d7

# Hyperparameters ----
learner = lrn('surv.coxboost')
?CoxBoost::CoxBoost() # few HPs
learner$help()

# if covariates are 0-1, computation is much faster!
learner$param_set$default$x.is.01 # FALSE
# if you don't want the `scoremat` (stepno x p) in the output, set to FALSE
learner$param_set$default$return.score # TRUE
# don't penalize these covariates (e.g. use for clinical data features!!!)
unpen.index = c(1,2)
# Allows to penalize subset of features
# For example in a microarray setting, the (many) microarray features would
# be taken to be optional covariates, and the (few) potential clinical
# covariates would be taken to be mandatory, by including their indices in
# `unpen.index`

## to tune or set
return.score = FALSE
standardize = FALSE # MANDATORY VARS ARE NOT STANDARDIZED
stepno = to_tune(50, 5000) # number of boosting steps
unpen.index = c(1,2)
penalty = 9 * sum(status == 1) # seems arbitary
stepsize.factor # 1 default, can be < 1 and > 1

# penalty = 9 * sum(status[subset] == 1) in the function call,
# seems very arbitrary
# paper says put it 'large' enough and don't tune
penalty = to_tune(p_dbl(10, 1000, logscale = TRUE))
# `hscore` for heuristic score (chooses a subset of covariates, faster)
# `score` should be slower than `pscore` for p > 100 features - Benchmark!!!
criterion = c('pscore', 'score', 'hpscore', 'hscore')

learner$param_set$default$criterion # pscore
criterion = to_tune(c('pscore', 'hpscore'))
# indicates the criterion to be used for selection in each boosting step.
# "pscore" corresponds to the penalized score statistics, "score" to the
# un-penalized score statistics.
# Different results will only be seen for un-standardized covariates ("pscore" will result in preferential selection of covariates with larger
# covariance), or if different penalties are used for different covariates

learner$param_set$default$sf.scheme
sf.scheme	= 'sigmoid' # to_tune(c('sigmoid', 'linear')) # shouldn't matter
# scheme for changing step sizes (via stepsize.factor)

# constant penalty for variables that are chosen on the update scheme
# like the `learning rate` => DON'T TUNE according to paper, leave it at 1
learner$param_set$default$stepsize.factor
stepsize.factor = to_tune(p_dbl(0.1, 10, logscale = TRUE))

learner$param_set$default$trace # FALSE (TRUE => prints variable names, useless)
at_step = NULL # default => predict using the last boosting step

# Example (from package) ----
## Generate some survival data with 10 informative covariates
n = 200
p = 100
beta = c(rep(1,10),rep(0,p-10))
x = matrix(rnorm(n*p),n,p)
real.time = -(log(runif(n)))/(10*exp(drop(x %*% beta)))
cens.time = rexp(n,rate=1/10)
status = ifelse(real.time <= cens.time,1,0)
obs.time = ifelse(real.time <= cens.time,real.time,cens.time)

## fit a Cox proportional hazards model by CoxBoost
cbfit = CoxBoost::CoxBoost(time = obs.time, status = status, x = x,
  stepno = 100, penalty=100)
summary(cbfit)

## get model coefficient
coef(cbfit)
cbfit$coefficients[101,] # does one more repetition? don't use since it doesn't include names of the variables!

## ... with covariates 1 and 2 being mandatory
cbfit.mand = CoxBoost::CoxBoost(time=obs.time, status=status, x=x, unpen.index=c(1,2),
  stepno=100, penalty=100)
summary(cbfit.mand)
coef(cbfit.mand)

# estimate p-values
?CoxBoost::estimPVal
#set.seed(42) # doesn't help with reproducibility
p1 = estimPVal(cbfit,x,permute.n=10)

# get a second vector of estimates for checking how large random variation is
p2 = estimPVal(cbfit,x,permute.n=10)
plot(p1,p2,xlim=c(0,1),ylim=c(0,1),xlab="permute 1",ylab="permute 2")

# Veteran task ----
task = as_task_surv(x = survival::veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]
task

set.seed(42)
train_indxs = sample(seq_len(task$nrow), 100)
test_indxs  = setdiff(seq_len(task$nrow), train_indxs)
intersect(train_indxs, test_indxs)

# Native call ----
data = task$data()[train_indxs]
tn = task$target_names
time = data[[tn[1L]]]
status = data[[tn[2L]]]
data = as.matrix(data[, !tn, with = FALSE])

fit = CoxBoost::CoxBoost(time = time, status = status, x = data,
  standardize = FALSE, return.score = FALSE, stepno = 200, penalty = 100,
  criterion = 'pscore')
coef(fit)

fit2 = CoxBoost::CoxBoost(time = time, status = status, x = data,
  standardize = TRUE, return.score = FALSE, stepno = 200, penalty = 100,
  criterion = 'pscore')
coef(fit2)

?predict.CoxBoost
preds1 = predi3ct(fit, newdata = task$data(cols = task$feature_names)[test_indxs], type = 'lp')
preds1 # lps!

preds2 = predict(fit2, newdata = task$data(cols = task$feature_names)[test_indxs], type = 'lp')
preds2

# mlr3proba example ----
learner = lrn('surv.coxboost', standardize = TRUE, return.score = FALSE,
  stepno = 200, penalty = 100, criterion = 'pscore')
learner

learner$train(task, row_ids = train_indxs)
learner$model

preds = learner$predict(task, row_ids = test_indxs)
preds # lp = crank
all(preds$lp == preds1) # same, OK!!!
preds$score() # 0.74

# mlr3proba example with unpenalized features
learner = lrn('surv.coxboost', standardize = TRUE, return.score = FALSE,
  stepno = 200, penalty = 100, criterion = 'pscore', unpen.index = c(1,3,5))
learner

learner$train(task, row_ids = train_indxs)
learner$model

# CoxPH ----
cox = lrn('surv.coxph')
cox$train(task, row_ids = train_indxs)$
    predict(task, row_ids = test_indxs)$
    score() # 0.74 (baseline)

# Tune CoxBoost ----
coxboost_learner = lrn('surv.coxboost', standardize = FALSE, # check if you need to change this
  return.score = FALSE, # don't need this
  stepno = to_tune(p_int(50, 500)), # up to 5000 or too much?
  penalty = to_tune(p_int(10, 1000, logscale = TRUE)), # leave at default?
  stepsize.factor = to_tune(p_dbl(1e-01, 10, logscale = TRUE)), # leave at default - 1?
  criterion = to_tune(p_fct(c('pscore', 'hpscore'))) # penalized scores
)
# `score` should be slower than `pscore` for p > 100 features - Benchmark!!!

data.table::rbindlist(
  generate_design_random(learner$param_set$search_space(), 5)$transpose()
)

coxboost_at = AutoTuner$new(
  learner = coxboost_learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 20), # 10 - 100
  tuner = tnr('mbo')
)
coxboost_at$train(task, row_ids = train_indxs)

# check if chosen learner has the hps it should (OK!)
coxboost_at$tuning_result
coxboost_at$learner$model$stepno
coxboost_at$learner$model

coxboost_at$timings['train']

p = coxboost_at$predict(task, row_ids = test_indxs)
p # crank, lp, distr
p$score() # For this small dataset, performance is equal to CoxPH!!!

# check surrogate
coxboost_at$tuner$surrogate$model

autoplot(coxboost_at$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(coxboost_at$tuning_instance, type = 'performance')
