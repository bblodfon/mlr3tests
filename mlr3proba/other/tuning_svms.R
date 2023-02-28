# FAILS on a simple dataset sadly
# Optimizers I think are the issue
# and on biostat2, same code was using > 100 CPUs!
# Issues are two-fold: learner gets stucked (use timeout) or fails
# (use fallback kaplan learner)
# https://github.com/mlr-org/mlr3proba/issues/287
library(mlr3verse)
library(mlr3proba)
library(survivalsvm)

set.seed(42)
task = as_task_surv(x = veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]

train_indxs = sample(seq_len(nrow(veteran)), 120)
test_indxs  = setdiff(seq_len(nrow(veteran)), train_indxs)

# SVM learner (4 HPs) ----
learner = lrn('surv.svm',
  type = to_tune(c('regression', 'vanbelle1', 'vanbelle2', 'hybrid')),
  diff.meth = to_tune(c('makediff1', 'makediff2', 'makediff3')),
  gamma.mu = to_tune(ps(
    gamma = p_dbl(1e-03, 10, logscale = TRUE),
    mu    = p_dbl(1e-03, 10, logscale = TRUE, depends = type == 'hybrid'),
    .extra_trafo = function(x, param_set) {
      list(gamma.mu = c(x$gamma, x$mu))
    },
    .allow_dangling_dependencies = TRUE
  )),
  kernel = to_tune(c('lin_kernel', 'add_kernel', 'rbf_kernel', 'poly_kernel'))
)

# SVM learner (2 HPs) ----
learner = lrn('surv.svm',
  type = 'hybrid',
  diff.meth = 'makediff3',
  gamma.mu = to_tune(ps(
    gamma = p_dbl(1e-03, 10, logscale = TRUE),
    mu    = p_dbl(1e-03, 10, logscale = TRUE),
    .extra_trafo = function(x, param_set) {
      list(gamma.mu = c(x$gamma, x$mu))
    }
  )),
  kernel = to_tune(c('lin_kernel', 'add_kernel', 'rbf_kernel', 'poly_kernel'))
)

# saves you from when the learner crashes
learner$fallback = lrn('surv.kaplan')

# saves you from when the learner is stuck
learner$timeout = c('train' = 2, 'predict' = Inf)

# SVM regression learner ----
# ALSO FAILS DAMN IT!
# learner = lrn('surv.svm',
#   type = to_tune('regression'),
#   gamma.mu = to_tune(p_dbl(1e-03, 10, logscale = TRUE)),
#   kernel = to_tune(c('lin_kernel', 'add_kernel', 'rbf_kernel', 'poly_kernel')))

#learner$param_set$values$eig.tol  = 1e-03
#learner$param_set$values$conv.tol = 1e-03
#learner$param_set$values$posd.tol = 1e-03
#learner$param_set$values$opt.meth = 'ipop'
#learner$param_set$values$sigf = 2

#generate_design_random(learner$param_set$search_space(), 3)$transpose()

# Train AutoTuner ----
ssvm_at = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 30),
  tuner = tnr('random_search'))
ssvm_at$train(task)

# HP config that doesn't work ----
library(survivalsvm)
fit = survivalsvm(Surv(time, status) ~ ., data = veteran, type = 'hybrid',
  gamma.mu = c(0.76, 0.09), diff.meth = 'makediff3',
  kernel  = 'poly_kernel')
