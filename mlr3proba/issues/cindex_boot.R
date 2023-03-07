# OLD TRIES
library(mlr3verse)
library(mlr3proba)
library(boot)

# Nice Articles
# https://elizavetalebedeva.com/bootstrapping-confidence-intervals-the-basics/
# https://www.r-bloggers.com/2019/09/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/
# from chapter 8 in the book L. Wasserman, All of Statistics (2004)
# https://link.springer.com/content/pdf/10.1007/978-0-387-21736-9_8.pdf

# hacked version with boot() ----
task = tsk('rats')
poe  = po('encode', method = 'treatment')
task = poe$train(list(task))[[1]]
task

learner = lrn('surv.coxph')

learner$train(task, row_ids = 1:200)

test_task = task$clone(deep = TRUE)$filter(rows = 201:300)
preds = learner$predict(test_task)
preds

cindex_harrell = msr('surv.cindex')
preds$score(cindex_harrell)

# the hack is that we need to pass a data.frame and then convert each time to a task
boot_fun = function(data, index, learner) {
  boot_task = mlr3proba::as_task_surv(x = data, time = 'time', event = 'status', id = 'rats')
  boot_task$filter(rows = index)
  learner$predict(boot_task)$score()
}

boot_fun(test_task$data(), sample(1:test_task$nrow), learner) # all
boot_fun(test_task$data(), sample(1:test_task$nrow, 42), learner) # random sample of 42
boot_fun(test_task$data(), sample(1:test_task$nrow, test_task$nrow, replace = TRUE), learner) # bootstrap

boot_res = boot(data = test_task$data(), statistic = boot_fun, R = 1000,
  parallel = 'multicore', ncpus = 4, learner = learner)
boot_res

boot_res$R
t0 = boot_res$t0
t = boot_res$t
sqrt(var(t))
sqrt(sum((t-mean(t))^2)/(length(t)-1)) # same as above

# Is statistic normally distributed?
plot(boot_res)

res = boot.ci(boot_res, type = c('basic', 'norm', 'perc', 'bca'))
res

# percentile (easiest to understand) - it's just the estimated values at
# (0.025, 0.975) percentiles of t
boot:::perc.ci

conf = 0.95
alpha = (1 + c(-conf, conf))/2
alpha
qq = boot:::norm.inter(t, alpha)
qq
# SOS => close to but not exactly the same with quantiles():
quantile(x = sort(t), probs = alpha)
res$percent # same as qq

# basic or empirical bootstrap
# 2*t0 +- quantiles at (0.025, 0.975) probabilities of t
boot:::basic.ci
sort(2*t0 - qq[,2])
res$basic

boot:::basic.ci(t0, t)

# normal interval (assumes normal distribution of the estimated data)
# t0 +- sqrt(var.t0) (SE) * qnorm((1 + conf)/2) # qnorm => quantile function of normal distribution
boot::norm.ci

res$normal
boot::norm.ci(t0 = t0, t = t)

# Accelerated Bootstrap or Bias Correction Bootstrap or
# adjusted bootstrap percentile (BCa) interval?
boot:::bca.ci

res$bca
# needs boot.out !!! can bypass though?
# boot:::bca.ci(t0 = t0, t = t)

# More nicely packed hacked version ----
#' `data` is a data.frame or equivalent
#' By default we convert `data` to `TaskSurv` (TODO: use should provide task type?)
boot_fun = function(data, index, learner, measure) {
  boot_task = mlr3proba::as_task_surv(x = data, time = 'time', event = 'status')
  boot_task$filter(rows = index)
  learner$predict(boot_task)$score(measure)
}

#' `task`: test data task
#' `nrsmps`: number of bootstrap resamplings
#' `measure`: one of `mlr_measures`
get_bootCIs = function(task, learner, measure = mlr3::msr('surv.cindex'), nthreads = 1, nrsmps = 1000) {
  if (!task$task_type %in% c('surv')) {
    print('Only Survival Tasks are supported!')
  } else {
    boot_res = boot::boot(data = task$data(), statistic = boot_fun, R = nrsmps,
      parallel = 'multicore', ncpus = nthreads, learner = learner, measure = measure)

    bootci_res = boot::boot.ci(boot_res, type = c('basic', 'norm', 'perc', 'bca'))

    return(list(boot_res = boot_res, bootci_res = bootci_res))
  }
}

#' `data` is a data.table/data.frame object
boot_fun2 = function(data, index, learner, measure) {
  learner$predict_newdata(data[index])$score(measure)
}

boot_fun2(data = test_task$data(), index = sample(1:test_task$nrow),
  learner, measure = mlr3::msr('surv.cindex')) # all
boot_fun2(data = test_task$data(), index = sample(1:test_task$nrow,
  test_task$nrow, replace = TRUE), learner, measure = mlr3::msr('surv.cindex'))

#' `test_data` is a data.table/data.frame object
get_bootCIs2 = function(test_data, learner, measure = mlr3::msr('surv.cindex'), nthreads = 1, nrsmps = 1000) {
    boot_res = boot::boot(data = test_data, statistic = boot_fun2, R = nrsmps,
      parallel = 'multicore', ncpus = nthreads, learner = learner, measure = measure)

    bootci_res = boot::boot.ci(boot_res, type = c('basic', 'norm', 'perc', 'bca'))

    return(list(boot_res = boot_res, bootci_res = bootci_res))
}

res = get_bootCIs(task = test_task, learner = learner, nthreads = 4)
res2 = get_bootCIs2(test_data = test_task$data(), learner = learner, nthreads = 1)
# more CPUs, faster:
res2 = get_bootCIs2(test_data = test_task$data(), learner = learner, nthreads = 4)

# using mlr3 (???) ----
library(mlr3pipelines)
?mlr_pipeops_subsample
subsample = po('subsample', param_vals = list(frac = 0.7, replace = TRUE))
graph = pipeline_greplicate(subsample, n = 10)

subsample$train(list(test_task))[[1L]]$data(rows = 1:5)

res = graph$train(test_task)
learner$predict(res[[1]])

ens.boot = subsample %>>% learner
g_rep = pipeline_greplicate(ens.boot, n = 10)
g_rep$train(task)
g_rep$predict(test_task)

boot = po('subsample', param_vals = list(frac = 1, replace = TRUE))
pred = po('predict') # task, learner, performance score
# replicate n times
# aggregate performance scores
# compute bootstrap CI intervals based on chosen hyperparams (e.g. method)
