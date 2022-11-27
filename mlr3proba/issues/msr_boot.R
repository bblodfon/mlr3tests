# Make your own parallelized version of bootstrapping on a test set
library(mlr3verse)
library(mlr3proba)
library(dplyr)
library(tictoc)

# Task Lung ----
task = tsk('lung')
preprocess = po('encode') %>>% po('imputelearner', lrn('regr.rpart'))
task = preprocess$train(task)[[1]]
task$missings()
task

train_indx = 1:200
test_indx  = 201:task$nrow

# Learners ----
cox = lrn('surv.coxph')
rsf = lrn('surv.ranger', verbose = FALSE, num.threads = 16)

cox$train(task, train_indx)
rsf$train(task, train_indx)

cox$predict(task, test_indx)$score()
rsf$predict(task, test_indx)$score()

# Measures ----
harrell_c = msr('surv.cindex')
harrell_c$label = 'HarrellC'
uno_c = msr('surv.cindex', weight_meth = 'G2')
uno_c$label = 'UnoC'
ibrier = msr('surv.graf')
ibrier$label = 'IBrier'
measure = harrell_c # used for training
test_measures = list(harrell_c, uno_c) # used for bootstrap on the test set

# Use future_sapply() ----

# DOESN'T WORK as expected! NOT all workers are used

## inputs
# task, train_indx, test_indx, learner, measure
# nthreads = parallelly::availableCores()
nrsmps = 1000
checkmate::assert(nrsmps > 1)

t0 = cox$predict(task, test_indx)$score(harrell_c) %>% unname()
t0

system.time({t = sapply(1:nrsmps, function(n) {
  indx = sample(x = test_indx, size = length(test_indx), replace = TRUE)
  test_data = task$data(rows = indx)
  cox$predict_newdata(test_data)$score(harrell_c) %>% unname()
}, USE.NAMES = FALSE)})

library(future.apply)
future::plan('multisession', workers = 8)
RNGkind("L'Ecuyer-CMRG")

system.time({t = future.apply::future_sapply(1:nrsmps, function(n) {
  indx = sample(x = test_indx, size = length(test_indx), replace = TRUE)
  test_data = task$data(rows = indx)
  cox$predict_newdata(test_data)$score(harrell_c) %>% unname()
}, USE.NAMES = FALSE, future.seed = 42)})

my_boot = function(task, train_indx, test_indx, learner, measure, nrsmps, nthreads) {
  future::plan('multisession', workers = nthreads)
  RNGkind("L'Ecuyer-CMRG")

  t0 = learner$predict(task, test_indx)$score(measure) %>% unname()
  t = future.apply::future_sapply(1:nrsmps, function(n) {
    indx = sample(x = test_indx, size = length(test_indx), replace = TRUE)
    test_data = task$data(rows = indx)
    learner$predict_newdata(test_data)$score(measure) %>% unname()
  }, USE.NAMES = FALSE, future.seed = TRUE)

  future::plan('sequential')

  list(msr_label = measure$label, msr_id = measure$id, t0 = t0, t = t,
    t_mean = mean(t, na.rm = T), t_median = median(t, na.rm = T), R = nrsmps)
}

for(n in c(1,2,4,8,16)) {
  tic()
  res = my_boot(task, train_indx, test_indx, learner = cox,
    measure = harrell_c, nrsmps = 500, nthreads = n)
  toc()
}

# Use parallel() -----
library(parallel)

my_boot2 = function(task, train_indx, test_indx, learner, measure, nrsmps, nthreads) {
  cl = makeCluster(nthreads)
  clusterSetRNGStream(cl, 42)
  clusterExport(cl, varlist = c('task', 'train_indx', 'test_indx', 'learner',
    'measure'), envir = environment())

  t0 = unname(learner$predict(task, test_indx)$score(measure))
  t = parallel::parSapply(cl = cl, X = 1:nrsmps, function(i) {
    indx = sample(x = test_indx, size = length(test_indx), replace = TRUE)
    test_data = task$data(rows = indx)
    unname(learner$predict_newdata(test_data)$score(measure))
  }, USE.NAMES = FALSE, chunk.size = 250)

  stopCluster(cl)

  list(msr_label = measure$label, msr_id = measure$id, t0 = t0, t = t,
    t_mean = mean(t, na.rm = T), t_median = median(t, na.rm = T), R = nrsmps)
}

# sequential version of my_boot2()
my_boot3 = function(task, train_indx, test_indx, learner, measure, nrsmps) {
  t0 = unname(learner$predict(task, test_indx)$score(measure))
  t = sapply(1:nrsmps, function(i) {
    message(i)
    indx = sample(x = test_indx, size = length(test_indx), replace = TRUE)
    test_data = task$data(rows = indx)
    unname(learner$predict_newdata(test_data)$score(measure))
  }, USE.NAMES = FALSE)

  list(msr_label = measure$label, msr_id = measure$id, t0 = t0, t = t,
    t_mean = mean(t, na.rm = T), t_median = median(t, na.rm = T), R = nrsmps)
}

# 25 secs
tic()
res = my_boot3(task, train_indx, test_indx, learner = cox, measure = harrell_c,
  nrsmps = 1000)
toc()

# Best `nthreads` = 4, ~5 secs
for(n in c(2,4,8,16)) {
  message('Nthreads: ', n)
  tic()
  res = my_boot2(task, train_indx, test_indx, learner = cox, measure = harrell_c,
    nrsmps = 1000, nthreads = n)
  toc()
}

# Best `nthreads` = 4
for(n in c(4,8,16)) {
  message('Nthreads: ', n)
  tic()
  res = my_boot2(task, train_indx, test_indx, learner = rsf, measure = harrell_c,
    nrsmps = 1000, nthreads = n)
  toc()
}

xgb = lrn('surv.xgboost', nrounds = 30, eta = 0.01, max_depth = 2, nthread = 4,
  booster = 'gbtree', objective = 'survival:cox')
xgb$train(task, train_indx)
xgb$model

tic()
res = my_boot3(task, train_indx, test_indx, learner = xgb, measure = harrell_c,
  nrsmps = 1000)
toc()

# Uses ALL cores!!!
for(n in c(4,8,16)) {
  message('Nthreads: ', n)
  tic()
  res = my_boot2(task, train_indx, test_indx, learner = xgb, measure = harrell_c,
    nrsmps = 1000, nthreads = n)
  toc()
}

# mRNA bench ----
task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA

set.seed(42)
# ~70%/30% train/test split
train_indx = sample(seq_len(task_mRNA$nrow), 100)
test_indx  = setdiff(seq_len(task_mRNA$nrow), train_indx)

xgb = lrn('surv.xgboost', nrounds = 300, eta = 0.01, max_depth = 2, nthread = 4,
  booster = 'gbtree', objective = 'survival:cox')
xgb$train(task_mRNA, train_indx)
xgb$predict(task_mRNA, row_ids = test_indx)$score()

# xgb$param_set$values$nthread = 1 # doesn't work

tic()
res = my_boot3(task_mRNA, train_indx, test_indx, learner = xgb, measure = harrell_c,
  nrsmps = 1000)
toc()

for(n in c(2,4,8,16)) {
  message('Nthreads: ', n)
  tic()
  res = my_boot2(task = task_mRNA, train_indx, test_indx, learner = xgb,
    measure = harrell_c, nrsmps = 1000, nthreads = n)
  toc()
}

#' @param `data` data.table/data.frame object with the test data
#' that has the same structure (features and target columns)
#' as the `task` that was used to train the provided `learner`
#' on the `train_indx` rows
#' @param `learner` trained Learner object
#' @param `measure` an mlr3 `Measure`
#' @param task an mlr3 `Task`
#' @param train_indx row ids of the training set of the given `task`
calc_score = function(data, index, learner, measure, task, train_indx) {
  # do we need {task, train_indx} in the `$score()`?
  use_extra_params = extra_params_required(measure)

  if (!use_extra_params)
    learner$predict_newdata(data[index])$score(measure)
  else
    learner$predict_newdata(data[index])$score(measure, task = task, train_set = train_indx)
}

extra_params_required = function(measure) {
  use_extra_params = FALSE
  # Uno's C-index
  if (measure$id == 'surv.cindex' && measure$param_set$values$weight_meth == 'G2')
    use_extra_params = TRUE

  # Integrated Brier Score
  if (measure$id == 'surv.graf')
    use_extra_params = TRUE

  use_extra_params
}

# TODO ???
# Split to 4 x 250 and do these in parallel, though 1) data size and
# 2) parallel prediction capacity of some learners create CPU overhead, leading
# to very low or very high CPU usages
