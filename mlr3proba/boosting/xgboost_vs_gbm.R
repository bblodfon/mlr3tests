# see if parallelization helps xgboost or not
# who is faster? (especially on larger datasets)
library(mlr3verse)
library(mlr3proba)
library(tibble)
library(survival)
library(rbenchmark)

# Veteran task ----
if (FALSE) {
  as_tibble(veteran)
  set.seed(42)
  train_indxs = sample(seq_len(nrow(veteran)), 100)
  test_indxs  = setdiff(seq_len(nrow(veteran)), train_indxs)
  intersect(train_indxs, test_indxs)

  task = as_task_surv(x = veteran, time = 'time', event = 'status')
  poe = po('encode')
  task = poe$train(list(task))[[1]]
  task
}

# mRNA task ----
tasks = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')
task = tasks$mRNA

# learners ----
nrounds = 2000
eta = 0.01
depth = 7

gbm = lrn('surv.gbm', n.trees = nrounds, interaction.depth = depth,
  shrinkage = eta, bag.fraction = 1, verbose = FALSE)
xgboost1 = lrn('surv.xgboost', nrounds = nrounds, eta = eta, max_depth = depth,
  nthread = 1)
xgboost2 = lrn('surv.xgboost', nrounds = nrounds, eta = eta, max_depth = depth,
  nthread = 2)
xgboost4 = lrn('surv.xgboost', nrounds = nrounds, eta = eta, max_depth = depth,
  nthread = 4)
xgboost8 = lrn('surv.xgboost', nrounds = nrounds, eta = eta, max_depth = depth,
  nthread = 8)
xgboost16 = lrn('surv.xgboost', nrounds = nrounds, eta = eta, max_depth = depth,
  nthread = 16)
xgboost32 = lrn('surv.xgboost', nrounds = nrounds, eta = eta, max_depth = depth,
  nthread = 32)

# benchmark ----
within(rbenchmark::benchmark(
  "gbm" = { gbm$train(task) },
  "xgboost1" = { xgboost1$train(task) },
  "xgboost2" = { xgboost2$train(task) },
  "xgboost4" = { xgboost4$train(task) },
  "xgboost8" = { xgboost8$train(task) },
  "xgboost16" = { xgboost16$train(task) },
  "xgboost32" = { xgboost16$train(task) },
  replications = 10,
  columns = c("test", "replications", "elapsed", "relative") # elapsed is in secs
), { average_min = (elapsed/replications)/60 })

# see screenshot: `bench.png`