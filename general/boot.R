# Example from issue ----
# https://github.com/mlr-org/mlr3pipelines/issues/681
library(mlr3pipelines)
library(mlr3verse)
library(mlr3proba)
library(tictoc)
library(future.apply)

# learner = lrn('classif.rpart')
boot_rsmp = po('subsample', frac = 1, replace = TRUE)
#
# task = tsk('iris')
# ids = partition(task)
#
# task_predict = task$clone()$filter(ids$test) # test task
# task$filter(ids$train) # task for training
#
# learner$train(task)
# learner # trained learner
#
# future::plan('multisession', workers = 4)
# future::plan('sequential')
#
# tic()
# res = future_sapply(1:300, function(i) {
#   set.seed(i)
#   task_subsampled = boot_rsmp$train(list(task_predict))[[1L]]
#   unname(learner$predict(task_subsampled)$score())
# }, future.seed = TRUE)
# toc()
#
# # performance scores
# res

# mRNA example ----
#task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA
task_mRNA = readRDS(file = gzcon(url('https://github.com/bblodfon/paad-survival-bench/blob/main/data/task_mRNA_flt.rds?raw=True'))) # 1000 features
task_mRNA$select(cols = task_mRNA$feature_names[1:100]) # 100 features
part = partition(task_mRNA, ratio = 0.8)

train_task = task_mRNA$clone()$filter(part$train)
test_task  = task_mRNA$clone()$filter(part$test)

learner = lrn('surv.ranger', num.threads = 1, verbose = FALSE, num.trees = 25)
learner$train(train_task)

future::plan('sequential')

if (FALSE) {
  # large memory footprint by generating large tasks (I think)
  tic()
  res = future_sapply(1:1000, function(i) {
    set.seed(i)
    boot_task = boot_rsmp$train(list(test_task))[[1L]] # <= HERE IS THE PROBLEM
    unname(learner$predict(boot_task)$score())
  }, future.seed = TRUE)
  toc()

  future::plan('multisession', workers = 10)
  tic()
  res = future_sapply(1:1000, function(i) {
    set.seed(i)
    boot_task = boot_rsmp$train(list(test_task))[[1L]] # <= HERE IS THE PROBLEM
    unname(learner$predict(boot_task)$score())
  }, future.seed = TRUE)
  toc()
}

# new version with no new task being generated
print('sequential')
future::plan('sequential')
tic()
res = future_sapply(1:1000, function(i) {
  set.seed(i)
  test_indx = part$test
  row_ids = sample(x = test_indx, size = length(test_indx), replace = TRUE)
  test_data = task_mRNA$data(rows = row_ids)
  unname(learner$predict_newdata(test_data)$score())
}, future.seed = TRUE)
toc()

print('4 workers')
future::plan('multisession', workers = 4)
tic()
res = future_sapply(1:1000, function(i) {
  set.seed(i)
  test_indx = part$test
  row_ids = sample(x = test_indx, size = length(test_indx), replace = TRUE)
  test_data = task_mRNA$data(rows = row_ids)
  unname(learner$predict_newdata(test_data)$score())
}, future.seed = TRUE)
toc()

print('8 workers')
future::plan('multisession', workers = 8)
tic()
res = future_sapply(1:1000, function(i) {
  set.seed(i)
  test_indx = part$test
  row_ids = sample(x = test_indx, size = length(test_indx), replace = TRUE)
  test_data = task_mRNA$data(rows = row_ids)
  unname(learner$predict_newdata(test_data)$score())
}, future.seed = TRUE)
toc()

print('16 workers')
future::plan('multisession', workers = 16)
tic()
res = future_sapply(1:1000, function(i) {
  set.seed(i)
  test_indx = part$test
  row_ids = sample(x = test_indx, size = length(test_indx), replace = TRUE)
  test_data = task_mRNA$data(rows = row_ids)
  unname(learner$predict_newdata(test_data)$score())
}, future.seed = TRUE)
toc()

# [1] "sequential"
# 26.867 sec elapsed
# [1] "4 workers"
# 15.678 sec elapsed
# [1] "8 workers"
# 20.658 sec elapsed
# [1] "16 workers"
# 35.257 sec elapsed
