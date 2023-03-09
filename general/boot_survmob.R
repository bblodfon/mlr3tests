library(mlr3verse)
library(mlr3proba)
library(dplyr, warn.conflicts = FALSE)
library(survmob)

# Task lung ----
task = tsk('lung')
preprocess = po('encode', method = 'treatment') %>>% po('imputelearner', lrn('regr.rpart'))
task = preprocess$train(task)[[1]]
print(task)

# rsf learner + train
part = partition(task, ratio = 0.8)
rsf = lrn('surv.ranger', verbose = FALSE, num.threads = 1)
rsf$train(task, part$train)

for (test_workers in c(1,4,8,12,16)) {
  print(paste0('#Workers: ', test_workers))
  br = BootRes$new(test_measure_ids = c('uno_c', 'ibrier'), test_nrsmps = 1000,
    test_workers = test_workers)
  br$calculate(task = task, learner = rsf, part = part, quiet = FALSE)
}

#' `4` cores on RSFs
# [1] "#Workers: 1"
# 81.919 sec elapsed
# [1] "#Workers: 4"
# 34.866 sec elapsed => BEST
# [1] "#Workers: 8"
# 36.327 sec elapsed
# [1] "#Workers: 12"
# 43.964 sec elapsed
# [1] "#Workers: 16"
# 53.313 sec elapsed

#' `1` core on RSFs (`num.threads` == 1)
#' - Same exactly results as above (execution time)
#' - More workers, more memory is used in general
#' - Even with `1` worker, `4` cores are used!!!!!!!!

# cox model
cox = lrn('surv.coxph')
cox$train(task, part$train)

for (test_workers in c(1,4,8,12,16)) {
  print(paste0('#Workers: ', test_workers))
  br = BootRes$new(test_measure_ids = c('uno_c', 'ibrier'), test_nrsmps = 1000,
    test_workers = test_workers)
  br$calculate(task = task, learner = cox, part = part, quiet = FALSE)
}

# [1] "#Workers: 1"
# 39.729 sec elapsed
# [1] "#Workers: 4" => BEST STILL (less CPU and memory used => due to simple cox model implementation)
# 18.317 sec elapsed
# [1] "#Workers: 8"
# 22.574 sec elapsed
# [1] "#Workers: 12"
# 30.726 sec elapsed
# [1] "#Workers: 16"
# 39.148 sec elapsed


# Task mRNA (1) ----
## 1000 features
#task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/task_mRNA_flt.rds')
task_mRNA = readRDS(file = gzcon(url('https://github.com/bblodfon/paad-survival-bench/blob/main/data/task_mRNA_flt.rds?raw=True')))
print(task_mRNA)
part = partition(task_mRNA, ratio = 0.8)
rsf = lrn('surv.ranger', verbose = FALSE, num.threads = 4)
rsf$train(task_mRNA, part$train)

for (test_workers in c(1,4,8,12,16)) {
  print(paste0('#Workers: ', test_workers))
  br = BootRes$new(
    test_measure_ids = c('uno_c', 'ibrier'),
    test_nrsmps = 1000,
    test_workers = test_workers
  )
  br$calculate(task = task_mRNA, learner = rsf, part = part, quiet = FALSE)
}

# [1] "#Workers: 1"
# 59.158 sec elapsed
# [1] "#Workers: 4"
# 26.767 sec elapsed => BEST STILL!!!!
# [1] "#Workers: 8"
# 31.762 sec elapsed
# [1] "#Workers: 12"
# 41.159 sec elapsed
# [1] "#Workers: 16"
# 50.949 sec elapsed

# Task mRNA (2) ----
## 10000 features
# task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA
