#' `multicore` FASTER than `multisession`
#' More cores with `multicore` => faster, `multisession` 'throttles' (`4` cores
#' seemed to be the best in XPS 13)
#' RSF parallelization didn't seem to affect speed-up no matter the config
#' XGB parallelization problems: works only with 1 worker (sequential) +
#' setting `nthread` > `1` (worked alright)
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(dplyr, warn.conflicts = FALSE)
library(survmob)

cores = c(1,2,4,8,16)
bootest = function(cores, task, learner, part) {
  # train learner on train set
  learner$reset()
  learner$train(task, part$train)

  # run the bootstrap test
  for (test_workers in cores) {
  print(paste0('#Workers: ', test_workers))
  br = BootRes$new(
    test_measure_ids = c('uno_c', 'ibrier'),
    test_nrsmps = 1000,
    test_workers = test_workers
  )
  br$calculate(task = task, learner = learner, part = part, quiet = FALSE)
  }
}

# Task lung ----
task_lung = tsk('lung')
preprocess = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task_lung = preprocess$train(task_lung)[[1]]
print(task_lung)
part_lung = partition(task_lung, ratio = 0.8)

## CoxPH ----
print('CoxPH')
cox = lrn('surv.coxph')
bootest(cores, task = task_lung, learner = cox, part = part_lung)

#' `multisession`
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

#' `multicore`
# [1] "#Workers: 1"
# 39.639 sec elapsed
# [1] "#Workers: 4"
# 10.773 sec elapsed
# [1] "#Workers: 8"
# 8.429 sec elapsed
# [1] "#Workers: 12"
# 6.518 sec elapsed
# [1] "#Workers: 16"
# 5.627 sec elapsed

## RSF ----
print('RSF')
rsf = lrn('surv.ranger', verbose = FALSE, num.threads = 8)
bootest(cores, task = task_lung, learner = rsf, part = part_lung)

#' `4` cores on RSFs (multisession)
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

#' `multicore` results are WAY BETTER!!!
# [1] "#Workers: 1"
# 79.865 sec elapsed
# [1] "#Workers: 4"
# 26.69 sec elapsed
# [1] "#Workers: 8"
# 20.087 sec elapsed
# [1] "#Workers: 12"
# 20.271 sec elapsed
# [1] "#Workers: 16"
# 19.759 sec elapsed

## XGBoost ----
print('XGBoost')
xgb = mlr3pipelines::ppl('distrcompositor',
  learner = lrn('surv.xgboost', nthread = 4,
    nrounds = 500, eta = 0.03, max_depth = 5,
    booster = 'gbtree', fallback = lrn('surv.kaplan'),
    objective = 'survival:cox', id = 'XGBoostCox'),
  estimator = 'kaplan',
  form = 'ph',
  overwrite = TRUE,
  graph_learner = TRUE
)
xgb$id = 'XGBoostCox'
bootest(cores = 1, task = task_lung, learner = xgb, part = part_lung)

# Task mRNA (1) ----
## 1000 features
#task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/task_mRNA_flt.rds')
task_mRNA = readRDS(file = gzcon(url('https://github.com/bblodfon/paad-survival-bench/blob/main/data/task_mRNA_flt.rds?raw=True')))
print(task_mRNA)
part_mRNA = partition(task_mRNA, ratio = 0.8)

## RSF ----
print('RSF')
bootest(cores, task = task_mRNA, learner = rsf, part = part_mRNA)

#' `multisession`
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

#' `multicore`
# [1] "#Workers: 1"
# 60.37 sec elapsed
# [1] "#Workers: 4"
# 19.015 sec elapsed
# [1] "#Workers: 8"
# 13.423 sec elapsed
# [1] "#Workers: 12"
# 12.649 sec elapsed
# [1] "#Workers: 16"
# 12.422 sec elapsed

## XGBoost ----
print('XGBoost')
bootest(cores = 1, task = task_mRNA, learner = xgb, part = part_mRNA)

# Task mRNA (2) ----
## 10000 features
# task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA
