# https://github.com/ropensci/aorsf/issues/16
library(mlr3proba)
library(mlr3extralearners)

task_mRNA = readRDS(file = gzcon(url('https://github.com/bblodfon/paad-survival-bench/blob/main/data/task_mRNA_flt.rds?raw=True'))) # 1000 features

#tasks = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')
#task_mRNA = tasks$mRNA

dsplit = mlr3::partition(task_mRNA, ratio = 0.8)
train_indxs = dsplit$train
test_indxs  = dsplit$test

orsf = lrn('surv.aorsf',
  importance = 'none',
  oobag_pred_type = 'surv',
  attach_data = FALSE,
  verbose_progress = TRUE,
  n_tree = 10
)

orsf$train(task = task_mRNA, row_ids = train_indxs)
#>  growing tree no. 9 of 10
p = orsf$predict(task = task_mRNA, row_ids = test_indxs)
#> Error: Assertion on 'length(times)' failed: FALSE.

# times and status from `$model` slot
test_task = task_mRNA$clone()$filter(rows = test_indxs)
time = orsf$model$data[[test_task$target_names[1]]]
status = orsf$model$data[[test_task$target_names[2]]]

# times and status from `task` (TRAINING)
targets = test_task$target_names
target = test_task$data(cols = targets)
time1 = target[[targets[1]]]
status1 = target[[targets[2]]]

