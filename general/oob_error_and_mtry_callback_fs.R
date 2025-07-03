library(mlr3verse)
library(tictoc)
library(mlr3fselect)
library(data.table)

# Issues ----
#' https://github.com/mlr-org/mlr3fselect/issues/67 (oob_error)
#' https://github.com/mlr-org/mlr3fselect/discussions/72 (adaptive `mtry.ratio`)
#' UPDATE: works with `mlr3fselect::0.9.1` and mlr3 `0.14.1@@849dd87` (for the `oob_error`)
task = tsk('spam')

# learner has `importance` and `oob_error` properties
learner = lrn('classif.ranger', num.threads = 10, num.trees = 50,
  importance = 'permutation', mtry.ratio = 0.1)

# so I can do the following
learner$train(task)
learner$importance() # for RFE
learner$oob_error()

mr = 0.5
n = length(task$feature_names)

callback_mtry = callback_batch_fselect('empty')
callback_mtry = callback_batch_fselect('mtry',
  on_eval_after_design = function(callback, context) {
    nfeats = length(context$design$task[[1]]$feature_names)
    # compute new mtry.ratio based on #features n
    #mtry.ratio = 1 - ((nfeats - 1) / (60 - 1))
    # new adaptive mtry.ratio formula
    mtry.ratio = mr^(log(nfeats)/log(n))

    context$design$learner[[1]]$param_set$set_values(mtry.ratio = mtry.ratio)
  })

learner = lrn('classif.ranger', num.threads = 10, num.trees = 50,
  importance = 'permutation', mtry.ratio = 0.1)

instance = FSelectInstanceBatchSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp('insample'),
  measure = msr('oob_error'),
  terminator = trm('none'),
  callbacks = callback_mtry,
  store_models = TRUE
)

fselector = fs('rfe', feature_fraction = 0.8, n_features = 1)

tic()
fselector$optimize(instance) # 4 secs
toc()

# hacky way to get the subset sizes
subset_sizes = unlist(lapply(as.data.table(instance$archive)$importance, length))
subset_sizes
ce = as.data.table(instance$archive)$oob_error # OOB miss-classification error
ce

# mtry.ratio increases
mtry_ratios = unlist(mlr3misc::map(as.data.table(instance$archive)$resample_result, function(rr) rr$learner$param_set$values$mtry.ratio))
mtry_ratios

dt = data.table(subset_size = subset_sizes, classif_error = ce, mtry.ratio = mtry_ratios)
dt[, mr1 := mr^(log(subset_size)/log(n))] # same formula as above
dt[, mtry1 := ceiling(mr1 * subset_size)]
dt

## Time with normal CV ----
instance2 = FSelectInstanceBatchSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp('cv', folds = 8),
  measure = msr('classif.ce'),
  terminator = trm('none'),
  callbacks = callback_mtry,
  store_models = TRUE
)

fselector2 = fs('rfe', feature_fraction = 0.8, n_features = 2)

tic()
fselector2$optimize(instance2) # 18 secs, definitely slower!
toc()

# hacky way to get the subset sizes
subset_sizes2 = unlist(lapply(as.data.table(instance2$archive)$importance, length))
subset_sizes2
ce2 = as.data.table(instance2$archive)$classif.ce # CV missclassification error
instance2$result_feature_set

# mtry.ratio increases
mtry_ratios2 = unlist(mlr3misc::map(as.data.table(instance2$archive)$resample_result, function(rr) rr$learner$param_set$values$mtry.ratio))

dt2 = data.table(subset_size = subset_sizes2, classif_error = ce2, mtry.ratio = mtry_ratios2)
dt2[, mtry_ratio_formula := 1 - ((subset_size - 1) / (60 - 1))] # add formula
dt2 # notice how close/similar are the CV-errors here with the oob_error in dt

## AutoFSelector ----
learner
task
at = AutoFSelector$new(
  learner = learner,
  resampling = rsmp('insample'), # TRAIN == TEST
  measure = msr('oob_error'),
  terminator = trm('none'),
  fselector = fs('rfe', feature_fraction = 0.8, n_features = 2),
  store_models = TRUE,
  callbacks = callback_mtry
)
at$train(task)

at$archive

# hacky way to get the subset sizes
subset_sizes3 = unlist(lapply(as.data.table(at$archive)$importance, length))
subset_sizes3
ce3 = as.data.table(at$archive)$oob_error # OOB missclassification error
ce3
at$fselect_result

# mtry.ratio increases
mtry_ratios3 = unlist(mlr3misc::map(as.data.table(at$archive)$resample_result, function(rr) rr$learner$param_set$values$mtry.ratio))

# see table with all
dt3 = data.table(subset_size = subset_sizes3, classif_error = ce3, mtry.ratio = mtry_ratios3)
dt3[, mtry_ratio_formula := 1 - ((subset_size - 1) / (60 - 1))] # add formula
dt3
