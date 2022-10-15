# Filter methods assign an importance value to each feature
library(mlr3verse)

# Checking available filters ----

#' which filters can be used with a survival dataset?
#' check `task_type` property
for (key in mlr_filters$keys()) {
  task_types = flt(key)$task_type

  if ('surv' %in% task_types) print(key) # surv
  if (length(task_types) == 1 && is.na(task_types)) print(key) # NA
}

# AUC Filter ----
?mlr_filters_auc

## Calculate each separate feature's AUC score (in relation to target)
task = tsk('spam')
auc_flt = flt('auc')
auc_flt # applies only to two-class prediction problems (classification)
auc_flt$calculate(task)
as.data.table(auc_flt)[1:10]

## make a pipeline to get a new task
?mlr_pipeops_filter

## keep 5 features with best AUC (higher distance from random perf 0.5)
po_aucflt = po('filter', filter = flt('auc'), filter.nfeat = 5)
po_aucflt$state
po_aucflt$param_set

## filter the task and get extracted AUC scores!
filtered_task = po_aucflt$train(list(task))[[1]]
filtered_task$feature_names
head(po_aucflt$state$scores, 10)

# How is the AUC filter calculated? It is per feature!
best_auc_feat = po_aucflt$state$scores[1]
best_auc_feat # charExclamation

# they have their own auc function
auc = function(truth, prob) {
  n_pos = sum(truth)
  n_neg = length(truth) - n_pos
  if (n_pos == 0L || n_neg == 0L) {
    return(0.5) # nocov
  }
  r = rank(prob, ties.method = 'average')
  (sum(r[truth]) - n_pos * (n_pos + 1L) / 2L) / (n_pos * n_neg)
}

y = task$truth() == task$positive
x = task$data(cols = names(best_auc_feat)) # or all features cols = task$feature_names
score = mlr3misc::map_dbl(x, function(x) {
  keep = !is.na(x)
  auc(y[keep], x[keep])
})
best_auc_feat == abs(0.5 - score)

# Variance filter ----
task = tsk('spam')
task$ncol
flt('variance')$properties # can be applied to any task!
po_varflt = po('filter', mlr3filters::flt('variance'), filter.frac = 0.5)
task2 = po_varflt$train(list(task))[[1]]
task2$ncol
po_varflt$state$scores %>% dplyr::as_tibble()
head(as.data.table(po_varflt$filter), n = 10)

# Embedded filter selection ----
?mlr_filters_selected_features

task = tsk('spam')
sel_feat = flt('selected_features', learner = lrn('classif.rpart'))

sel_feat$calculate(task)
sum(sel_feat$scores) # some selected (1), some not (0)

sel_feat = flt('selected_features', learner = lrn('classif.rpart'))
# Note: All filter scores are either 0 or 1, i.e. setting `filter.cutoff = 0.5` means that we select all 'selected features'.
embed_flt = po('filter', filter = sel_feat, filter.cutoff = 0.5)
out_task = embed_flt$train(list(task))[[1L]]
out_task
