library(glmnet)
library(survival)
library(mlr3proba)
library(mlr3pipelines)
library(mlr3extralearners)
library(mlr3verse)
library(progressr)

task = tsk("rats")
cat_encode = as_graph(po("encode", method = "treatment"))
task = cat_encode$train(task)[[1L]]

#' add `strata` => we can't have a task$truth() that has class = stratifySurv
#' so glmnet doesn't use `strata` or `newstrata`
#task$col_roles$stratum = 'sex'
#task

#' add `weights`
#task$col_roles$weight = 'litter'
#task

set.seed(42)
part = partition(task, ratio = 0.9)

offset = rep(0.2, length(part$train))
newoffset = rep(0.1, length(part$test))

# cv_glmnet ----
if(FALSE) {
  # learner = lrn("surv.cv_glmnet", type.measure = 'C', alpha = 0.5, offset = offset, newoffset = newoffset)
  learner = lrn("surv.cv_glmnet", type.measure = 'C', alpha = 0.2, s = 4)
  learner

  learner$train(task, row_ids = part$train)
  learner$model$model
  learner$param_set$default$s
  learner$param_set$values$s
  p = learner$predict(task, row_ids = part$test)
  p

  p$distr[1]$survival(100)
  p$score()

  rr = resample(task, learner, resampling = rsmp('cv', folds = 3))
  rr$score()
}

# glmnet ----
if (FALSE) {
  learner = lrn("surv.glmnet", lambda = 0.03, offset = offset, newoffset = newoffset)

  learner = lrn("surv.glmnet", lambda = 0.03, alpha = 0.3)
  learner = lrn("surv.glmnet") # many lambdas fit

  learner
  learner$train(task, row_ids = part$train)
  #learner$selected_features(lambda = 0.03)

  p = learner$predict(task, row_ids = part$test)
  p

  p$distr[1]$survival(100)
  p$score()
  p$score(msr("surv.rcll", ERV = TRUE), task = task, train_set = part$train) # NICE!!!
  surv_mat = 1 - distr6::gprm(p$distr, 'cdf')
  dim(surv_mat) # obs x times

  model = learner$model$model

  # arguments used for training
  x    = as.matrix(task$data(rows = part$train, cols = task$feature_names))
  newx = as.matrix(task$data(rows = part$test , cols = task$feature_names))
  y = task$truth(rows = part$train)

  fit1 = survival::survfit(model, s = 0.03, x = x, y = y, newx = newx)
  fit2 = survival::survfit(model, s = 0.01, x = x, y = y, newx = newx)

  # some checks
  surv_mat1 = t(fit1$surv)
  colnames(surv_mat1) = fit1$time
  surv_mat2 = t(fit2$surv)
  colnames(surv_mat2) = fit2$time

  testthat::expect_equal(surv_mat1, surv_mat2) # NEVER, if `model` trained with many lambdas
  testthat::expect_equal(surv_mat , surv_mat2) # DEPENDS!!!
}

# Tune glmnet ----
lgr::get_logger('bbotk')$set_threshold('warn')
lgr::get_logger('mlr3')$set_threshold('warn')

# Progress bars
options(progressr.enable = TRUE)
handlers(global = TRUE)
handlers('progress')

learner = lrn('surv.glmnet')

# to tune: alpha and lambda
ss = paradox::ps(
  lambda = p_dbl(1e-03, 10, logscale = TRUE),
  alpha  = p_dbl(0, 1) # from Ridge to Lasso penalty
)

data.table::rbindlist(generate_design_random(ss, 6)$transpose())

#future::plan("sequential")
#future::plan("multisession", workers = 5)

future::plan("multicore", workers = 10)

at = AutoTuner$new(
  learner = learner,
  search_space = ss,
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.graf"),
  terminator = trm('evals', n_evals = 100),
  #tuner = tnr("mbo")
  tuner = tnr("random_search", batch_size = 20)
)

at$train(task, row_ids = part$train)
print(at$timings["train"])

p = at$predict(task, row_ids = part$test)
p$score(msrs(c("surv.cindex", "surv.rcll", "surv.graf")))

at$learner$model$model
at$archive
at$tuning_result$x_domain[[1]]
at$archive$best()$x_domain[[1]]
at$tuning_result$learner_param_vals

#at$tuning_instance
#autoplot(at$tuning_instance, type = 'parameter', trafo = TRUE)
#autoplot(at$tuning_instance, type = 'performance')
#autoplot(at$tuning_instance, type = 'surface', trafo = TRUE)
