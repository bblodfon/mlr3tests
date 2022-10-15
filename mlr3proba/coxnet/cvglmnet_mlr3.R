library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(survival)
library(tidyverse)

# Task ----
## Preprocessing
lung = survival::lung
lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

## convert to survival task
task_lung = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
class(task_lung)
task_lung$missings()

## Simple imputation pipeline for missing values
?mlr3pipelines::mlr_pipeops_imputelearner
impute_po = po('imputelearner', lrn('regr.rpart'))
task_lung = impute_po$train(list(task = task_lung))[[1]]

task_lung$missings()
task_lung

# learner ----
glmnetcv_lrn = lrn('surv.cv_glmnet', alpha = 1, # default `alpha` = 1 => LASSO
  nfolds = 5, type.measure = "C",
  standardize = TRUE, # default, important to keep it that way
  s = 'lambda.1se') # `s` is which lambda will be used after the CV is done when
# calling `predict` - can be also `lambda.min` or any number between 0 and 1
glmnetcv_lrn$param_set$default$s # default `s`

# Train/Predict (simple holdout) ----
## Dataset split (randomly in half)
set.seed(42)
train_indx = sample(seq_len(task_lung$nrow), task_lung$nrow/2)
test_indx  = setdiff(seq_len(task_lung$nrow), train_indx)
intersect(train_indx, test_indx) # no common elements, good!

## Train
glmnetcv_lrn$train(task = task_lung, row_ids = train_indx)
glmnetcv_lrn$model # our model and some summary
glmnetcv_lrn$model$lambda.min
glmnetcv_lrn$model$lambda.1se

## selected features by LASSO and their coefficients
glmnetcv_lrn$selected_features() # uses `s` lambda by default `lambda.1se`
glmnetcv_lrn$selected_features(lambda = glmnetcv_lrn$model$lambda.min)

## extract coef
coef_tbl = coef(glmnetcv_lrn$model, s = 'lambda.1se') %>% # s = 'lambda.min' different
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = 'coef_name') %>%
  dplyr::rename(value = `1`)
coef_tbl

## predict on test set
preds = glmnetcv_lrn$predict(task_lung, row_ids = test_indx)
preds$lp # the linear predictors for the patients in the test set

# how well we discriminate the test patients?
preds$score() # default => Harrell's C-index
preds$score(msr('surv.cindex')) # same as above

## the above used s = 'lambda.1se', we can change that to 'lambda.min':
glmnetcv_lrn$param_set$values$s = 'lambda.min'
preds2 = glmnetcv_lrn$predict(task_lung, row_ids = test_indx)
preds2$score() # different!

## predict survival for each individual patient in the test set
glmnetcv_lrn = lrn('surv.cv_glmnet', alpha = 1, # default `alpha` = 1 => LASSO
  nfolds = 5, type.measure = "C",
  standardize = TRUE, # default, important to keep it that way
  s = 'lambda.1se')
grlrn = ppl('distrcompositor',
  glmnetcv_lrn,
  estimator = 'kaplan', # or `nelson`
  form = 'ph') %>% as_learner()
grlrn$id = 'cvglmnet'
grlrn

set.seed(42)
grlrn$train(task = task_lung, row_ids = train_indx)
grlrn$model$surv.cv_glmnet$model # our model and some summary

preds3 = grlrn$predict(task = task_lung, row_ids = test_indx)
preds3 # also distr included!

# get survival info
preds3$distr$survival(times = c(5,50,500)) # rows = times, columns = patients-in-test-set

# the full survival matrix
surv_mat = preds3$data$distr
surv_mat

# Nested-CV ----
grlrn$reset() # empty model and have learner ready
rs = resample(task = task_lung, learner = grlrn,
  resampling = rsmp('cv', folds = 7), # out-loop CV folds
  store_models = TRUE)
res = rs$score() %>% as_tibble()
res$surv.cindex # unbiased C-indexes
rs$score(msr('surv.graf')) # Another measure of how calibrated/good were the `distr` predictions

# inspect an inside glmnet model
res$learner[[1]]$model$surv.cv_glmnet$model
