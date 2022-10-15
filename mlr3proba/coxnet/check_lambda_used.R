# glmnet test
# for prediction when trained with a single lambda, that one is selected in `surv.glmnet` for prediction!
# https://github.com/mlr-org/mlr3learners/blob/main/R/LearnerSurvGlmnet.R
# https://github.com/mlr-org/mlr3learners/blob/main/R/helpers_glmnet.R#L17

library(mlr3verse)
library(mlr3extralearners)
library(survival)
library(dplyr)
library(ggplot2)

# Preprocessing
lung = survival::lung
lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

task_lung = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
task_lung$missings()

# Imputation graph for missing values
impute_graph = po("copy", 1) %>>% po("imputehist")
task_lung = impute_graph$train(task_lung)[[1]]
task_lung$missings()
task_lung

# Glmnet Learner
learner = lrn('surv.glmnet')
learner$param_set$values = list(
  alpha = to_tune(0.5, 1),
  lambda = to_tune(p_dbl(0.001, 0.1))
)
measure = msr('surv.cindex')
terminator = trm("evals", n_evals = 10)
tuner = tnr("random_search")

#resampling = rsmp('cv', folds = 7)
resampling = rsmp('holdout')
#resampling = rsmp('repeated_cv', repeats = 5, folds = 7)

set.seed(42)
at = AutoTuner$new(learner, resampling, measure, terminator, tuner)
at$train(task_lung)

# check a resampling (the first)
vars = at$archive$data[1,] %>% select(alpha, lambda, surv.cindex, uhash)
rres = at$archive$resample_result(uhash = vars$uhash)
lps = rres$predictions()[[1]]$lp
rres$aggregate(measure) == vars$surv.cindex

score_1st_cv_iter = rres$score(measure)$surv.cindex[1]
score_1st_cv_iter
train_indexes = rres$resampling$train_set(1)
test_indexes  = rres$resampling$test_set(1)

# test glmnet produces the same linear predictors!
data = as.matrix(task_lung$data(cols = task_lung$feature_names))
train_data = data[train_indexes,]
test_data  = data[test_indexes,]
target = task_lung$truth()
train_target = target[train_indexes,]
test_target  = target[test_indexes,]
fit = glmnet::glmnet(x = train_data, y = train_target, family = "cox",
  alpha = vars$alpha, lambda = vars$lambda)
lps2 = predict(fit, newx = test_data, type = "link")
stopifnot(lps == lps2)


# another test (more simple)
glmnet_learner = lrn('surv.glmnet')
glmnet_learner$param_set$values = list(alpha = 0.4, lambda = 0.003)
glmnet_learner$train(task_lung)
res = glmnet_learner$predict(task_lung)
lps = res$lp
head(lps)

data = as.matrix(task_lung$data(cols = task_lung$feature_names))
target = task_lung$truth()
fit = glmnet::glmnet(x = data, y = target, family = "cox", alpha = 0.4, lambda = 0.003)
lps2 = predict(fit, newx = data, s = 0.003, type = "link")
lps3 = predict(fit, newx = data, type = "link")
stopifnot(lps3 == lps2)
head(lps2)
stopifnot(lps == lps2) # YES!!!
