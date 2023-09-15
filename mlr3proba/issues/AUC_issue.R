library(mlr3verse)
library(mlr3extralearners)
library(mlr3proba)

#'###############################
#' Only Uno's AUC was trustworthy
#'###############################

library(mlr3proba)
task = tsk("rats")
task$select(c("litter", "rx"))
l = lrn("surv.coxph")
l = lrn("surv.rpart")
set.seed(1)
part = partition(task)
p = l$train(task, part$train)$predict(task, part$test)
p$score(msr('surv.hung_auc'), task = task, train_set = part$train)
p$score(msr('surv.song_auc'), task = task, train_set = part$train, learner = l)
p$score(msr('surv.chambless_auc'), task = task, train_set = part$train, learner = l)

#learner_lp = lrn("surv.glmnet")
learner_lp = lrn("surv.coxph")
#learner_lp = lrn('surv.xgboost')
l = lrn("surv.rpart")
set.seed(1)
part = partition(task)
prediction_lp = learner_lp$train(task, part$train)$predict(task, part$test)
p = l$train(task, part$train)$predict(task, part$test)

mlr_measures$keys(pattern = '^surv')
measures = msrs(c('surv.uno_auc', 'surv.chambless_auc', 'surv.hung_auc'))


# issue with `times` => https://github.com/mlr-org/mlr3proba/issues/315 (fixed now)
uno_auc = msr('surv.uno_auc', integrated = FALSE)
uno_auc$param_set$values$integrated # FALSE

uno_auc = msr('surv.uno_auc', times = 100)
uno_auc$param_set$values$times # 100

uno_auc$param_set$values$times = 100 # now it takes it
uno_auc

# SOS: you have to set parameters like below due to faulty initialization in the code!
# See: https://github.com/mlr-org/mlr3proba/blob/main/R/MeasureSurvUnoAUC.R#L29
# `times` is never set!
measure_uno = msr('surv.uno_auc')
measure_uno$param_set$values = list(integrated = TRUE, times = c(1,10,100))
# integrated means one result => if `times` not given, it's the sorted, unique test dataset times!
# https://github.com/mlr-org/mlr3proba/blob/HEAD/R/MeasureSurvAUC.R#L52
measure_uno$param_set$values = list(integrated = FALSE, times = 100) # times is required as a scalar???
measure_uno
prediction_lp$score(measures = measure_uno, task = task, train_set = 1:280)

train_set = 1:280

# get the full results directly from the function
Surv.rsp = task$truth(train_set)
Surv.rsp.new = prediction_lp$truth
lpnew = prediction_lp$lp
times = c(1,10,50,100,120)
#times = 100
survAUC::AUC.uno(Surv.rsp, Surv.rsp.new, lpnew, times)

measure_hung = msr('surv.hung_auc') # too high AUC? (there seems to be problem with the package I think)
measure_hung$param_set$values = list(integrated = TRUE, times = c(1,10,100))
measure_hung$param_set$values = list(integrated = FALSE, times = 45)
prediction_lp$score(measures = measure_hung, task = task, train_set = 1:280)

# Below issue is now Fixed! (`0.4.7`)
# Issue: Song's AUC requires lp but it's not there since `learner_lp$model$linear.predictors` is NULL, see:
# https://github.com/mlr-org/mlr3proba/blob/main/R/MeasureSurvAUC.R#L47
# Maybe this: `learner$predict(task, row_ids = train_set)$lp` is what is needed?
# The vector of predictors estimated from the training data from the `AUC.sh` documentation
measure_song = msr('surv.song_auc')
prediction_lp$score(measures = measure_song, task = task, train_set = 1:280, learner = learner_lp)

# Same Issue
measure_cham = msr('surv.chambless_auc')
prediction_lp$score(measures = measure_cham, task = task, train_set = 1:280, learner = learner_lp)

# Song and Chambless can only be used with CoxPH learner!!!

