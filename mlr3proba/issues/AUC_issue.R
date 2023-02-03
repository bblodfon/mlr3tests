library("mlr3verse")
library('mlr3extralearners')

#'###############################
#' Only Uno's AUC was trustworthy
#'###############################

task = tsk("rats")
task$select(c("litter", "rx"))
learner_lp = lrn("surv.glmnet")
learner_lp = lrn("surv.coxph")
learner_lp = lrn('surv.xgboost')
prediction_lp = learner_lp$train(task, row_ids = 1:280)$predict(task, row_ids = 281:300)

mlr_measures$keys(pattern = '^surv')
measures = msrs(c('surv.uno_auc', 'surv.chambless_auc', 'surv.hung_auc'))

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

