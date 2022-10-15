library(mlr3verse)
library(mlr3proba)

task = tsk('rats')
poe  = po('encode', method = 'treatment')
task = poe$train(list(task))[[1]]

rr   = resample(task, lrn('surv.coxph'), rsmp('cv', folds = 5))

rr$score(msr('time_train')) # time_train is `NaN`

# learners have that information though:
for (learner in rr$learners) {
  print(learner$timings)
}

# mlr3@0.13.4 fixed!
rr$score(msr('time_train'))$time_train
rr$score(msr('time_predict'))$time_predict
