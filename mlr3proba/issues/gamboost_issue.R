library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)

task = tsk("actg")
task

learner = lrn("surv.gamboost", baselearner = 'bols')
learner$train(task)
learner$importance()

learner = lrn("surv.gamboost", baselearner = 'btree')
learner$train(task)
learner$importance()

# fails!!!
learner = lrn("surv.gamboost", baselearner = 'bbs', dfbase = 4)
learner$train(task)