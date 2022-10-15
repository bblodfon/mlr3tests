library(mlr3)

# Task (Data)
#task = mlr_tasks$get('iris')
task = tsk("iris")

# Learner (Classifier)
learner = lrn("classif.rpart")

# train a model of this learner for a subset of the task
learner$train(task, row_ids = 1:120)

# this is what the decision tree looks like
learner$model

predictions = learner$predict(task, row_ids = 121:150)
predictions

# prediction performance
predictions$score(measures = msr('classif.acc'))

