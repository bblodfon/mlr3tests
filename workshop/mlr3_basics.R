library(mlr3verse)
library(dplyr)

# Task
task = tsk("iris")
task

task$data()
task$nrow
task$ncol
task$col_info
task$col_roles
task$feature_names

?autoplot.TaskClassif
autoplot(task, type = 'target')
autoplot(task, type = 'duo')
autoplot(task, type = 'pairs')

# Tree Learner
mlr_learners
mlr_learners$keys(pattern = '^classif')

learner = lrn("classif.rpart")
learner
learner$model
learner$man
learner$predict_types
learner$predict_type = 'prob'
learner$param_set

# train (use subset of the task)
learner$train(task, row_ids = 1:120)

# this is what the decision tree looks like
learner$model
learner$importance

# predict
predictions = learner$predict(task, row_ids = 121:150)
predictions

# Performance Measures
predictions$confusion # confusion matrix

mlr_measures$keys(pattern = '^classif')
mm = msr('classif.auc')
mm$task_properties
predictions$score(mm) # fails (correct)
measures = msrs(c('classif.acc', 'classif.ce'))

# Test accuracy of our model on the test set of the final 30 rows
predictions$score(measures)

# Manual tune model
learner = lrn("classif.rpart")
learner$param_set$values = list(cp = 0.03, xval = 0)
learner$param_set
learner$train(task, row_ids = 1:120)

pred = learner$predict(task, row_ids = 121:150)
pred$confusion
pred$score(measures)

# Resamplings
mlr_resamplings

rs = rsmp('cv', folds = 5)
rs = rsmp('repeated_cv', folds = 5)
rs # not instantiated

set.seed(42)
rs$instantiate(task)

# Individual sets
rs$train_set(1)
rs$test_set(1)

# Disjunct sets
intersect(rs$train_set(1), rs$test_set(1))
intersect(rs$test_set(1), rs$test_set(2))

learner = lrn('classif.rpart')
rr = resample(task, learner, rs, store_models = TRUE)
rr

rr$predictions()

pred = rr$prediction()
pred # merged predictions

rr$score(measures)
rr$aggregate(measures) # CV error and accuracy
