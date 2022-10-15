library(mlr3verse)

mlr_learners

# (Standard) provided learners for survival
lrns = mlr_learners$mget(mlr_learners$keys("^surv"))
names(lrns)

# Get a learner ----
lrn_coxph  = mlr_learners$get('surv.coxph')
lrn_ranger = lrn('surv.ranger')

lrn_ranger$feature_types
lrn_ranger$packages
lrn_ranger$predict_types
lrn_ranger$properties # e.g. does it have importance, selected features?
lrn_ranger$param_set # VERY IMPORTANT!!!

learner = lrn('classif.rpart')
learner

learner$param_set$values # `xval` is different than the default!
learner$param_set$values = list(cp = 0.03, xval = 0)
learner$param_set$values
learner

# change prediction type
learner$predict_types
learner$predict_type = 'prob'
learner

# List measures for survival prediction ----
mlr_measures$keys('^surv')

# Change 2-Class Threshold ---
data("Sonar", package = "mlbench")
task = as_task_classif(Sonar, target = "Class", positive = "M")
learner = lrn("classif.rpart", predict_type = "prob")

# split train-test (80-20%)
set.seed(42)
train_size = round(0.8*task$nrow)
test_size  = task$nrow - train_size
checkmate::assert(train_size + test_size == task$nrow)

# SOS: use `setdiff(x, y)` to get test row IDs!!!
train_rows = sample(x = task$row_ids, replace = FALSE, size = train_size)
test_rows  = task$row_ids[!task$row_ids %in% train_rows]
pred = learner$train(task, row_ids = train_rows)$predict(task, row_ids = test_rows)

# Get a list of measures ----
mlr_measures$keys('^classif')
measures = msrs(c("classif.tpr", "classif.tnr", "classif.acc", "classif.auc"))
pred$confusion # confusion matrix
pred$score(measures)

# You can change the threshold after the predictions are made!
pred$set_threshold(0.2)
pred$confusion
pred$score(measures)

# Train & Predict ----
# NOTE: This is the standard 'holdout' resampling
task = tsk("penguins")
learner = lrn("classif.rpart")

# split train/test to 80-20%
set.seed(42)
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

# no model trained yet
learner$model

learner$train(task, row_ids = train_set)
learner$model
?rpart::print.rpart() # to understand what is printed
learner$model$variable.importance

prediction = learner$predict(task, row_ids = test_set)
prediction

as_tibble(as.data.table(prediction))
prediction$confusion

# change predict type
learner$predict_type
learner$predict_types
learner$predict_type = 'prob'
learner$predict_type

# re-fit the model
learner$train(task, row_ids = train_set)

# rebuild prediction object
prediction = learner$predict(task, row_ids = test_set)

as_tibble(as.data.table(prediction))
prediction$confusion
prediction$response
prediction$truth
prediction$prob
autoplot(prediction)

# performance evaluation
prediction$score()
mlr_measures$keys('^classif')
measures = msrs(c('classif.acc', 'classif.bacc'))
prediction$score(measures)
