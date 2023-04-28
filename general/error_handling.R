#' from https://mlr3book.mlr-org.com/technical.html#sec-error-handling
library(mlr3verse)

task = tsk("penguins")
learner = lrn("classif.debug")
print(learner)

learner$param_set

# works (predicts one random class only)
learner$train(task)$predict(task)$confusion

# set probability to signal an error to 1
learner$param_set$values = list(error_train = 1)

# error
learner$train(task)

# encapsulation => capture the errors, warnings, etc.
task = tsk("penguins")
learner = lrn("classif.debug")

# this learner throws a warning and then stops with an error during train()
learner$param_set$values = list(warning_train = 1, error_train = 1)

# enable encapsulation for train() and predict()
learner$encapsulate = c(train = "evaluate", predict = "evaluate")

learner$train(task)
learner$log
learner$warnings
learner$errors

# add segmentation fault during training
learner$param_set$values = list(segfault_train = 1)
learner$reset()
#learner$train(task) # SOS => THIS WILL FUCK UP YOUR R SESSION FOR SURE

learner$encapsulate = c(train = "callr", predict = "callr")
learner$param_set$values = list(segfault_train = 1)
learner$train(task = task)
learner$errors

# 5 minute timeout for training, no timeout for predict
learner$timeout = c(train = 5 * 60, predict = Inf)

