library(mlr3verse)
library(mlr3proba)
library(survivalsvm)
library(survival)
library(tibble)

# survivalsvm ----
?survivalsvm # see the default HP values

as_tibble(veteran)

set.seed(42)
train_indxs = sample(seq_len(nrow(veteran)), 100)
test_indxs  = setdiff(seq_len(nrow(veteran)), train_indxs)
intersect(train_indxs, test_indxs)

fit = survivalsvm(Surv(time, status) ~ ., data = veteran,
  subset = train_indxs, gamma.mu = 0.1)
fit

preds = predict(fit, newdata = veteran, subset = test_indxs)
preds$predicted # returns survival times (higher survival time, less risk rank)

# C-index from `survivalsvm`
# need the `truth()` Surv object
ss = Surv(veteran$time, veteran$status)[test_indxs]
survivalsvm::conindex(obj = preds, Y = ss) # which uses the below function
Hmisc::rcorr.cens(x = preds$predicted, S = ss)

# mlr3proba example ----
task = as_task_surv(x = veteran, time = 'time', event = 'status')
learner = lrn('surv.svm', gamma.mu = 0.1)
learner$predict_types # includes `response`, `crank` => time-to-event is included always

learner$train(task, row_ids = train_indxs)
learner$model

preds2 = learner$predict(task, row_ids = test_indxs)
preds2
preds2$score() # maybe different than below (but close?)!
Hmisc::rcorr.cens(x = preds2$response, ss)['C Index']

# Testing HP params ----
if (FALSE) {
  learner = lrn('surv.svm', gamma.mu = 1, kernel = 'add_kernel')
  learner$train(task, row_ids = train_indxs)
  learner$predict(task, row_ids = test_indxs)$score()

  learner = lrn('surv.svm', gamma.mu = c(1, 0.1), opt.meth = 'ipop')
  learner$train(task, row_ids = train_indxs) # fails
  learner2 = survivalsvm(Surv(time, status) ~ ., data = veteran,
    subset = train_indxs, gamma.mu = c(1, 0.1)) # fails

  learner = lrn('surv.svm', gamma.mu = 1, type = 'vanbelle1')
  learner$train(task, row_ids = test_indxs)
  # Types 'vanbelle1', 'vanbell2' and 'hybrid' require an argument diff.meth.

  learner = lrn('surv.svm', gamma.mu = 1, kernel = 'rbf4_kernel') # not included
  learner = lrn('surv.svm', gamma.mu = 0.1, kernel = 'rbf_kernel')
  learner$train(task, row_ids = train_indxs)
  learner$model

  learner$predict(task, row_ids = test_indxs)$score()

  learner = lrn('surv.svm', gamma.mu = 0.1, kernel = 'add_kernel', type = 'hybrid', diff.meth = 'makediff1')
  learner$train(task) # gamma.mu needs to be a vector!
}

# Prediction outputs ----
learner1 = lrn('surv.svm',
  gamma.mu = 0.1, kernel = 'add_kernel',
  type = 'vanbelle1', diff.meth = 'makediff3')
p1 = learner1$train(task, row_ids = train_indxs)$
  predict(task, row_ids = test_indxs)
p1 # only crank
p1$score()

learner2 = lrn('surv.svm',
  gamma.mu = 0.1, kernel = 'add_kernel',
  type = 'regression')
p2 = learner2$train(task, row_ids = train_indxs)$
  predict(task, row_ids = test_indxs)
p2 # crank + response
p2$score()

learner3 = lrn('surv.svm',
  gamma.mu = c(0.1, 0.1), kernel = 'add_kernel',
  type = 'hybrid', diff.meth = 'makediff3')
p3 = learner3$train(task, row_ids = train_indxs)$
  predict(task, row_ids = test_indxs)
p3 # crank + response
p3$score()
