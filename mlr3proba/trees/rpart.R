library(mlr3verse)
library(mlr3proba)
library(survival)
library(rpart)

# Task Lung ----
task = tsk('lung')
task$missings()
task$data()$sex

preprocess = po('encode') %>>% po('imputelearner', lrn('regr.rpart'))
task = preprocess$train(task)[[1]]
task$missings()


# Native call ----
## hyperparameters
?rpart.control

# Formula says: use all variables from given data argument!!!
task$formula()

# fit rpart
fit = rpart::rpart(formula = task$formula(), data = task$data(),
  method = "exp", model = TRUE, control = rpart.control(minsplit = 3, cp = 0.01))
fit

# outputs some kind of ranking (higher is worse)
predict(fit, newdata = task$data()[1:5,])

# same
res = predict(fit, newdata = task$data()[1:5,], type = "vector")
res

# mlr3proba example ----
## you want at least `minsplit` obs in a node to perform a split
rpart_lrn = lrn('surv.rpart', minsplit = 3, cp = 0.01)

rpart_lrn$train(task)
rpart_lrn$model
pred = rpart_lrn$predict(task, row_ids = 1:5)
pred # only crank!
stopifnot(pred$crank == res) # same results as above

# SOS => predict distr as well!
?mlr_pipeops_compose_distr
rpart_graph_lrn = ppl('distrcompositor',
  rpart_lrn,
  estimator = 'nelson',
  form = 'aft',
  overwrite = TRUE,
  graph_learner = TRUE,
  id = 'Survival Tree'
)

rpart_graph_lrn$train(task)
pred2 = rpart_graph_lrn$predict(task, row_ids = 1:5)
pred2
stopifnot(pred2$crank == res)

# get survival probabilities
pred2$distr$survival(times = c(1, 100, 300, 700, 1000))

# Tuning example ----
set.seed(42)
train_indxs = sample(seq_len(task$nrow), 180) # 80% for training
test_indxs  = setdiff(seq_len(task$nrow), train_indxs)
intersect(train_indxs, test_indxs)

rpart_lrn = lrn('surv.rpart',
  # minsplit => at least that number of obs in a node to do a split
  minsplit = to_tune(p_int(1, 100, logscale = TRUE)), # minbucket = minsplit/3
  cp = to_tune(1e-04, 1, logscale = TRUE))

a = dplyr::bind_rows(
  generate_design_random(rpart_lrn$param_set$search_space(), 20)$transpose()
)
a

minbucket = ceiling(a$minsplit / 3)
minbucket

rpart_at = AutoTuner$new(
  learner = rpart_lrn,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 20), # 10 - 100
  tuner = tnr('mbo')
)
rpart_at$train(task, row_ids = train_indxs)

# see params used
rpart_at$learner$model$control

p = rpart_at$predict(task, row_ids = test_indxs)
p
p$score()

autoplot(rpart_at$tuning_instance, type = 'surface', trafo = TRUE)
autoplot(rpart_at$tuning_instance, type = 'performance')
