library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)

# task
task = tsk("sonar")
part = partition(task)

# learner
learner = lrn("classif.xgboost", nrounds = 5000, early_stopping_rounds = 10,
              eval_metric = "logloss") # default for binary classif
pca = po("pca")
grlrn = pca %>>% learner |> as_learner()
grlrn

# simple train/test
set_validate(grlrn, validate = 0.2) # 20% of the training data will be used for validation
grlrn$train(task, row_ids = part$train)
grlrn$model$classif.xgboost$model$best_iteration # early stopping was used
grlrn$model$classif.xgboost$model$evaluation_log # internal evaluation log
grlrn$predict(task, row_ids = part$test)$score()

# do a 10-fold CV
grlrn$reset()
set_validate(grlrn, validate = "test") # use always the test fold as validation fold
rr = resample(task, grlrn, rsmp("cv"))
rr$score()
rr$aggregate()

# so a simple train/test while (internal/early-stopped) tuning
search_space = ps(
  # note the pipeop `id` in-front of all hyperparameters!
  ## preprocessing hyperparameter, how many PCA components?
  pca.rank. = p_int(2, 20),
  ## learning rate tuning hyperparameter
  classif.xgboost.eta = p_dbl(0.001, 0.1, logscale = TRUE),
  ## early-stopped tuned hyperparameter
  classif.xgboost.nrounds = p_int(upper = 500, tags = "internal_tuning",
    ## this function takes the mean of all the early-stopped nrounds for the final model
    aggr = function(x) as.integer(mean(unlist(x))))
)

at = auto_tuner(
  tuner = tnr("grid_search"),
  search_space = search_space,
  learner = grlrn,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  term_evals = 10L
)
at$train(task, row_ids = part$train)
# final model ('early-stopped' as you can see from the nrounds, also pca's rank. and xgboost eta have been tuned)
at$model$learner
at$predict(task, row_ids = part$test)$score()

# similar at is a model so you can passit to resample, benchmark, etc.
