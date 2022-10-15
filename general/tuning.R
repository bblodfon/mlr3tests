library(mlr3verse)
library(dplyr)
#library(mlr3tuning)

# Prerequisites ----
# Task, Learner, Resampling strategy, Performance measure
task = tsk('pima')
learner = lrn('classif.rpart')
hout = rsmp("holdout")
rsmp_cv = rsmp('cv', folds = 5)
measure = msr("classif.ce")

# Search Space ----
# (TuningInstance* Classes)

# ParamSet
?paradox::ps

search_space = ps(
  cp = p_dbl(lower = 0.001, upper = 0.1),
  minsplit = p_int(lower = 1, upper = 10)
)
search_space

# p_uty: untyped parameter
?paradox::ParamUty

# Tuning/Optimization budget: setting the termination criterion
as.data.table(mlr_terminators)

# 20 evaluations (i.e. resamplings) in total per parameter value
?mlr_terminators_evals
evals20 = trm("evals", n_evals = 20)
evals20
none = trm("none")

?mlr3tuning::TuningInstanceSingleCrit
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp_cv, # hout #(faster)
  measure = measure,
  search_space = search_space,
  terminator = evals20
)
instance

# Optimization/Tuning Algorithm ----
?mlr3tuning::mlr_tuners
?mlr_tuners_grid_search
?mlr_tuners_random_search

# simple grid search optimization algorithm
# 5^2 = 25 hyperparameter configurations, the tuner will stop after evaluating
# (in random order) 20 of these
tuner = tnr("grid_search", resolution = 5) # `batch_size` here!!!!!!!

tuner$optimize(instance)

instance
as.data.table(instance$archive) %>% as_tibble()

instance$result_learner_param_vals # best configuration
instance$result_y
instance$result # all above together

# score individual resampling results on a different measure
instance$archive$benchmark_result$score(msr("classif.acc"))

# Train Learner on best config (SOS!!!)
learner$param_set$values = instance$result_learner_param_vals
learner$train(task)

# Tune on many Perf. Measures (NICE) ----
?mlr_measures_classif.bacc
?mlr_measures_time_train
?mlr_measures_classif.ce
measures = msrs(c("classif.ce", "classif.bacc", "time_train"))

learner = lrn('classif.rpart')
instance = TuningInstanceMultiCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp_cv,
  measures = measures,
  search_space = search_space,
  terminator = evals20
)
instance

tuner$optimize(instance)

# choose a param config
instance$result %>% as_tibble() %>%
  arrange(desc(classif.bacc), classif.ce, time_train) %>% slice(1)

learner$param_set$values = instance$result_learner_param_vals[[1]]

learner$train(task, row_ids = 1:(task$nrow-1))
learner$predict(task, row_ids = task$nrow)
pred = learner$predict(task, row_ids = task$nrow)
pred$score(measures)

# AutoTune ----
?mlr3tuning::AutoTuner

learner = lrn("classif.rpart")
learner$model
search_space = ps(
  cp = p_dbl(lower = 0.001, upper = 0.1),
  minsplit = p_int(lower = 1, upper = 10)
)
terminator = trm("evals", n_evals = 12) # number of resamplings
tuner = tnr("random_search")

at = AutoTuner$new(
  learner = learner,
  resampling = rsmp("repeated_cv", repeats = 5, folds = 10),
  # Single performance measure, i.e. works only with `TuningInstanceSingleCrit`
  measure = msr("classif.ce"),
  search_space = search_space,
  terminator = terminator,
  tuner = tuner
)
at

class(at) # it's a `Learner`!

at$train(task)
at$learner # learner with best config
at$learner$param_set$values # best config
at$instance_args # see the AutoTuner's parameters

at$archive
at$archive$data %>% as_tibble() %>% arrange(classif.ce) %>% slice(1)

# Nested CV ----
learner = lrn("classif.rpart")
resampling = rsmp("repeated_cv", repeats = 5, folds = 5) # inner loop
#resampling = rsmp('cv', folds = 5)
measure = msr("classif.ce")
search_space = ps(cp = p_dbl(lower = 0.001, upper = 0.1))
terminator = trm("evals", n_evals = 10)
tuner = tnr("random_search")

at2 = AutoTuner$new(learner, resampling, measure, terminator, tuner, search_space)

task = tsk("pima")
outer_resampling = rsmp("cv", folds = 3)

rr = resample(task, at, outer_resampling, store_models = TRUE)

# Get average validation error across inner loop CV per configuration tried
archives = extract_inner_tuning_archives(rr) # 10 configs x 3 outer resamplings => 30 rows
archives
# `extract_inner_tuning_results` is the same as below:
archives %>% as_tibble() %>% group_by(iteration) %>% arrange(classif.ce) %>% slice(1)
# best hyperparameters and their CV error
res = extract_inner_tuning_results(rr)
res

rr$score() # should be higher than previous!!!

# unbiased performance of the model with optimal hyperparameter found by grid search:
rr$aggregate()
autoplot(rr)

# Final Model (SOS) ----
# You tune the hyperparameters of your learner and fit the final model on the FULL DATASET
at$train(task)
