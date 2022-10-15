# Nested CV - tune both (lambda, a) parameters
library(mlr3verse)
library(mlr3proba)
library(survival)
library(tictoc)
suppressMessages(library(dplyr))
library(future)

# Task Lung ----
# Preprocessing
lung = survival::lung
lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

task_lung = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
#task_lung$missings()

# Simple imputation pipeline for missing values
#?mlr3pipelines::mlr_pipeops_imputelearner
impute_po = po('imputelearner', lrn('regr.rpart'))
task_lung = impute_po$train(list(task = task_lung))[[1]]
#task_lung$missings()

# Nested CV: glmnet ----
glmnet_lrn = lrn('surv.glmnet', standardize = FALSE,
  lambda = to_tune(p_dbl(0.001, 0.1, logscale = TRUE)),
  alpha = to_tune(0, 1))
harrell_cindex = msr('surv.cindex')
num_configs = 30 # change to more configurations if needed
eval_trm = trm("evals", n_evals = num_configs)
rand_search_tuner = tnr("random_search")

inner_folds = 6
inner_resampling = rsmp('cv', folds = inner_folds) # inner folds (5-10)

glmnet_at = AutoTuner$new(
  learner = glmnet_lrn,
  resampling = inner_resampling,
  measure = harrell_cindex,
  terminator = eval_trm,
  tuner = rand_search_tuner
)

outer_folds = 4
outer_resampling = rsmp("cv", folds = outer_folds) # outer folds (3-10)

# Benchmark design ----
design = benchmark_grid(tasks = task_lung, learners = glmnet_at, resamplings = outer_resampling)

# Less logging
lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

## sequential
future::plan("sequential")
tic()
bm_res = benchmark(design, store_models = TRUE)
toc()

## parallel
future::plan("multisession")
tic()
bm_res = benchmark(design, store_models = TRUE)
toc()

## parallel-sequential
future::plan(list("multisession", "sequential"))
tic()
bm_res = benchmark(design, store_models = TRUE)
toc()

## sequential-parallel
future::plan(list("sequential", "multisession"))
tic()
bm_res = benchmark(design, store_models = TRUE)
toc()

# results from genlab7
# sequential: 143.708 sec elapsed
# multisession: 51.32 sec elapsed
# multisession-sequential: 51.567 sec elapsed
# sequential-multisession: 406.501 sec elapsed
