#' `mm-ePCR`: multi-omics and multi-cohort ePCR (ensemble penalized Cox-regression)
#' Use `mlr3proba:PipeOpSurvAvg` to average predictions + CoxNet with AutoTuner

# first let's try to do the ePCR method with the example datasets
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(ePCR)
data(TYKSSIMU)

# 2 cohorts with 101 clinical variables -  clinical, demographic, lab values,
# medical history, lesion sites, and previous treatments
all(colnames(xTEXTSIMU) == colnames(xMEDISIMU)) # all variables are the same

# test search cohort
task1 = mlr3proba::as_task_surv(x = cbind(yTEXTSIMU[,c("LKADT_P", "DEATH")], xTEXTSIMU),
    time = "LKADT_P", event = "DEATH", id = "TEXTSIMU")
task1

# medication curated cohort
task2 = mlr3proba::as_task_surv(x = cbind(yMEDISIMU[,c("LKADT_P", "DEATH")], xMEDISIMU),
  time = "LKADT_P", event = "DEATH", id = "MEDISIMU")
task2

coxnet = lrn('surv.glmnet', id = 'CoxNet', fallback = lrn('surv.kaplan'),
             standardize = TRUE, maxit = 10^4)
search_space = paradox::ps(
  lambda = p_dbl(1e-03, 10, logscale = TRUE),
  alpha  = p_dbl(0, 1) # from Ridge to Lasso penalty
)

# Initialize AutoTuner
lgr::get_logger('bbotk')$set_threshold('warn')
lgr::get_logger('mlr3')$set_threshold('warn')

# Progress bars
library(progressr)
options(progressr.enable = TRUE)
handlers(global = TRUE)
handlers('progress')


#' To ensure full parallelization, make sure that the `batch_size` multiplied by
#' the number of resampling iterations is at least equal (or multiple) to the
#' number of available `workers`
#' ...aim for a multiple of the number of workers.
options(mlr3.exec_chunk_bins = 15)
future::plan("multisession", workers = 15)
at = AutoTuner$new(
  learner = coxnet,
  resampling = rsmp("repeated_cv", repeats = 3, folds = 5), # 15 resamplings
  measure = msr("surv.graf", proper = TRUE, ERV = TRUE),
  search_space = search_space,
  terminator = trm('evals', n_evals = 100),
  tuner = tnr('random_search', batch_size = 100) # 10 hp configs per batch
)
at = AutoTuner$new(
  learner = coxnet,
  resampling = rsmp("repeated_cv", repeats = 3, folds = 5),
  measure = msr("surv.graf", proper = TRUE, ERV = TRUE),
  search_space = search_space,
  terminator = trm('evals', n_evals = 100),
  tuner = tnr('random_search')
)
at$train(task1) # one PSP
at$predict(task2) # task2 has same features as task1

at$tuning_result$x_domain[[1]]
autoplot(at$tuning_instance, type = 'performance')
autoplot(at$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(at$tuning_instance, type = 'surface', trafo = FALSE) + labs(x = 'Log(lambda)')
autoplot(at$tuning_instance, type = 'surface', trafo = TRUE)

l1 = at$learner
p1 = l1$predict(task2)
p1$score()

at$reset()

at$train(task2)
l2 = at$learner
p2 = l2$predict(task1)
p2$score()

# Average `PredictionSurv` objects
?PipeOpSurvAvg
poc = po("survavg", param_vals = list(weights = c(0.2, 0.8)))
poc$predict(list(p1, p2)) # can't do as number of rows is unequal



