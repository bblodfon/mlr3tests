# benchmark CoxBoost runtime speed given different `criterion` hyperparameter
library(mlr3verse)
library(mlr3proba)
library(rbenchmark)

# mRNA Task ----
# ~10000 features
tasks = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')
task_mRNA = tasks$mRNA

coxboost_score = lrn('surv.coxboost',
  standardize = FALSE, # data already standardized
  return.score = FALSE, # don't need this in the output
  stepno = 66,
  criterion = 'score'
)

coxboost_pscore = lrn('surv.coxboost',
  standardize = FALSE, # data already standardized
  return.score = FALSE, # don't need this in the output
  stepno = 66,
  criterion = 'pscore'
)

coxboost_hpscore = lrn('surv.coxboost',
  standardize = FALSE, # data already standardized
  return.score = FALSE, # don't need this in the output
  stepno = 66,
  criterion = 'hpscore'
)

res = within(rbenchmark::benchmark(
  'CoxBoost-score'   = { coxboost_score$train(task_mRNA)   },
  'CoxBoost-pscore'  = { coxboost_pscore$train(task_mRNA)  }, # ~ same time as above
  'CoxBoost-hpscore' = { coxboost_hpscore$train(task_mRNA) }, # FASTEST
  replications = 5,
  columns = c("test", "replications", "elapsed", "relative")
), { average_sec = (elapsed/replications)})
