library(mlr3proba)
library(mlr3extralearners)
library(progressr)

# get all survival tasks in mlr3proba
keys = as.data.table(mlr_tasks)[task_type == "surv"][["key"]]
tasks = lapply(keys, function(key) {
  tsk(key)
})

# logging
lgr::get_logger("mlr3")$set_threshold("warn")

# Progress bars
options(progressr.enable = TRUE)
handlers(global = TRUE)
handlers("progress")

# parallelization
future::plan("multicore", workers = 15)

# conduct benchmark
set.seed(42)
bm_grid = benchmark_grid(
  tasks = tasks,
  learners = lrns(c("surv.kaplan", "surv.ranger", "surv.coxph")),
  resamplings = rsmp("repeated_cv", folds = 5)
)
bm = benchmark(bm_grid)

saveRDS(bm, "meas.rds")

