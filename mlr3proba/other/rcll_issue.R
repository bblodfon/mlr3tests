# pdf(t) => 0 for test time points that are not in the initial matrix
library(mlr3extralearners)
library(mlr3proba)

# Logging
lgr::get_logger('bbotk')$set_threshold('warn')
lgr::get_logger('mlr3')$set_threshold('warn')

task = tsk("veteran") # same as below from OML
# dataset_tbl = readRDS(here::here("dataset_table.rds"))
# veteran_id = 46166
# dat = mlr3oml::odt(veteran_id)
# task = mlr3proba::as_task_surv(mlr3::as_data_backend(dat), target = "time", event = "status", id = dat$name)
task$set_col_roles("status", add_to = "stratum")

cox = lrn("surv.coxph", id = "cox")
lrn_weibull = lrn("surv.parametric", form = "aft", discrete = TRUE, dist = "weibull", id = "weibull")
lrn_exp = lrn("surv.parametric", form = "aft", discrete = TRUE, dist = "exponential", id = "exp")
lrn_lognorm = lrn("surv.parametric", form = "aft", discrete = TRUE, dist = "lognormal", id = "lognorm")
lrn_loglog = lrn("surv.parametric", form = "aft", discrete = TRUE, dist = "loglogistic", id = "loglog")
km = lrn("surv.kaplan", id = "kaplan")

resamplings = lapply(1:100, \(.x) {
  rsmp("holdout", ratio = 0.8)
})

grid = benchmark_grid(
  tasks = list(task), 
  learners = list(cox, lrn_weibull, lrn_exp, lrn_loglog, lrn_lognorm, km),
  resamplings = resamplings
  # resamplings = list(rsmp("repeated_cv", folds = 5, repeats = 5))
)

bm = benchmark(design = grid, store_models = TRUE, store_backends = TRUE)

rcll = msr("surv.rcll")
res = bm$score(rcll)
library(tidyverse)
res |> as_tibble() |> select(learner_id, iteration, surv.rcll) |>
  group_by(learner_id) |> summarise(avg_rcll = mean(surv.rcll), sd_rcll = sd(surv.rcll))

# more manual: only censored obs in the test set
cens_ids = which(task$status() == 0)
part = list(train = setdiff(task$row_ids, cens_ids), test = cens_ids)

pcox = cox$train(task, part$train)$predict(task, part$test)
pwei = lrn_weibull$train(task, part$train)$predict(task, part$test)
pexp = lrn_exp$train(task, part$train)$predict(task, part$test)
plognorm = lrn_lognorm$train(task, part$train)$predict(task, part$test)
ploglog = lrn_loglog$train(task, part$train)$predict(task, part$test)

# RCLL is less in general, but the same across learners
pcox$score(rcll)
pwei$score(rcll)
pexp$score(rcll)
plognorm$score(rcll)
ploglog$score(rcll)

# check number of time points
ntimes = sapply(1:500, \(index) {
  ncol(bm$resample_result(index)$predictions()[[1]]$data$distr)
})
ntimes |> summary() # min => 81

# check last time points
last_timepoints = sapply(1:500, \(index) {
  tail(as.numeric(colnames(bm$resample_result(index)$predictions()[[1]]$data$distr)), 1)
})

table(last_timepoints)

# ids with low last time point
indx = which(last_timepoints < 600)

# which models have low last time points 
res$learner_id[indx] # most are AFT

# num time points
ntimes[indx]

# what RCLL do these have? => they are OKAY!!! (not low!)
res$surv.rcll[indx]

