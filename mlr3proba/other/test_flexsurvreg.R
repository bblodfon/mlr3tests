library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3pipelines)
library(ggplot2)

# tasks
files = list.files(pattern = "^dataset_.*\\.rds$")

tasks = lapply(files, function(f) {
  data = readRDS(f)

  # extract ID from filename, e.g. dataset_750_4.rds → "dataset_750_4"
  id = sub("\\.rds$", "", f)

  task = as_task_surv(
    x = data,
    target = "time",
    event  = "status",
    id     = id
  )

  task$set_col_roles("status", add_to = "stratum")
  #task$filter(rows = sample(x = task$nrow, size = 50))
  task
})

# learners
# correct
l1 = lrn("surv.flexreg", id = "weib_int",
         formula = survival::Surv(time, status) ~ x1 + x2 + x1:x2,
         anc = list(shape = ~ x2),
         dist = "weibull")
# misspecified AFT models
l2 = lrn("surv.flexreg", id = "weib_noint",
         formula = survival::Surv(time, status) ~ x1 + x2,
         anc = list(shape = ~ x2),
         dist = "weibull")
l3 = lrn("surv.flexreg", id = "lnorm_int",
         formula = survival::Surv(time, status) ~ x1 + x2 + x1:x2,
         anc = list(sdlog = ~ x2),
         dist = "lnorm")
l4 = lrn("surv.flexreg", id = "lnorm_noint",
         formula = survival::Surv(time, status) ~ x1 + x2,
         anc = list(sdlog = ~ x2),
         dist = "lnorm")
# misspecified PH models
l5 = lrn("surv.flexspline", id = "weibPH_spline",
         k = 1, # can be 0
         scale = "hazard",
         anc = list(gamma1 = ~x2),
         formula = survival::Surv(time, status) ~ x1 + x2 + x1:x2)
l6 = lrn("surv.coxph", id = "cox")
l7 = po("modelmatrix", formula =  ~ -1 + x1 + x2) %>>%
  lrn("surv.coxph") |>
  as_learner()
l7$id = "cox_int"
l7$encapsulate(method = "evaluate", fallback = lrn("surv.kaplan"))
learners = list(l1,l2,l3,l4,l5,l6,l7)
learners = list(l1,l2,l6,l7)

bm_grid = benchmark_grid(tasks, learners, rsmp("cv", folds = 5))

#plan("multisession", workers = 15)
bm_res = benchmark(bm_grid)

library(mlr3viz)
autoplot(bm_res, measure = msr("surv.cindex"))
autoplot(bm_res, measure = msr("surv.cindex")) +
  labs(title = "Harrell's C-index (higher is better)")

autoplot(bm_res, measure = msr("surv.graf"))
autoplot(bm_res, measure = msr("surv.rcll")) +
  labs(title = "RCLL (lower is better)")
autoplot(bm_res, measure = msr("surv.dcalib", truncate = 15)) +
  labs(title = "D-calibration (lower is better)")
autoplot(bm_res$clone(deep = TRUE)$filter(task_ids = tasks[[3]]$id),
         measure = msr("surv.uno_auc"))
