library(mlr3proba)
library(mlr3extralearners)
library(mlr3pipelines)

# Less logging
lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

tsk_grace = tsk("grace")
tsk_grace$col_roles$stratum = 'status'
tsk_grace$filter(sample(tsk_grace$nrow, 500))
msr_txt = c("surv.graf", "surv.rcll", "surv.cindex", "surv.dcalib")
measures = msrs(msr_txt)

graph_learner = as_learner(ppl(
  "distrcompositor",
  learner = lrn("surv.glmnet"),
  estimator = "kaplan",
  form = "ph"
))
graph_learner$id = "Coxnet"
learners = c(lrns(c("surv.coxph", "surv.kaplan")), graph_learner)

bmr = benchmark(benchmark_grid(tsk_grace, learners, rsmp("repeated_cv", repeats = 5, folds = 3)))
bmr$aggregate(measures)[, c("learner_id", ..msr_txt)]

learners = c(lrns(c("surv.coxph", "surv.kaplan", "surv.glmnet"))) # distr with Breslow
bmr = benchmark(benchmark_grid(tsk_grace, learners, rsmp("repeated_cv", repeats = 5, folds = 3)))
bmr$aggregate(measures)[, c("learner_id", ..msr_txt)]
