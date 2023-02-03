library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(dplyr)

kaplan = lrn("surv.kaplan")
cox = lrn("surv.coxph")
xgb = ppl("distrcompositor", learner = lrn("surv.xgboost"), estimator = "kaplan", form = "ph") %>% as_learner()
learners = list(cox, kaplan, xgb)
task = TaskSurv$new(id = "rats", backend = survival::rats[,1:4], time = "time", event = "status")
resample = rsmp("cv", folds = 5)
set.seed(42)
design = benchmark_grid(task, learners, resample)
bm = benchmark(design)
measures = msrs(c("surv.intlogloss", "surv.cindex"))
bm$aggregate(measures)
res = bm$score(measures)
res

bm$score(measures = msr("surv.cindex")) %>%
  group_by(learner_id) %>%
  summarize(avg = mean(surv.cindex), sd = sd(surv.cindex)) %>%
  select(learner_id, avg, sd)
