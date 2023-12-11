library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(dplyr)

# Aim: Benchmark 3 learners on lung dataset

# Data preprocessing, convert to SurvTask
?survival::lung

lung = survival::lung
lung %>% as_tibble()

lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

task_lung = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
task_lung$missings() # missing values!
task_lung$truth()

# Simple imputation pipeline for missing values
mlr_pipeops$keys(pattern = '^impute')
?mlr_pipeops_imputelearner
?mlr_learners_regr.kknn

impute_po = po("imputelearner", po("imputehist") %>>% lrn("regr.kknn", k = 5))
impute_po = po("imputelearner", lrn('regr.rpart'))
task_lung_1 = impute_po$train(list(task = task_lung))[[1]]
task_lung_1$missings()

# learners
mlr_learners$keys(pattern = '^surv')

?mlr_learners_surv.kaplan
kaplan = lrn("surv.kaplan")

cox = lrn("surv.coxph")

xgb_graph = ppl("distrcompositor",
  learner = lrn("surv.xgboost"),
  estimator = "kaplan",
  form = "ph")
xgb_graph$plot()

xgb = xgb_graph %>% GraphLearner$new(id = 'surv.xgboost')
xgb$predict_types

learners = list(cox, kaplan, xgb)
resample = rsmp("cv", folds = 5)

set.seed(42)
design = benchmark_grid(task_lung, learners, resample)
design

# do the benchmark!
bm = benchmark(design)

measures = msrs(c("surv.intlogloss", "surv.cindex"))
measures

res = bm$score(measures)
res

bm$aggregate(measures)
bm$score(measures = msr("surv.cindex")) %>%
  group_by(learner_id) %>%
  summarize(avg = mean(surv.cindex), sd = sd(surv.cindex)) %>%
  select(learner_id, avg, sd)

autoplot(bm)
autoplot(bm, measure = msr('surv.intlogloss'))
autoplot(bm, measure = msr('surv.graf'))
