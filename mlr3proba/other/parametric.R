library(survival)
library(mlr3proba)
library(mlr3pipelines)
library(mlr3extralearners)

# Some library examples ----
?survival::survreg
m1 = survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), lung)
m1

y = rweibull(1000, shape=2, scale=5)
plot(density(y))
survreg(Surv(y)~1, dist="weibull")

# mlr3proba learner ----
learner = lrn("surv.parametric")
learner$help()
learner$param_set
# hps: type (predict) and scale (train)

# task lung ----
task = tsk('lung')
prep = po('encode', method = 'treatment') %>>% po('imputelearner', lrn('regr.rpart'))
task = prep$train(task)[[1]]
task

# Native train/predict ----
l1 = survreg(formula = task$formula(), data = task$data(),
  dist = 'weibull', scale = 0)
l1$coefficients[1] # location is the intercept!
l1$scale # scale
l1$dist
l1

# predict lp (not exported!)
lps = survival:::predict.survreg(object = l1, newdata = task$data(rows = c(1,2,3)), type = 'lp')
# https://github.com/mlr-org/mlr3extralearners/blob/main/R/learner_survival_surv_parametric.R#L169 (doing it manually)

# from doc: survreg's scale  =    1/(rweibull shape)
# from doc: survreg's intercept = log(rweibull scale)
baseline_distr = distr6::Weibull$new(
  shape = 1 / l1$scale, scale = exp(l1$coefficients[1]),
  decorators = "ExoticStatistics"
)
baseline_distr$summary()

learner$train(task)
learner$model$fit
l1 # same

p = learner$predict(task, row_ids = 1:3)
p
testthat::expect_equal(unname(lps), -p$lp)

p$score()
p$score(msr('surv.rcll')) # slow to compute?
p$score(msr('surv.brier'))

# mRNA data ----
task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA
learner = lrn("surv.parametric", dist = "weibull", type = "aft", scale = 1)
learner$train(task_mRNA) # it ran for ~10 min, didn't finish!
