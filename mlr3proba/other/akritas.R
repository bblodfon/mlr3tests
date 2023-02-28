library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(survival)
library(tibble)

# akritas ----
## (X = covariate-matrix, Y = time-to-event, C = censoring status)
## Y => random censoring
## C => depends on X
## So use when censoring is DEPENDENT on the covariates (instead of KM for example)
?survivalmodels::akritas

akritas1 = lrn('surv.akritas') # lambda = 0.5
akritas2 = lrn('surv.akritas', lambda = 1)
akritas3 = lrn('surv.akritas', lambda = 0)
akritas1$param_set
# reverse => If TRUE fits estimator on censoring distribution, otherwise (default) survival distribution.

# lung task ----
task = tsk('lung')
prep = po('encode', method = 'treatment') %>>% po('imputelearner', lrn('regr.rpart'))
task = prep$train(task)[[1]]
task

# Predictions ----
kaplan = lrn('surv.kaplan')
nelson = lrn('surv.nelson')

?survivalmodels::predict.akritas
ap1 = akritas1$train(task, row_ids = 1:200)$predict(task, row_ids = 201:228)
ap2 = akritas2$train(task, row_ids = 1:200)$predict(task, row_ids = 201:228)
ap3 = akritas3$train(task, row_ids = 1:200)$predict(task, row_ids = 201:228)
kp  = kaplan$train(task, row_ids = 1:200)$predict(task, row_ids = 201:228)
np  = nelson$train(task, row_ids = 1:200)$predict(task, row_ids = 201:228)

# all cranks the same for KM and NA
unique(kp$crank)
unique(np$crank)
kp$score()
np$score()

# not for all akritas though:
unique(ap1$crank)
unique(ap2$crank)
unique(ap3$crank) # different

ap1$score()
ap2$score()
ap3$score() # different than random (0.5)

# when lambda = 1, identical to Kaplan-Meier
expect_equal(ap1$distr$survival(task$unique_times())[,1], kp$distr$survival(task$unique_times())[,1]) # not so much
expect_equal(ap2$distr$survival(task$unique_times())[,1], kp$distr$survival(task$unique_times())[,1]) # not so much
expect_equal(ap3$distr$survival(task$unique_times())[,1], kp$distr$survival(task$unique_times())[,1]) # very different
