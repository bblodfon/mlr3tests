# testing how mlr3proba calculates distr predictions for
# the kaplan and nelson non-parametric estimators
library(mlr3verse)
library(mlr3proba)
library(survival)
library(testthat)

# Task lung ----
task = tsk('lung')
preprocess = po('encode') %>>% po('imputelearner', lrn('regr.rpart'))
task = preprocess$train(task)[[1]]
task$missings()

# Train + Predict ----
kaplan = lrn('surv.kaplan')
nelson = lrn('surv.nelson')

pred_kaplan = kaplan$train(task)$predict(task)
pred_kaplan
pred_nelson = nelson$train(task)$predict(task)
pred_nelson

# S(t) comparisons (survfit) ----
# Main findings:
# `model$surv` is KM `distr`(S(t))
# `exp(-model$cumhaz)` is NA `distr`(S(t))
model = survival::survfit(formula = task$formula(1), data = task$data())
model
summary(model)

head(model$surv)
head(model$time)
head(model$cumhaz)
# doesn't work since `stype = 1` => direct estimation of survival, i.e. using KM formula)
testthat::expect_equal(model$surv, exp(-model$cumhaz)) # S(t) != exp(-H(t))

## checks ----
times = model$time # ordered timepoints
# pick first individual, but for all it's the same!
expect_equal(unname(pred_kaplan$distr$survival(times[c(1,42,100)])[,1]),
  model$surv[c(1,42,100)])
expect_equal(unname(pred_nelson$distr$survival(times[c(1,42,100)])[,1]),
  exp(-model$cumhaz)[c(1,42,100)])

# check `cumhaz` is indeed calculated from Nelson-Aalen's formula
expect_equal(cumsum(model$n.event/model$n.risk), model$cumhaz)
# check `surv` is indeed calculated from KM formula
expect_equal(cumprod(1 - model$n.event/model$n.risk), model$surv)

# DIY (mlr3proba) ----
surv = matrix(rep(model$surv, task$nrow), ncol = length(times), nrow = task$nrow,
  byrow = TRUE)
colnames(surv) = times
output = mlr3proba::.surv_return(times = times, surv = surv)
expect_equal(unname(output$distr[1,]), model$surv)
expect_equal(output$distr[1,], pred_kaplan$distr$survival(times)[,1], tolerance = 1e-14)
expect_equal(output$crank, pred_kaplan$crank)

surv = matrix(rep(exp(-model$cumhaz), task$nrow), ncol = length(times),
  nrow = task$nrow, byrow = TRUE)
colnames(surv) = times
output = mlr3proba::.surv_return(times = times, surv = surv)
expect_equal(unname(output$distr[1,]), exp(-model$cumhaz))
expect_equal(output$distr[1,], pred_nelson$distr$survival(times)[,1], tolerance = 1e-14)
expect_equal(output$crank, pred_nelson$crank)

head(survivalmodels::surv_to_risk(surv)) # crank calculation

# survfit (stype = 2) ----
# `stype = 2` means S(t) = exp(-H(t)) and H(t) we get from Nelson-Aalen
?survfit.formula
model = survival::survfit(formula = task$formula(1), data = task$data(), stype = 2)
expect_equal(model$surv, exp(-model$cumhaz))
# and S(t) is not from the KM formula:
expect_equal(cumprod(1 - model$n.event/model$n.risk), model$surv)
