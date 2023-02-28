# Cox Proportional Hazards model using mlr3proba and other packages
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
library(mlr3verse)
library(mlr3proba)
library(survival)
library(tidyverse)
library(survminer)

# Task lung ----
task = tsk('lung')
pre = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task = pre$train(task)[[1]]
task$missings()

# CoxPH (mlr3proba) ----
cox = lrn('surv.coxph')
cox$help()
# Hazard function: prob/risk of dying at t
# h(t) = ho(t) * exp(bt), b = vector of covariates coefficients

# HAZARD RATIO or RISK: HR = exp(bX)
# if HR > 1, hazard increases => BAD prognostic factor
# if HR < 1, hazard decreases => GOOD prognostic factor

# Proportional hazards assumptions: h(t)/h'(t) = something with no time
# The hazard curves for the groups should be proportional and cannot cross

p = cox$train(task)$predict(task)
cox$model

# CoxPH (survival) ----
## Model fit ----
fit = survival::coxph(formula = task$formula(), data = task$data())
fit
# Age does not play a role (non-significant p-value)
# Higher ph.ecog is worst (bad prognostic)

## Predictions ----
pred_lp = predict(fit, task$data(), type = "lp", se.fit = TRUE)
all(pred_lp$fit == p$lp)
all(pred_lp$fit == p$crank)

pred_risk = predict(fit, task$data(), type = "risk", se.fit = FALSE)
all(pred_risk == exp(p$lp))

# the coefficients per subject fitted
pred_terms = predict(fit, task$data(), type = "terms", se.fit = TRUE)

# next outputs don't use!!! (don't make sense)
test_indx = c(1,3,13,28) # test patients :)
pred_expected = predict(fit, task$data()[test_indx], type = "expected", se.fit = FALSE)
pred_expected # expected number of events (H(t))
pred_surv = predict(fit, task$data()[test_indx], type = "survival", se.fit = FALSE)
all(pred_surv == exp(-pred_expected)) # S(t) = exp(-H(t))

## Plot survival curves ----
?survival::survfit.coxph
# NOTE: stype = 2, Breslow estimator for survival function, e.g. exp(-H(t))
res = survival::survfit(fit, newdata = task$data()[test_indx])
res$cumhaz # 4th is worse
res$surv
survminer::ggsurvplot(res, data = task$data(), conf.int = FALSE)

# At timepoint = 122, calculate the survival probabilities of the 4 test individuals
# from mlr3proba
s1 = p$distr$survival(c(122))[test_indx]
s1

## from survival
time_indx = which(res$time == 122)
s2 = res$surv[time_indx,]
s2
all(s1 == s2) # same

## PH assumption (Schoenfeld) ----
# http://www.sthda.com/english/wiki/cox-model-assumptions
?survival::cox.zph
test.ph = survival::cox.zph(fit)
test.ph
test.ph$table['GLOBAL',] # 0.1269
# p-values NOT significant (covariates + global test)
# So => ASSUME PROPORTIONAL HAZARDS

?plot.cox.zph
# The plot gives an estimate of the time-dependent coefficient
# β(t) - Schoenfeld Residuals. If the proportional hazards
# assumption holds then the true β(t) function would be a
# horizontal line.
plot(test.ph)

?survminer::ggcoxzph # Wrapper around plot.cox.zph :)
survminer::ggcoxzph(test.ph) #, var = 'sex')

## Test linearity (function form of a predictor) ----
# Plotting the Martingale residuals against continuous covariates
fit = survival::coxph(formula = Surv(time, status) ~ age + log(age) + sqrt(age), data = task$data())
fit
ggcoxfunctional(fit = fit, data = task$data())

## Important covariates? ----

# none of the observations is terribly influential individually
# (around 0 everything, nothing stands out too much)
survminer::ggcoxdiagnostics(fit, type = "dfbeta",
  ox.scale = 'observation.id', ggtheme = theme_bw())

## Outlier detection ----
# residuals should be roughly symmetrically distributed about
# zero with a standard deviation of 1
survminer::ggcoxdiagnostics(fit, type = "deviance",
    ox.scale = 'observation.id', ggtheme = theme_bw())

## Time-dependent variables ----

# Read (for CoxPH only)
# https://cran.rstudio.com/web/packages/survival/vignettes/timedep.pdf
