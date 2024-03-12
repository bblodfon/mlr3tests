#' https://cran.r-project.org/web/packages/coxed/vignettes/simulating_survival_data.html
#' `sim.survdata` allows for the simulation of duration/survival data without
#' assuming a particular parametric form for hazard
library(coxed)
library(mlr3proba)
library(survival)
library(mlr3verse)
library(tidyverse)
library(GGally)

# params ----
#' N => n_obs
#' T => t_max (event max time)
#' type (NULL) => no time-varying coefficients or coefficients, "tvc" => time-varying covariates
#' hazard.fun = NULL => gen. basHaz using flexible-hazard method from paper (2018)
#' num.data.frames (1) => #generated datasets
#' fixed.hazard (FALSE) => different hazard functions for each output data.frame
#' spline = TRUE (smooth hazard)
#' X (NULL) => dependence? variables that condition duration?
#' xvars (3) => number of variables to generate
#' mu (0), sd (0.5) => normal distr parameters for xvars
#' censor (0.1) => censoring rate
#' censor.cond (FALSE) => if TRUE, dependent censoring

# ind vs dep censoring ----
## Random censoring
simdata = sim.survdata(N = 1000, T = 100, num.data.frames = 1, censor = 0.2, censor.cond = FALSE)
logit = glm(failed ~ X1 + X2 + X3, data = simdata$data, family = binomial(link="logit"))
summary(logit) # most covariates are insignificant for predicting the censoring status

## Dependent censoring
simdata = sim.survdata(N = 1000, T = 100, num.data.frames = 1, censor = 0.8, censor.cond = TRUE)
logit = glm(failed ~ X1 + X2 + X3, data = simdata$data, family = binomial(link="logit"))
summary(logit) # covariates are significant for predicting the censoring status
plot_surv(surv = simdata$ind.survive, ids = sample(nrow(simdata$ind.survive), 5), type = "line")

# ml experiment
task = as_task_classif(
  x = simdata$data |> as.data.frame() |> select(starts_with("X") | failed),
  target = "failed",
  id = "cens_predict"
)
task
library(ranger)
learner = lrn("classif.ranger", num.trees = 250, predict_type = "prob")
learner = lrn("classif.cv_glmnet", predict_type = "prob")
p = learner$train(task)$predict(task)
p$confusion
p$score()
# Brier < 0.25 or logloss < 0.63 is better than baseline
p$score(msr("classif.bbrier"))
p$score(msr("classif.logloss"))

rs = resample(task, learner, rsmp("cv", folds = 10), store_models = TRUE)
rs$aggregate(measures = msrs(c("classif.ce", "classif.bbrier", "classif.logloss")))

# plots ----
# S(t), pdf, cdf, h(t)
simdata = sim.survdata(N = 1000, T = 100, num.data.frames = 4, censor = 0.8, spline = FALSE, xvars = 10)
survsim.plot(simdata, type = "baseline", df = 1) # df = 2 => the second data.frame

# returned result ----
# (per simulated dataset)
res = simdata[[1]]
head(res$data) # X + (time => y, status => failed)
head(res$xdata) # X only
head(res$baseline) # data.frame with pdf(t), cdf(t), S(t) and h(t)
lp = res$xb[,1] # lp's
head(lp)
head(res$exp.xb[,1]) # exp(lp)
head(res$betas[,1]) # for b*X
head(res$ind.survive) # survival matrix
# An (N x T) survival matrix containing the individual survivor function at
# time t for the individual represented by row n

  # mlr3proba conversion ----
task = as_surv_task(res)
p = get_pred_surv(res, row_ids = 1:10)
cor(p$lp, p$crank)
p$score(msr("surv.graf"), task = task, train_set = 1:task$nrow)
p$score(msr("surv.graf"))

# admin cens prop
cens_stats(task)

# User-specified baseline hazard ----
my.hazard = function(t) { # lognormal with mean of 50, sd of 10
  dnorm((log(t) - log(50))/log(10)) /
    (log(10)*t*(1 - pnorm((log(t) - log(50))/log(10))))
}
simdata = sim.survdata(N=1000, T=100, hazard.fun = my.hazard)
survsim.plot(simdata, type = "baseline")
plot_surv(surv = simdata$ind.survive, ids = sample(nrow(simdata$ind.survive), 5), type = "line") # PH still

task = as_surv_task(simdata)
autoplot(task)
cens_stats(task) # not admin censoring!

haz = sapply(seq(from = 1, to = 100, by = 1), my.hazard)
cumhaz = cumsum(haz)
surv = exp(-cumhaz)
surv # does not go to zero!

# Time-varying covariates (ie non-PH) ----
simdata = sim.survdata(N=1000, T=100, type="tvc", xvars=5, num.data.frames=1)
simdata$data

# will have to implement coxph() for time-varying effects and possibly survreg()

# cox model with time-varying effects
simdata = sim.survdata(N = 1000, T = 100, type = "tvc", xvars = 5)
model = coxph(Surv(start, end, failed) ~ ., data = simdata$data[,-1])
summary(model)
survival::cox.zph(fit = model) # p < 0.05 indicates PH violation
survsim.plot(simdata, type = "baseline")
plot_surv(simdata$ind.survive, ids = sample(nrow(simdata$ind.survive), 6))

# good agreement as cox with time-dependent covariates was used
diff = unname(model$coefficients) - simdata$betas[,1] # true coefs
mean(diff)

# Time-varying coefficients (also non-PH) ----
simdata = sim.survdata(N=1000, T=365, type="tvbeta", num.data.frames = 1, xvars = 10)
head(simdata$betas)
head(simdata$data)

model = coxph(Surv(y, failed) ~ ., data=simdata$data)
model$coefficients
summary(model)
zph_test = survival::cox.zph(fit = model) # p < 0.05 indicates PH violation
zph_test
survsim.plot(simdata, type = "baseline")
plot_surv(surv = simdata$ind.survive, ids = sample(nrow(simdata$ind.survive), 5), type = "step") # ISSUE: survival lines proportional?

# Only a few datasets are really non-PH!
res = vapply(1:100, function(i) {
  set.seed(i)
  xvars = sample(2:10, 1)
  simdata = sim.survdata(N=1000, T=365, type="tvbeta", num.data.frames = 1,
    xvars = xvars)

  model = coxph(Surv(y, failed) ~ ., data=simdata$data)
  zph_test = survival::cox.zph(fit = model)
  vec = zph_test$table["GLOBAL", "p"]
  names(vec) = xvars
  vec
}, numeric(1))
100 * sum(res > 0.05)/length(res) # % PH-compliant
100 * sum(res < 0.05)/length(res) # % Non-PH-compliant

simdata = sim.survdata(N = 1000, T = 365, type = "tvbeta",
    num.data.frames = 100, xvars = 5)

res = vapply(1:100, function(i) {
  obj = simdata[[i]]
  model = coxph(Surv(y, failed) ~ ., data = obj$data)
  zph_test = survival::cox.zph(fit = model)
  value = zph_test$table["GLOBAL", "p"]
  names(value) = xvars
  value
}, numeric(1))
