library(mlr3proba)
library(mlr3extralearners)

# https://github.com/mlr-org/mlr3proba/issues/263

l = lrn("surv.gbm")
t = tsk("whas")
l$train(t)
p = l$predict(t)

# S(t) = exp(-H(t)), H(t) => cumulative hazard
plot(exp(-gbm::basehaz.gbm(t$truth()[, 1], t$truth()[, 2], p$lp,
  t.eval = sort(unique(t$truth()[, 1])) # Breslow (last timepoints get you NA's)
)), ylim = c(0, 1), type = "l", xlab = "T", ylab = "S(T)")

lines(survival::survfit(t$formula(1), t$data())$surv, col = 2) # KM
t$formula(1) # fit only intercept of the regression model => mean response value

library(survival)
df = t$data()
lines(exp(-survival::basehaz(coxph(t$formula(), df)))[, 1], col = 3) # NA? (CoxPH)
legend("topright", lty = 1, col = 1:3, legend = c("GBM", "KM", "CPH"))
