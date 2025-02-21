# How to get CIFs from the `etm` R package + variance
# https://cran.r-project.org/web/packages/etm/vignettes/etmCIF_tutorial.pdf
library(etm)
library(survival)

# simulate some data
set.seed(1)
n = 100  # Number of individuals

# Simulated survival times
time = round(rexp(n, rate = 0.1), digits = 3)  # Exponential distribution for event times
length(unique(time)) # 100 time points unique
# Simulated event types (1 = Risk 1, 2 = Risk 2, 0 = Censored)
event = sample(c(0, 1, 2), size = n, replace = TRUE, prob = c(0.4, 0.3, 0.3))

data = data.frame(
  tstart = rep(0, n),
  tstop = time,
  event = event
)
head(data)

as_task_cmprisk(x = data)

fit = etm::etmCIF(formula = survival::Surv(tstart, tstop, event != 0) ~ 1,
                  data = data, etype = event, failcode = 1)
# plot(fit, which.cif = c(1,2), col = 1:2, ci.type = "pointwise")
class(fit[[1]]) # etm

#etm:::xyplot.etm(fit[[1]]) # doesn't work???

fit[[1]]$trans
length(fit[[1]]$time)
fit[[1]]$time

etm::trprob(fit[[1]], tr.choice = "0 0")
etm::trprob(fit[[1]], tr.choice = "0 1")
# Predict probabilities (does constant interpolation)
trprob(fit[[1]], tr.choice = "0 2", timepoints = c(0, 0.38, 0.55, 49, 1000))

ss = summary(fit, ci.fun = "cloglog")
ss[[1]]$`CIF 1`
ss[[1]]$`CIF 2`

