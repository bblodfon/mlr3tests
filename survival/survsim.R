### Load package
library(survsim)

### 3. Simple survival data (Section 3.3, pp. 5-6)
## Y
dist.ev = "weibull" # exponential
anc.ev = 1 # shape
beta0.ev = 5.268

## C
dist.cens = "weibull"
anc.cens = 1
beta0.cens = 5.368

## X
x = list(c("bern", 0.3), c("bern", 0.4), c("normal", 0, 2)) # 3 covariates {norm, unif, bernoulli}
beta = list(-0.4, -0.25, 0.3) # must match the variables

set.seed(11092014)
simple.dat = simple.surv.sim(
  n = 300, foltime = 365,
  dist.ev, anc.ev, beta0.ev,
  dist.cens, anc.cens, beta0.cens,
  z = NULL, beta, x
)

head(simple.dat)
summary(simple.dat)

### 4. Multiple event survival data (Section 4.3, pp. 8-10)
## Adverse Event data
## 3 x Y
dist.ev = c("weibull", "llogistic", "weibull")
anc.ev = c(0.8, 0.9, 0.82)
beta0.ev = c(3.56, 5.94, 5.78)

## X
beta = list(c(-0.04, -0.02, -0.01), c(-0.001, -8e-04, -5e-04), c(-0.7, -0.2, -0.1))
x = list(c("normal", 26, 4.5), c("unif", 50, 75), c("bern", 0.25))

set.seed(11092014)
clinical.data = mult.ev.sim(
  n = 100, foltime = 30,
  dist.ev, anc.ev, beta0.ev,
  dist.cens = "weibull", anc.cens = 1, beta0.cens = 5.2, # exponential
  z = list(c("unif", 0.6, 1.4)),
  beta, x,
  nsit = 3 # Number of different events for each subject
  # max_ep = Inf # maximum permitted number of episodes per subject
)
nrow(clinical.data) # 300
head(round(clinical.data, 2))
summary(clinical.data)

### 5. Recurrent event survival data (Section 5.3, pp. 13-16)
## Recurrent sick leave episodes
## Y
dist.ev = c("lnorm", "llogistic", "weibull", "weibull") # last distr => generate times for more than 4 episodes!
anc.ev = c(1.498, 0.924, 0.923, 1.051)
beta0.ev = c(7.195, 6.583, 6.678, 6.43)
## C
anc.cens = c(1.272, 1.218, 1.341, 1.484)
beta0.cens = c(7.315, 6.975, 6.712, 6.399)
## X
beta = list(c(-0.4, -0.5, -0.6, -0.7))
## Mean duration of each event (in days)
lambda = c(2.18, 2.33, 2.40, 3.46)

set.seed(11092014)
sim.data = rec.ev.sim(
  n = 500, foltime = 3600,
  dist.ev, anc.ev, beta0.ev,
  , anc.cens, beta0.cens,
  z = list(c("unif", 0.8, 1.2)),
  beta, x = list(c("bern", 0.5)),
  lambda,
  priskb = 0.5, max.old = 730 # 50% of individuals are at risk max 730 days before day 0 (start of follow-up)
)
head(sim.data) # start2, stop2 => calendar time
# the first individual who was at risk prior to follow-up
sim.data[sim.data$nid == 251, ]
summary(sim.data)

### 7. Aggregated data (Section 7.1, pp. 18)
agg.data = accum(sim.data)
head(agg.data)

### 8. Application using Cox models (Section 8, pp. 18-19)
library(survival)
library(MASS)

## simple Cox regression (start == 0 always)
single = coxph(Surv(start, stop, status) ~ as.factor(x) + as.factor(x.1), data = simple.dat)
single

## multiple event Cox regression
# separate baseline hazards for each event number (`strata`)
# estimate coefs within each stratum (`/`)
# observations are clustered by individual (`cluster`)
# => adjust for within-cluster correlation between events for the same person
multiple = coxph(Surv(start, stop, status) ~ strata(ev.num)/(x + x.1 + as.factor(x.2)) +
  cluster(nid), data = clinical.data)
multiple

## Andersen-Gill Cox regression model for recurrent event data
AG.event = coxph(Surv(start2, stop2, status) ~ as.factor(x) + cluster(nid), data = sim.data)
AG.event
