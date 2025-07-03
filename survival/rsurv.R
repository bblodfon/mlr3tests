library(rsurv) # takes some time to install
library(flexsurv)
library(dplyr)
library(GGally)
library(survstan)

# qsurv ----
#' Allows the use of any baseline survival distribution for which there exists an
#' implementation of its quantile function.
#'
#' My own quantile function: inverse of CDF
qmydist = function(p, lambda, ...) {
  x = qexp(p, rate = lambda, ...)
  return(x)
}

set.seed(1234567890)
u = runif(5) # ~U(0,1)
x1 = qexp(u, rate = 1, lower.tail = FALSE) # ~Exp(1)
x2 = rsurv:::qsurv(u, baseline = "exp", rate = 1) # ~Exp(1)
x3 = rsurv:::qsurv(u, baseline = "mydist", lambda = 1) # same!
# same as above => special case of the generalized Gamma distr
#' `?qgengamma.orig`
x4 = rsurv:::qsurv(u, baseline = "gengamma.orig", shape=1, scale=1, k=1)
cbind(x1, x2, x3, x4)

# Right administrative censoring ----
## Type-I (admin. cens) right-censored data, Y ~ AFT, baseline => loglogistic, age + sex + interaction
set.seed(1234567890)
n = 1000
tau = 10 # maximum follow up time
simdata = data.frame(
  age = rnorm(n),
  sex = sample(c("f", "m"), size = n, replace = TRUE)
  ) |>
  mutate(
    t = rsurv::raftreg(
      u = runif(n),
      formula = ~ age*sex, # gets extended
      beta = c(1, 2, -0.5),
      dist = "llogis",
      shape = 1.5,
      scale = 1
    ),
    # c = runif(n, 0, 10)
  ) |>
  rowwise() |>
  mutate(
    time = min(t, tau),
    # time = min(t, c),
    status = as.numeric(time == t)
  ) |> ungroup()
simdata

# baseline options => exponential, weibull, lognormal, loglogistic
fit = survstan::aftreg(Surv(time, status) ~ age*sex, data = simdata, baseline = "loglogistic")
estimates(fit) # correspond to the beta, shape and scale

# Crossing survival curves ----
## Y ~ Yang Prentice model, random censoring
set.seed(1234567890)
n = 1000
simdata = data.frame(trt = sample(c("chemo", "chemo+rad"), size = n, replace = TRUE)) |>
  mutate(
    t = rypreg(runif(n), ~ trt, beta = 2, phi = 1.5, dist = "weibull", shape = 1.5, scale = 1),
    c = rexp(n, rate = 1)
  ) |>
  rowwise() |>
  mutate(
    time = min(t, c),
    status = as.numeric(time == t)
  ) |>
  select(-c(t, c))

km = survfit(Surv(time, status) ~ trt, data = simdata)
ggsurv(km) + theme(legend.position = "bottom") # crossing S

fit = ypreg(Surv(time, status) ~ trt, data = simdata, dist = "weibull")
estimates(fit)

# Mixture cure rate model with a probit link function ----
set.seed(1234567890)
kappa = c(0.5, 1.5, -1.1) # coefs
n = 1000
# generating the set of explanatory variables:
simdata = data.frame(
  trt = sample(c("A", "B"), size = n, replace = TRUE),
  age = rnorm(n)
)

# generating the data set:
v = inv_pgf(formula = ~ trt + age,
            incidence = bernoulli("probit"), # incidence sub-model + link function
            kappa = kappa,
            data = simdata)

simdata = simdata |>
  mutate(t = qexp(v, rate = 1, lower.tail = FALSE),
         c = rexp(n, rate = 1)) |>
  rowwise() |>
  mutate(time = min(t, c), status = as.numeric(time == t)) |>
  select(-c(t, c))

km = survfit(Surv(time, status) ~ trt, data = simdata)
ggsurv(km) + ylim(c(0,1))

# Frailty ----
library(frailtyEM)

set.seed(1234567890)
n = 1000 # sample size
L = 100 # number of clusters
simdata = data.frame(
  id = rep(1:L, each = n/L),
  age = rnorm(n),
  sex = sample(c("f", "m"), size = n, replace = TRUE)
) |>
  mutate(
    # generate frailty term
    frailty = rfrailty(cluster = id, frailty = "gamma", sigma = 0.5),
    # added via offset in the linear predictor
    t = rphreg(u = runif(n), ~ age*sex + offset(frailty), beta = c(1, 2, -0.5), dist = "exp", rate = 1),
    c = runif(n, 0, 5)
  ) |>
  rowwise() |>
  mutate(time = min(t, c), status = as.numeric(t < c)) |>
  select(-c(t, c))
simdata

# Semiparametric Shared Frailty Model fitted via EM
em = frailtyEM::emfrail(Surv(time, status) ~ age*sex + cluster(id),
                        distribution = emfrail_dist(dist = "gamma"),
                        data = simdata)
summary(em)

# Interval-censored data ----
library(icenReg)

set.seed(1234567890)
n = 300
covariates = data.frame(
  age = rnorm(n),
  sex = sample(c("f", "m"), size = n, replace = TRUE)
)

## type I interval censored survival data ("current status")
simdata1 =
  covariates |>
  mutate(time = rphreg(u = runif(n), ~ age+sex, beta = c(1, 0.5), dist = "exp", rate = 1),
         tau = rweibull(n, scale = 2, shape = 1.5), # vector of censoring times
         rinterval(time, tau, type = "I")) # either left or right censoring, single examination!
head(simdata1)
tail(simdata1)

fit1 = ic_par(cbind(left, right) ~ age+sex, data = simdata1, model = "ph", dist = "exponential")
summary(fit1)

## type II interval censored survival data
simdata2 = covariates |>
  mutate(time = raftreg(u = runif(n), ~ age+sex, beta = c(1, 0.5), dist = "exp", rate = 1),
         #' `tau` => time grid of scheduled visits
         #' `prob` => attendance probability
         rinterval(time, tau = seq(0, 5, by = 1), type = "II", prob = 0.7)
)

head(simdata2)
fit2 = ic_par(cbind(left, right) ~ age+sex, data = simdata2, model = "aft", dist = "exponential")
summary(fit2)

# Dep. Censoring + Competing Risks ----
library(copula)

set.seed(1234567890)
n = 1000 # sample size
tau = 0.5 # Kendall's tau correlation

theta = copula::iTau(copula = claytonCopula(), tau = tau)
clayton = claytonCopula(param = theta, dim = 2)
clayton

# random generation
u = rCopula(clayton, n = n)
head(u)

# simulating the failure times:
simdata = data.frame(
  age = rnorm(n),
  sex = sample(c("f", "m"), size = n, replace = TRUE)
) |>
mutate(
  t1 = rphreg(u = u[,1], ~ age + sex, beta = c(1, 1.2), dist = "exp", rate = 2),
  t2 = rporeg(u = u[,2], ~ age + sex, beta = c(0.8, 1.1), dist = "exp", rate = 1),
)
head(simdata)

# checking out the correlation:
cor(simdata$t1, simdata$t2, method = "kendall")

# adding (right) censoring
simdata1 = simdata |> mutate(
  # random censoring
  c = runif(n, 0, 5)) |>
  rowwise() |>
  mutate(
    t1 = min(t1, c),
    t2 = min(t2, c),
    status1 = as.numeric(t1 < c),
    status2 = as.numeric(t2 < c)
  ) |> ungroup()

tail(simdata1)

# checking out the correlation:
cor(simdata1$t1, simdata1$t2, method = "kendall")

# Dep. censoring ----
simdata2 = simdata |>
  mutate(
    # random censoring
    a = runif(n, 0, 5)
  ) |>
  rowwise() |>
  mutate(
    #' `t1`: time to failure
    #' `t2`: dependent censoring time
    #' `a`: censoring time due to random censoring
    y = min(t1, t2, a),
    status1 = as.numeric(y == t1),
    status2 = as.numeric(y == t2)
  ) |>
  #select(-c(t1, t2)) |>
  ungroup()

head(simdata2) # (y, status1)

# CRs ----
n = 1000
tau = 0.5
theta = iTau(copula = gumbelCopula(), tau=tau)
gumbel = gumbelCopula(param = theta, dim = 3)
u = rCopula(gumbel, n = n)

simdata = data.frame(
  age = rnorm(n),
  sex = sample(c("f", "m"), size = n, replace = TRUE)
  ) |>
  mutate(
    t1 = rphreg(u = u[,1], ~ age + sex, beta = c(1, 1.2), dist = "lnorm", meanlog = 0, sdlog = 1),
    t2 = rphreg(u = u[,2], ~ age + sex, beta = c(0.8, 1.1), dist = "exp", rate = 1),
    t3 = rphreg(u = u[,3], ~ age + sex, beta = c(0.7, 1.0), dist = "exp", rate = 1),
    a = runif(n, 0, 5) # random censoring
  ) |>
  rowwise() |>
  mutate(
    y = min(t1, t2, t3, a),
    status1 = as.numeric(y == t1),
    status2 = as.numeric(y == t2),
    status3 = as.numeric(y == t3),
  ) |>
  #select(-c(t1, t2, t3)) |>
  ungroup()
head(simdata)
