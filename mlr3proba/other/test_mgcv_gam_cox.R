library(survival)
library(mgcv)

set.seed(123)

n = 300
dat = data.frame(
  time   = rexp(n, rate = 0.1),
  status = rbinom(n, 1, 0.7), # 1 = event, 0 = censored
  age    = runif(n, 40, 80),
  biomarker = rnorm(n),
  sex    = factor(sample(c("F", "M"), n, replace = TRUE))
)

# what is the different using vs not using weights = status?
# I think treats all observations as events
fit = gam(
  time ~ s(age, k = 10) + s(biomarker, k = 10) + s(sex),
  data = dat,
  family = "cox.ph",
  weights = status
)

train_lp = fit$linear.predictors

# train set lp, good!
testthat::expect_equal(as.numeric(predict(fit)), train_lp)

# prediction
newdat = data.frame(
  age = c(20, 65),
  biomarker = c(0.2, -1.0),
  sex = factor(c("F", "M"), levels = levels(dat$sex))
)

# newdata.guaranteed = TRUE
test_lp = as.numeric(predict(fit, newdata = newdat))
test_lp

# Rule: If a variable has < ~10 unique values, don’t smooth it! (especially factors)

# survival curves
mlr3proba::breslow(
  times = dat$time,
  status = dat$status,
  lp_train = train_lp,
  lp_test = test_lp,
  eval_times = c(0, 0.1, 1, 10, 100)
)

# fails ---
predict(fit, newdata = newdat, type = "response")

# other families to consider
# - `cnorm` => censored normal distribution, for log normal accelerated failure time models, Tobit regression and rounded data, for example.
# - `clog` =>  censored logistic distribution, for accelerated failure time models.
# - `bcg` => Box-Cox transformed censored Gaussian.
# - `cpois` => censored Poisson distribution.

# read paper: https://www.tandfonline.com/doi/suppl/10.1080/01621459.2016.1180986?scroll=top
