# Variational Bayesian proportional hazards models
# https://github.com/mkomod/survival.svb/issues/2 => author didn't engage enough
# IDEA: can we get lp scores from this model?
library(survival.svb)

n = 125                        # number of sample
p = 500                        # number of features
s = 5                          # number of non-zero coefficients
censoring_lvl = 0.25           # degree of censoring

# generate some test data
set.seed(1)
b = sample(c(runif(s, -2, 2), rep(0, p-s)))
X = matrix(rnorm(n * p), nrow=n)
Y = log(1 - runif(n)) / -exp(X %*% b)
delta  = runif(n) > censoring_lvl   		# 0: censored, 1: uncensored
Y[!delta] = Y[!delta] * runif(sum(!delta))	# rescale censored data
Y
X

# fit the model
fit = survival.svb::svb.fit(Y = Y, delta = delta, X = X)

all(fit$inclusion_prob == fit$gamma)
all(fit$gamma == fit$g)
all(fit$beta_hat == fit$g * fit$m)

svb.sample = function(fit, samples = 1e4) {
  p = length(fit$g) # number of features

  beta = replicate(samples,
    {
      # Uniform prob of inclusion, very few TRUEs here
      # j is the index of selected features
      j = runif(p) <= fit$g
      b = rep(0, p)

      if (sum(j) == 0)
        return(b)

      m = rnorm(sum(j), fit$mu[j], fit$s[j])
      b[j] = m
      return(b)
    }, simplify = "matrix")

  return(beta) # beta matrix,
}

res = svb.sample(fit)
dim(res)

mbeta  = apply(res, 1, mean)
sdbeta = apply(res, 1, sd) #
qbeta = apply(res, 1, quantile, probs = c(0.05, 0.95))

head(sort(mbeta, decreasing = T)) # same
head(sort(fit$beta_hat, decreasing = T)) # same


