# https://pbreheny.github.io/ncvreg/articles/models.html
# ref: Breheny P and Huang J. (2011) Coordinate descent algorithms for nonconvex penalized regression, with applications to biological feature selection. Annals of Applied Statistics, 5: 232-253. doi:10.1214/10-AOAS388
library(ncvreg)

# Regression ----
data(Prostate)
X = Prostate$X
y = Prostate$y

fit = ncvreg(X, y, gamma = 1.2, penalty = "MCP") # gamma > 1 for MCP
fit = ncvreg(X, y, gamma = 2, penalty = "SCAD") # gamma > 2 for SCAD

fit = ncvreg(X, y)
plot(fit) # coefs path
summary(fit, lambda=0.05)
local_mfdr(fit, 0.1)
predict(fit, X[1, ], type = "link", lambda = 0.05)
predict(fit, X[1, ], type = "response", lambda = 0.05) # same as above
predict(fit, X, type = "class", lambda = 0.05) # same as above

cvfit = cv.ncvreg(X, y) # family = "gaussian" or "poisson"
plot(cvfit, log.l = FALSE) # CV error
plot(cvfit, type='rsq') # R^2
summary(cvfit)
cvfit$lambda # all lambdas tested
cvfit$lambda.min # min lambda
# lps are lamdba.min =>
cvfit$fit$linear.predictors[, cvfit$min] # `$min` gives index of lambda.min

coef(cvfit, lambda = 0.02)
coef(cvfit, lambda = 0.3)
predict(cvfit, X = X)
predict(cvfit, X = X, type = "response")

# Binary classification ----
## Logistic regression
data(Heart)
X = Heart$X
y = Heart$y
# fit = ncvreg(X, y, family='binomial')
local_mfdr(fit) # best lambda?
summary(fit) # needs lambda!

## predict
predict(fit, X[1, ], type = "link", lambda = 0.05)
predict(fit, X[1, ], type = "response", lambda = 0.05) # different!
predict(fit, X, type = "class", lambda = 0.05) # classes!

cvfit = cv.ncvreg(X, y, family='binomial')
plot(cvfit)
par(mfrow=c(2,2))
plot(cvfit, type='all')

predict(cvfit, type = "coef") # at min lambda

# Survival ----
data(Lung)
X = Lung$X
y = Lung$y

op = par(mfrow=c(2,2))
fit = ncvsurv(X, y)
plot(fit, main=expression(paste(gamma,"=",3)))
fit = ncvsurv(X, y, gamma=10)
plot(fit, main=expression(paste(gamma,"=",10)))
fit = ncvsurv(X, y, gamma=1.5)
plot(fit, main=expression(paste(gamma,"=",1.5)))
fit = ncvsurv(X, y, penalty="SCAD")
plot(fit, main=expression(paste("SCAD, ",gamma,"=",3)))
par(op)

fit = ncvsurv(X,y)
ll = log(fit$lambda)
op = par(mfrow=c(2,1))
plot(ll, BIC(fit), type="l", xlim=rev(range(ll)))
lam = fit$lambda[which.min(BIC(fit))]
b = coef(fit, lambda=lam)
b[b!=0]
#>       karno    squamous       adeno
#> -0.03316393 -0.42452397  0.41834812
plot(fit)
abline(v=lam)

# prediction?
fit = ncvsurv(X,y)
lambda = fit$lambda[12] # choose random lambda
lambda = lam
p = predict(fit, X, type='link', lambda = lambda)
# not equal! => lps are centered with mean 0
testthat::expect_equal(p, fit$linear.predictors[, 12])

# order by time on study
p = p[order(y[, 1])]
# now equal!!!
testthat::expect_equal(
  unname(p - mean(p)),
  unname(fit$linear.predictors[, which(fit$lambda == lambda)])
)

newdata = X[1:2,]
newdata = X
predict(fit, newdata, type = "link", lambda = lambda) # lp
predict(fit, newdata, type = "median", lambda = lambda) # response (median survival times)
S_list = predict(fit, newdata, type = "survival", lambda = lambda) # survival distr
h_list = predict(fit, newdata, type = "hazard", lambda = lambda) # hazard distr

# Common time grid
times = attr(S_list, "time") # same as fit$time

# obs x times survival matrix
surv_mat = do.call(
  rbind,
  lapply(S_list, function(S) S(times))
)

haz_mat = do.call(
  rbind,
  lapply(h_list, function(h) h(times))
)

surv_mat
haz_mat

# $selected_features()
# how many?
predict(fit, newdata, type = "nvars", lambda = lambda) |> unname()
# which features?
names(predict(fit, newdata, type = "vars", lambda = lambda))

# CV of lambda + train/test split
data(Lung)
X = Lung$X
y = Lung$y

set.seed(42)
n = nrow(X)
train_ids = sample(seq_len(n), size = 0.8 * n)
test_ids  = setdiff(seq_len(n), train_ids)

X_train = X[train_ids, ]
y_train  = y[train_ids]

cvfit = cv.ncvsurv(X_train, y_train) # ncv.surv
# formalArgs(ncvreg)
# formalArgs(ncvsurv)
summary(cvfit) # prints best.lambda
plot(cvfit)
plot(cvfit, type="rsq")
cvfit$lambda # all lambdas tested
cvfit$lambda.min # best lambda
cvfit$min # index of best lambda
cvfit$lambda[cvfit$min] == cvfit$lambda.min

X_test = X[test_ids, ]
predict(cvfit, X = X_test)
predict(cvfit, X = X_test, type = "link") # lp (best lambda)
predict(cvfit, X = X_test, type = "link", lambda = cvfit$lambda.min) # same as above
predict(cvfit, X = X_test, type = "link", which = cvfit$min) # same as above
predict(cvfit, X = X_test, type = "link", which = 6) # different
predict(cvfit, X = X_test, type = "link", lambda = cvfit$lambda[14]) # different
# predict(cvfit, X = X_test, type = "response") # exp(lp), don't need this
predict(cvfit, X = X_test, type = "median") # median survival times

S_list = predict(cvfit, X = X_test, type = "survival") # S(t)

# Common time grid
times = attr(S_list, "time") # same as fit$time
all(times == cvfit$fit$time) # ordered time points from train set

# obs x times survival matrix
surv_mat = do.call(
  rbind,
  lapply(S_list, function(S) S(c(times)))
)
dim(surv_mat)

# USE ONLY NUMERIC VARIABLES!!!
library(ncvreg)
library(survival)

rats = survival::rats
y = Surv(time = rats$time, event = rats$status)
# if X is data.frame then it fails
X = rats[, c("litter", "rx", "sex")]
class(X)
cvfit = cv.ncvsurv(X, y)
predict(cvfit, X = X)

# this works
X2 = model.matrix(~ litter + rx + sex, data = rats)[, -1]
class(X2)
cvfit2 = cv.ncvsurv(X2, y)
predict(cvfit2, X = X2)

S_list = predict(cvfit2, X = X2, type = "survival") # S(t)
times = attr(S_list, "time") # |> unique()
surv_mat = do.call(
  rbind,
  lapply(S_list, function(S) S(c(times)))
)
surv_mat[, 300]



