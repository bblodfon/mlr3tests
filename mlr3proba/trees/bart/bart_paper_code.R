library(BART)
#' Note: `lrn('regr/classif.bart')` in `mlr3extralearners` uses `dbarts` library
#' Note: https://www.rob-mcculloch.org/BBC/BBC.pdf (nice slides)

# Continuous ----
# Example f is from Friedman's MARS paper
f = function(x) {
  10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2+10*x[,4]+5*x[,5] # nolint
}
sigma = 3.0  # y = f(x) + sigma*z , z~N(0,1)
n = 100 # number of observations
set.seed(99)
x=matrix(runif(n*10),n,10) # 10 variables, only first 5 matter
dim(x)
Ey = f(x)
y=Ey+sigma*rnorm(n) # continuous output
length(y)
lmFit = lm(y~.,data.frame(x,y)) # compare lm fit to BART later

## train ----
#bartFit = mc.wbart(x,y, mc.cores = 8, seed=99, nskip=5, ndpost=5)
bartFit = mc.wbart(x,y, mc.cores = 8, seed=99) # ndpost = 1000 # default

# test feature_types that can be used
if (FALSE) {
  x = data.frame(
    x0 = 1:20,
    #x1 = LETTERS[1:20], # Variables of type character are not supported
    x2 = as.factor(c(rep(letters[1], 10), rep(letters[2], 10))),
    x3 = ordered(c(rep(letters[1], 10), rep(letters[2], 10))),
    x4 = c(!logical(10), logical(10))
  )
  y = Ey[1:20]
  fit = wbart(x,y) # seems okay
}

#' `ndpost` => number of posterior draws for f(x)
head(bartFit$yhat.train) # ndpost x nobs (train) (predictions matrix)
dim(bartFit$yhat.train)
mean(bartFit$yhat.train[,100])
bartFit$yhat.train.mean[100] # same

#' plot posterior draws for train set
i = order(bartFit$yhat.train.mean) # increasing
boxplot(bartFit$yhat.train[, i]) # prediction uncertainty quantified,
#' i.e. for every y you have 1000 y_hat from the posterior draw

## VI ----
# variable importance
bartFit$varprob # posterior samples
bartFit$varprob.mean #' mean variable selection probability (fixed, more meaningful with `sparse = TRUE`)
bartFit$varcount.mean # mean observed counts of a variable used in the splits

## predict ----
x.test = x[1:3,]
pred = predict(bartFit, newdata = x.test, mc.cores = 8)
dim(pred) # ndpost x nobs (test)
pred[,1] # can get posterior intervals from this for subject #1

## BART bs lm ----
fitmat = cbind(y, Ey, lmFit$fitted, bartFit$yhat.train.mean)
colnames(fitmat) = c('y','Ey','lm','bart')
print(cor(fitmat))

## Boston housing ----
library(mlr3verse)
task = tsk('boston_housing')
d = task$data()
part = partition(task)

x = as.data.frame(d[,c('rm', 'lstat')])
dim(x) # 2 variables only
x.train = x[part$train,]
x.test  = x[part$test, ]

y = d$medv
y.train = y[part$train]
y.test  = y[part$test ]

head(cbind(x, y))

# train
nd = 200
burn = 20
bart_fit = mc.wbart(x.train = x.train, y.train = y.train,
  nskip = burn, ndpost = nd, mc.cores = 4, seed = 42) # parallel

# serial execution
bart_fit = wbart(x.train = x.train, y.train = y.train,
  nskip = burn, ndpost = nd)
length(bart_fit$sigma) # keeps the burn-in as well
plot(bart_fit$sigma, type = "l")
abline(v = burn, lwd = 2, col = "red")

# predict
set.seed(42)
#' don't use `mc.pwbart`, more difficult that way
p = predict(bart_fit, newdata = x.test, mc.cores = 10)
dim(p) # nd x nobs in test set, nd = number of draws
# RMSE
sum((y.test - apply(p, 2, mean))^2/length(y.test))
summary(as.double(y.test - apply(p, 2, mean)))

## PDP ----
#' PDPs are created by selecting a feature of interest and varying its values
#' while keeping all other features at a fixed value or an average value.
#' The corresponding predictions are then plotted, revealing how the feature's
#' effect on the model's output changes.
#' For us here, feature of interest is `lstat` and model output is `medv`

#' exclude `medv` : median house value
#' exclude `town, chas` due to factor conversion later (otherwise `predict` doesn't work)
x = as.data.frame(d[,-c('medv','town','chas')])

x.train = x[part$train,]
dim(x.train) # 16 variables
length(y.train)

set.seed(12)
post4 = mc.wbart(x.train, y.train, mc.cores = 5)
H = length(y.train)
#' make `x.test`: create sample `lstat` values within the range of the train set,
#' and add these as columns to all the rest of the variable matrix
L = 41 #' how many `lstat` values to sample
lstat = x.train[, 'lstat']
x.train.no.lstat = x.train[colnames(x.train) != 'lstat']
x_lstat = seq(min(lstat), max(lstat), length.out = L) #' sequence of `lstat`s

#' for each `lstat` value, concat the whole train matrix
x.test = cbind(x.train.no.lstat, lstat = x_lstat[1])
for(j in 2:L) {
  x.test = rbind(x.test, cbind(x.train.no.lstat, lstat = x_lstat[j]))
}
pred = predict(post4, x.test, mc.cores = 10)
dim(pred) # nd (1000) x obs in test set

partial = matrix(nrow = 1000, ncol = L)
for(j in 1:L) {
  h = (j - 1) * H + 1:H # columns (obs) to aggregate over (shifted, H in size)
  partial[, j] = apply(pred[, h], 1, mean)
}
dim(partial) # nd x L => post draws for each lstat

plot(x_lstat, apply(partial, 2, mean), type = "l", ylim = c(10, 50),
  xlab = "lstat", ylab = "mdev")
lines(x_lstat, apply(partial, 2, quantile, probs = 0.025), lty = 2)
lines(x_lstat, apply(partial, 2, quantile, probs = 0.975), lty = 2)

# Binary ----
#' see: `/opt/R/4.2.1/lib/R/library/BART/demo/nhanes.pbart1.R`
#' `pbart` => probit BART, `lbart` => logit BART (more comput. taxing)
## PDP (Fig. 7 in paper) ----
#' How BMI affects probability of back (or neck pain) outcome
#' for males vs females
B = 10
?arq
data(arq)
str(arq)
arth = as.matrix(arq)

N = length(arth[, 'riagendr'])

table(arth[, 'riagendr']) # gender (1 for males, 2 for females)
colnames(arth)[4]
table(arth[, 'arq010de']) # back pain
colnames(arth)[3]
table(arth[, 'arq010a'])  # neck pain
summary(arth[, 'bmxbmi']) # Body Mass Index

train_data = arth[ , 5:10]
colnames(train_data)
post1 = mc.pbart(x.train= train_data, y.train=arth[ , 4],
  mc.cores=B, seed=99) # back pain binary model
post2 = mc.pbart(x.train= train_data, y.train=arth[ , 3],
  mc.cores=B, seed=99) # neck pain binary model

bmxbmi = seq(15, 45, by=5)
H = length(bmxbmi)

# make test set
for(i in 1:2) { # 2 sexes
  for(j in 1:H) { # H bmi values
    x. = train_data
    x.[, 'riagendr'] = i
    x.[, 'bmxbmi']   = bmxbmi[j]
    if(i==1 && j==1) x.test = x.
    else x.test = rbind(x.test, x.)
  }
}

table(x.test[, 'riagendr'])
table(x.test[, 'bmxbmi'])

pred1 = predict(post1, newdata=x.test, mc.cores=B)
pred2 = predict(post2, newdata=x.test, mc.cores=B)

M = nrow(pred1$prob.test) # test obs
pd1 = matrix(nrow=M, ncol=H)
pd2 = matrix(nrow=M, ncol=H)

par(mfrow=c(1, 2))

for(j in 1:H) {
  h = (j-1)*N
  pd1[ , j] = apply(pred1$prob.test[ , h+1:N], 1, mean)
  h = h+N*H
  pd2[ , j] = apply(pred1$prob.test[ , h+1:N], 1, mean)
}

pd1.mean = apply(pd1, 2, mean)
pd2.mean = apply(pd2, 2, mean)
pd1.025 = apply(pd1, 2, quantile, probs=0.025)
pd2.025 = apply(pd2, 2, quantile, probs=0.025)
pd1.975 = apply(pd1, 2, quantile, probs=0.975)
pd2.975 = apply(pd2, 2, quantile, probs=0.975)

plot(bmxbmi, pd1.mean, type='l', col='blue',
  ylim=0:1, xlab='BMI', ylab=expression(p(x)),
  sub='Low-back pain: M(blue) vs. F(red)')
##sub='Low-back/buttock pain: M(blue) vs. F(red)')
lines(bmxbmi, pd1.025, type='l', col='blue', lty=2)
lines(bmxbmi, pd1.975, type='l', col='blue', lty=2)
lines(bmxbmi, pd2.mean, type='l', col='red')
lines(bmxbmi, pd2.025, type='l', col='red', lty=2)
lines(bmxbmi, pd2.975, type='l', col='red', lty=2)
lines(bmxbmi, rep(0, H), type='l')
lines(bmxbmi, rep(1, H), type='l')

for(j in 1:H) {
  h = (j-1)*N
  pd1[ , j] = apply(pred2$prob.test[ , h+1:N], 1, mean)
  h = h+N*H
  pd2[ , j] = apply(pred2$prob.test[ , h+1:N], 1, mean)
}

pd1.mean = apply(pd1, 2, mean)
pd2.mean = apply(pd2, 2, mean)
pd1.025 = apply(pd1, 2, quantile, probs=0.025)
pd2.025 = apply(pd2, 2, quantile, probs=0.025)
pd1.975 = apply(pd1, 2, quantile, probs=0.975)
pd2.975 = apply(pd2, 2, quantile, probs=0.975)

plot(bmxbmi, pd1.mean, type='l', col='blue',
  ylim=0:1, xlab='BMI', ylab=expression(p(x)),
  sub='Neck pain: M(blue) vs. F(red)')
lines(bmxbmi, pd1.025, type='l', col='blue', lty=2)
lines(bmxbmi, pd1.975, type='l', col='blue', lty=2)
lines(bmxbmi, pd2.mean, type='l', col='red')
lines(bmxbmi, pd2.025, type='l', col='red', lty=2)
lines(bmxbmi, pd2.975, type='l', col='red', lty=2)
lines(bmxbmi, rep(0, H), type='l')
lines(bmxbmi, rep(1, H), type='l')

## Sparse example ----
f = function(x) #only the first 5 matter
  sin(pi*x[ , 1]*x[ , 2]) + 2*(x[ , 3]-.5)^2+x[ , 4]+0.5*x[ , 5]-1.5

sigma = 1.0  #y = f(x) + sigma*z where z~N(0, 1)
P = 100      #number of covariates
thin = c(10, 10, 10)
n = c(200, 1000, 5000)

post = as.list(1:3)

for(i in 1:3) {
  N = n[i]
  set.seed(12)
  x.train = matrix(runif(N*P), N, P)
  Ey.train = f(x.train)
  y.train = ((Ey.train + sigma*rnorm(N))>0) * 1

  post[[i]] = mc.pbart(x.train, y.train, mc.cores=10,
    keepevery=thin[i], sparse=TRUE, seed=99) #' `keepevery` => thinning
}

# max proccesing time ~20 secs with 10 CPUs
post[[3]]$proc.time

for(i in 1:3) {
  N = n[i]
  plot(post[[i]]$varprob.mean, col=c(rep(2, 5), rep(1, P-5)),
    main=paste0('N:', N, ', P:', P, ', thin:', thin[i]),
    ylab='Selection Probability', ylim=c(0, 0.3),
    pch=1+45*(post[[i]]$varprob.mean <= 1/P))
  lines(c(0, 100), c(1/P, 1/P))

  # (Prob < 1/P) => 46, otherwise 1 (selected)
  print(table(1+45*(post[[i]]$varprob.mean <= 1/P)))
}

## mlr3 spam example ----
spam_task = tsk('spam')
set.seed(42)
part = partition(spam_task)
x = spam_task$data(cols = spam_data$feature_names)
y = spam_task$data(cols = spam_data$target_names)

x.train = x[part$train, ]
x.test  = x[part$test,  ]
y.train = y[part$train, ]
y.test  = y[part$test,  ]

# ranger
library(mlr3extralearners)
rf_lrn = lrn('classif.ranger', num.threads = 10, predict_type = 'prob')
rf_lrn$train(task = spam_task, row_ids = part$train)
p = rf_lrn$predict(task = spam_task, row_ids = part$test)
p$confusion

my_msrs = msrs(c('classif.ce', 'classif.acc', 'classif.bbrier'))
p$score(measures = my_msrs) # test data
# brier score DYI
sum((as.numeric(p$truth == 'spam') - p$prob[,'spam'])^2)/length(p$truth)
# train data (KINDA OVERFITS!)
rf_lrn$predict(task = spam_task, row_ids = part$train)$score(my_msrs)

# BART
bart_lrn = mc.pbart(
  x.train = as.data.frame(x.train),
  y.train = as.numeric(y.train$type == 'spam'),
  ntree = 200,
  keepevery = 10, mc.cores = 10, seed = 42
)
bart_lrn$proc.time
#' ~10 secs for `keepevery = 1`
#' ~40 secs for `keepevery = 10` and `ntree = 500`
#' ~3min for `keepevery = 100`

# test data
bart_p = predict(bart_lrn, newdata = as.data.frame(x.test), mc.cores = 10)
mean_pred = bart_p$prob.test.mean # mean probability of spam
length(mean_pred)

# brier score for test data
sum((as.numeric(y.test$type == 'spam') - mean_pred)^2)/length(mean_pred)

# train data is not overfitting!
mean_pred_train = predict(bart_lrn, newdata = as.data.frame(x.train),
  mc.cores = 10)$prob.test.mean

# brier score for train data
sum((as.numeric(y.train$type == 'spam') - mean_pred_train)^2)/length(mean_pred_train)

# Plot credible intervals for some test samples
library(tidyverse)
library(ggdist)
pred_tbl = bart_p$prob.test
dim(pred_tbl)
colnames(pred_tbl) = paste0('test', 1:ncol(pred_tbl))
test_row_ids = c(1,303, 1000, 1300)
test_row_names = paste0('test', test_row_ids)
p$truth[test_row_ids] # 2 spam, 2 non-spam

a = pred_tbl %>%
  as_tibble() %>%
  pivot_longer(everything(), names_to = 'sample', values_to = 'posterior') %>%
  mutate(
    sample = factor(sample),
    sample = forcats::fct_reorder(sample, .x = posterior, .desc = TRUE)
  )

probs = c(0.8, 0.95)
my_cols = c('#A6CEE3', '#1F78B4')
a %>%
  filter(sample %in% test_row_names) %>%
  ggplot(aes(y = sample, x = posterior)) +
  stat_pointinterval(
    aes(interval_color = after_stat(level)),
    .width = probs,
    point_interval = 'median_qi', # median quantile interval
    point_size = 2,
    point_color = '#D55E00'
  ) +
  scale_color_manual(
    values = my_cols,
    labels = scales::label_percent()(probs),
    aesthetics = 'interval_color',
    guide = guide_legend(
      override.aes = list(point_color = NA),
      title = 'Credible Intervals'
    )
  ) +
  xlim(c(0,1)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', color = '#E41A1C') +
  labs(title = 'Credible Interval predictions',
    subtitle = '2 spam and 2 non-spam samples')

# Survival ----
## Data transform ----
# (time, delta) => binary discrete-time transform (no covariates)
times = c(2.5, 1.5, 3.0)
delta = c(1, 1, 0)
a = surv.pre.bart(times = times, delta = delta)

# post = mc.surv.bart(x.train, times = times, delta = delta,
# x.test = x.train, ndpost = M, mc.cores = 10, seed = 99)
# pred = predict(post, pre$tx.test, mc.cores = 10)

## Lung dataset ----
## load survival package for the advanced lung cancer example
data(lung)

N = length(lung$status)

table(lung$ph.karno, lung$pat.karno)

## if physician's KPS unavailable, then use the patient's
h = which(is.na(lung$ph.karno))
lung$ph.karno[h] = lung$pat.karno[h]

times = lung$time
delta = lung$status-1 ##lung$status: 1=censored, 2=dead
##delta: 0=censored, 1=dead

## this study reports time in days rather than weeks or months
## coarsening from days to weeks or months will reduce the computational burden
##times = ceiling(times/30)
times = ceiling(times/30.44)  ## weeks

table(times)
table(delta)

## matrix of observed covariates (CHOOSE 3!!!)
x.train = cbind(lung$sex, lung$age, lung$ph.karno)
dim(x.train)

## lung$sex:        Male=1 Female=2
## lung$age:        Age in years
## lung$ph.karno:   Karnofsky performance score (dead=0:normal=100:by=10)
##                  rated by physician
dimnames(x.train)[[2]] = c('M(1):F(2)', 'age(39:82)', 'ph.karno(50:100:10)')

table(x.train[ , 1])
summary(x.train[ , 2])
table(x.train[ , 3])

## PDP: Sex differences ----
##test BART with token run to ensure installation works
post = mc.surv.bart(x.train=x.train, times=times, delta=delta,
  nskip=1, ndpost=1, keepevery=1)

## Not run:
## run one long MCMC chain in one process (~100 secs)
#set.seed(99)
#post = surv.bart(x.train=x.train, times=times, delta=delta)

## in the interest of time, consider speeding it up by parallel processing
## run "mc.cores" number of shorter MCMC chains in parallel processes
post = mc.surv.bart(x.train=x.train, times=times, delta=delta,
  mc.cores=15, seed=99)

pre = surv.pre.bart(times=times, delta=delta, x.train=x.train, x.test=x.train)

K = pre$K # unique times
K
N * K # for estimation ??? (train data has less than that)
M = post$ndpost
M # 1000

pre$tx.test = rbind(pre$tx.test, pre$tx.test)
pre$tx.test[ , 2] = c(rep(1, N*K), rep(2, N*K)) # Males (1) and Females (2)
## sex pushed to col 2, since time is always in col 1

pred = predict(post, newdata=pre$tx.test, mc.cores=8)

pd = matrix(nrow=M, ncol=2*K) # 2 times is for the 2 sexes

for(j in 1:K) {
  h = seq(j, N*K, by=K)
  pd[ , j]   = apply(pred$surv.test[ , h], 1, mean) # 1 => mean across patients
  pd[ , j+K] = apply(pred$surv.test[ , h+N*K], 1, mean)
}

pd.mu  = apply(pd, 2, mean) # 2 => mean across posterior draws
pd.025 = apply(pd, 2, quantile, probs=0.025)
pd.975 = apply(pd, 2, quantile, probs=0.975)

males = 1:K
females = males+K

plot(c(0, pre$times), c(1, pd.mu[males]), type='s', col='blue',
  ylim=0:1, ylab='S(t, x)', xlab='t (weeks)',
  main=paste('Advanced Lung Cancer ex. (BART::lung)',
    "Friedman's partial dependence function",
    'Male (blue) vs. Female (red)', sep='\n'))
lines(c(0, pre$times), c(1, pd.025[males]), col='blue', type='s', lty=2)
lines(c(0, pre$times), c(1, pd.975[males]), col='blue', type='s', lty=2)
lines(c(0, pre$times), c(1, pd.mu[females]), col='red', type='s')
lines(c(0, pre$times), c(1, pd.025[females]), col='red', type='s', lty=2)
lines(c(0, pre$times), c(1, pd.975[females]), col='red', type='s', lty=2)

## mlr3 example ----
# Convert Lung Dataset to SurvTask
library(mlr3proba)
df = cbind.data.frame(x.train, times, delta)
colnames(df) = make.names(colnames(df))
lung_task = mlr3proba::as_task_surv(x = df, time = 'times', event = 'delta')
autoplot(lung_task)

# partition to train and test
part2 = partition(lung_task)
length(part2$train) # 153 patients
length(part2$test)  # 75  patients

### Cox ----
cox = lrn('surv.coxph')

cox$train(lung_task, row_ids = part2$train)
cox$model
p2 = cox$predict(lung_task, row_ids = part2$test)
p2

measures = msrs(c('surv.cindex', 'surv.brier', 'surv.rcll'))
p2$score(measures)

### BART ----
# TRAIN
bart_fit2 = mc.surv.bart(
  x.train = x.train[part2$train, ],
  times = times[part2$train],
  delta = delta[part2$train],
  seed = 42,
  mc.cores = 15
)

dim(bart_fit2$yhat.train) # on the transformed data
dim(bart_fit2$prob.train) # same dim as above
length(bart_fit2$prob.train.mean)
bart_fit2$K # unique times
bart_fit2$times
dim(bart_fit2$tx.train)

# train via data transform first
pre2 = surv.pre.bart(
  times = times[part2$train],
  delta = delta[part2$train],
  x.train = x.train[part2$train, ]
)

bart_fit3 = mc.surv.bart(
  x.train = pre2$tx.train,
  y.train = pre2$y.train,
  seed = 42,
  mc.cores = 15
)

# SAME YES!!!
all(bart_fit3$prob.train.mean == bart_fit2$prob.train.mean)

# BART => PREDICT
# So for prediction on the test set, we do the following (needs the train set):
pre3 = surv.pre.bart(
  times = times[part2$train], # length(times) == length(delta) == nrow(x.train) arguments
  delta = delta[part2$train],
  x.train = x.train[part2$train, ],
  x.test  = x.train[part2$test , ] # add this line, just the covariates X of the test set
)

# x.test has K * N rows
pre3$K * length(part2$test)
dim(pre3$tx.test) # => this is what we need

# predict!
bart_p2 = predict(bart_fit2, newdata = pre3$tx.test, mc.cores = 15)
bart_p3 = predict(bart_fit3, newdata = pre3$tx.test, mc.cores = 15)

# YES! SAME
all(bart_p2$surv.test.mean == bart_p3$surv.test.mean)
bart_p2$surv.test.mean # mean of posterior draws

dim(bart_p2$surv.test) # ndraws x (N_test * K)

#' using `mc.surv.pwbart`
bart_p22 = mc.surv.pwbart(
  x.test = pre3$tx.test,
  binaryOffset = bart_fit2$offset,
  treedraws = bart_fit2$treedraws,
  mc.cores = 15,
  type = 'pbart',
  transposed = FALSE,
  nice = 19L
)
all(bart_p2$surv.test.mean == bart_p22$surv.test.mean) # YES

M = nrow(bart_p2$surv.test) # ndpost
K = bart_p2$K # unique times (from train set)
N = length(part2$test) # #obs (test)

stopifnot(length(bart_p2$surv.test.mean) == N * K) # for each patient, K times

# build matrix of mean posterior draws of the survival function
surv = matrix(nrow = N, ncol = K) # test patients X times

for(i in 1:N) {
  #print(paste0('#obs: ', i))
  indxs = ((i-1)*K + 1):(i*K)
  surv[i,] = bart_p2$surv.test.mean[indxs]
}

# another way to do the same (vectorized)
surv2 = matrix(bart_p2$surv.test.mean, nrow = N, ncol = K, byrow = TRUE)
all(surv == surv2)

rbenchmark::benchmark(
  "for-loop" = {
    surv = matrix(nrow = N, ncol = K);
    for(i in 1:N) {
      indxs = ((i-1)*K + 1):(i*K);
      surv[i,] = bart_p2$surv.test.mean[indxs]
  }},
  "vectorized" = {
    indices = matrix((0:(K-1)) + rep((0:(N-1)) * K, each = K) + 1, ncol = K, byrow = TRUE);
    surv2 = matrix(data = bart_p2$surv.test.mean[indices], nrow = N, ncol = K, byrow = TRUE)
  },
  "vectorized2" = {
    surv2 = matrix(bart_p2$surv.test.mean, nrow = N, ncol = K, byrow = TRUE)
  },
  replications = 1000,
  columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self")
)

p3 = mlr3proba::.surv_return(times = bart_p2$times, surv = surv)
names(p3) # a list with crank, surv matrix, ...

pred3 = PredictionSurv$new(
  row_ids = part2$test,
  truth = Surv(times[part2$test], delta[part2$test]),
  crank = p3$crank,
  distr = p3$distr,
  check = TRUE
)

# does worse than cox!
pred3$score(measures)
p2$score(measures)

# check some survival prob
p2$distr[1:3]$survival(77) # cox
p3$distr[1:3,"77"] # BART
pred3$distr[1:3]$survival(77) # same as above, YAY

## PAAD mRNA Example ----
# data
library(mlr3extralearners)
task_mRNA = readRDS(file = gzcon(url('https://github.com/bblodfon/paad-survival-bench/blob/main/data/task_mRNA_flt.rds?raw=True'))) # 1000 features
task_mRNA
part3 = partition(task_mRNA, ratio = 0.8)
length(part3$train) # 116
length(part3$test) # 29

# RSF
rsf = lrn('surv.ranger', num.threads = 10)
rsf$train(task_mRNA, row_ids = part3$train)
rsf_pred = rsf$predict(task_mRNA, row_ids = part3$test)
rsf_pred$score(measures)

# BART
bart = lrn('surv.bart', mc.cores = 15, sparse = FALSE, ntree = 50, seed = 42)
bart$train(task_mRNA, row_ids = part3$train)
bart_pred = bart$predict(task_mRNA, row_ids = part3$test)
bart_pred$score(measures)

imp = bart$model$varprob.mean # not sorted
P = 1000
plot(imp, # col = c(rep(2, 5), rep(1, P-5)),
  main=paste0('N:', length(part3$train), ', P:', P, ', thin:', 10),
  ylab='Selection Probability', #ylim=c(0, 0.3),
  pch=1+45*(imp <= 1/P))
lines(c(0, 1000), c(1/P, 1/P))

# selected (Prob < 1/P) => 46, otherwise 1 (selected)
print(table(1+45*(imp <= 1/P)))
