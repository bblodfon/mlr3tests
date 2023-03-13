#' LOW-DIM data => `survival::aareg`, `timereg::aalen` and `timereg::cox.aalen`
#' timereg has `predict` method and seems to be the best option to use
#' HIGH-DIM data => `ahaz::ahazpen`
library(mlr3verse)
library(mlr3proba)
library(survival)
library(timereg)
library(ggfortify)
library(ahaz)

# task lung ----
task = tsk('lung')
prep = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task = prep$train(task)[[1]]
task

# better to scale since center value is like reference
pos = po('scale')
task2 = pos$train(list(task))[[1L]]

# 'survival' Aalen model ----
# h(t) = b0(t) + b1(t) * x1 + b2(t) * x2 + ...
# cumulative hazard H(t) = Intercept(t) + X * B(t)
?survival::aareg()
fit = survival::aareg(formula = task2$formula(), data = task2$data())
fit

#' plot estimated coef (hazard) functions (coef estimates vary across time - b(t))
#' the `Intercept` is the hazard over time at reference levels of all covariates
#' For continuous values reference is 0 => so centering at least is needed for
#' correct interpretation of Intercept(t) hazard over time.
#' E.g. covariate age is in range [40,70]
#' https://stats.stackexchange.com/questions/569703/how-to-understand-aalen-additive-regression-model
par(mfrow=c(3,3))
# plot(fit)
?ggfortify:::autoplot.aareg()
autoplot(fit, maxtime = 300) # plot shows for 1 unit increase in each covariate, the additive hazard over time

## NO PREDICT for survival::aareg()!!!

# 'timereg' ----
data(sTRACE)

## Cox-Aalen ----
# combined model
?cox.aalen
out = cox.aalen(
  Surv(time,status==9) ~ prop(age)+prop(sex)+prop(vf)+prop(chf)+prop(diabetes),
  data = sTRACE
)

# makes Lin, Wei, Ying test for proportionality and time effects
summary(out)
par(mfrow=c(2,3))
plot(out,score=1)

?timereg::predict.cox.aalen
p = timereg::predict.cox.aalen(
  object = out,
  newdata = sTRACE[1:3, c("age", "sex", "vf", "chf", "diabetes")]
)
plot(p)

length(p$time) # 260 timepoints
apply(p$RR, 1, mean) # relative risk terms (n x timepoints)
p$S0 # survival predictions (n x timepoints)
p$se.S0 # pointwise standard error of the above

## Aalen ----
?aalen
#' Covariates vary with time (t)
out = timereg::aalen(Surv(time,status==9)~age+sex+diabetes+chf+vf,
  sTRACE,max.time=7,n.sim=100, resample.iid = 1)

summary(out) # check which covariates are constant
par(mfrow=c(2,3))
plot(out)

p = predict.aalen(out,X=rbind(c(1,0,0,0,0),rep(1,5)))
p$S0 # some probabilities here...
?plot.aalen()
par(mfrow=c(2,2))
plot(p,multiple=1,se=0,uniform=0,col=1:2,lty=1:2)
plot(p,multiple=0,se=1,uniform=1,col=1:2)

#' Semi-parametric additive hazards model => const(`variable`)
#' effect over time is constant
# Fits semi-parametric additive hazards model
out = aalen(Surv(time,status==9)~const(age)+const(sex)+const(diabetes)+chf+vf,
  sTRACE, max.time=7, n.sim=100, resample.iid = 1)
summary(out)
par(mfrow=c(2,3))
plot(out)

p2 = predict(out, X=rbind(c(1,0,0),c(1,1,0)), Z=rbind(c(55,0,1),c(60,1,1)))
head(p2$S0[,1:5]) # survival prob
head(p2$se.S0[,1:5])

?plot.aalen()
par(mfrow=c(2,2))
plot(p2,multiple=1,se=0,uniform=0,col=1:2,lty=1:2)
plot(p2,multiple=0,se=1,uniform=1,col=1:2)

# mRNA data ----
# Issues on training, see below!
#task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA
task_mRNA = readRDS(file = gzcon(url('https://github.com/bblodfon/paad-survival-bench/blob/main/data/task_mRNA_flt.rds?raw=True'))) # 1000 features

survival::aareg(formula = task_mRNA$formula(), data = task_mRNA$data()) # integer overflow

timereg::aalen(formula = task_mRNA$formula(), data = task_mRNA$data(), n.sim=50) # didn't finish after some minutes

timereg::cox.aalen(formula = task_mRNA$formula(), data = task_mRNA$data(), n.sim=50) # fails

# ahaz (low-dim) ----
#' Semi-parametric additive hazards model (LOW-DIM)
#' All covariates coefficients (betas) are CONSTANT over time
data(sorlie)

# Break ties
set.seed(10101)
time = sorlie$time + runif(nrow(sorlie)) * 1e-2

# Survival data + covariates
surv = Surv(time, sorlie$status)
X = as.matrix(sorlie[,15:24]) # ONLY LOW-DIM (n > p)
X[1:6, 1:6]

# Fit additive hazards model
?ahaz::ahaz
fit1 = ahaz(surv, X)
fit1
summary(fit1)

## Predictions ----
?plot.ahaz
coef(fit1) # Coefficient (beta) estimates (TRAIN, same as from `summary`)
lp2 = predict(fit1, type = 'lp') # linear predictors (TRAIN)
head(lp2) # additive hazard to the baseline per patient

p2 = predict(fit1, type = 'cumhaz') # Cum. Baseline Hazard (Breslow, TRAIN)
head(p2$cumhaz)
plot(fit1) # CumHazard Baseline (Breslow)

all(p2$cumhaz[2:length(p2$cumhaz)] == cumsum(p2$br)) # `$br` => Breslow ho(t)

## Survival calculation ----
# h(t) = ho(t) + lp  # lp = b_hat * x_new
# H(t) = cumsum(h(t))
# S(t) = exp(-H(t))
ht = matrix(data = 0, nrow = length(lp2), ncol = length(p2$times))
dim(ht) # different times per patient
rownames(ht) # patient ids
colnames(ht) = p2$times # times

for (index in 1:length(lp2)) {
  ht[index,] = p2$br + lp2[index]
}

Ht = t(apply(ht, 1, cumsum))
# head(Ht)
s = exp(-Ht)
# for some patients things look ok, for others NOT!
s[1,] # 1 => 1+ !!!!!!!!!!
s[4,] # 1 => 0
plot(s[6,]) # 1+ !!!!!!!!!!

# Residuals - model fit
resid = predict(fit1, type = 'residuals')
# Decorrelate, standardize, and check QQ-plots
stdres = apply(princomp(resid)$scores,2,function(x){x/sd(x)})
par(mfrow = c(2,2))
for(i in 1:4) {
  qqnorm(stdres[,i])
  abline(c(0,1))
}

# predict on new data
# newX = X[1:3,]
?ahaz::predict.ahaz
predict(fit1, newX = X, type = 'lp') # wrong format?

# Univariate models
X = as.matrix(sorlie[,3:ncol(sorlie)])
fit2 = ahaz(surv, X, univariate = TRUE)
fit2

# ahazpen ----
X = as.matrix(sorlie[,3:ncol(sorlie)]) # all variables
dim(X)

?ahaz::ahazpen()
?ahaz::tune.ahazpen()
# Fit additive hazards regression model with elasticnet penalty
model = ahazpen(surv, X, standardize = TRUE,
  penalty = lasso.control(alpha = 0.1))
#' `dfmax` = 30, limits the maximum number of variables in the model (speedup)
#' `penalty.wgt` => 0 means don't penalize variable (`keep` also does the same)
plot(model)
model
coef(model)[1,]

# Fit additive hazards regression model with lasso penalty
model2 = ahazpen(surv, X, standardize = TRUE, #lambda = 0.01,
  penalty = lasso.control(alpha = 1))
model2 = ahazpen(surv, X, standardize = TRUE, penalty = 'lasso') # same as above
dim(coef(model2))
model2

# predict
?predict.ahazpen

lp = predict(model, X[1:3,], type = 'lp') # lambda = NULL (calculate for all lambdas)
class(lp) # dgeMatrix
dim(lp) # 3x100 (test_patients x lambdas), which lambda?

# specify lambda
lp = predict(model, newX = X[1:3,], type = 'lp', lambda = 0.2)
lp # for 3 patients (vector)

predict(model, newX = X[1:3,], lambda = 0.2, type = 'cumhaz') # incorrect dimensions for newX
p = predict(model, newX = X, lambda = 0.2, type = 'cumhaz') # NEEDS THE WHOLE TRAINING MATRIX
p$cumhaz # it's the baseline hazard ONLY

# tune.ahazpen ----
# Training/test data
set.seed(20202)
train = sample(1:nrow(sorlie),76)
test  = setdiff(1:nrow(sorlie),train)

# Run cross validation on training data
set.seed(10101)
#' uses `ahazpen` to select a sequence of `lambdas`, and finds the optimal `lambda`
#' by doing 5-fold CV with a likelihood loss measure, see next function:
?ahaz.tune.control
?tune.ahazpen
#' it's like using `cv.glmnet`
cv.las = tune.ahazpen(surv = surv[train,], X = X[train,],
  tune = cv.control(nfolds = 5, reps = 10), standardize = TRUE,
  penalty = lasso.control(alpha = 0.8))
plot(cv.las)
cv.las
cv.las$lambda.min # it's the minimum lambda

# Check fit on the test data
lp_test = predict(cv.las, newX = X[test,], type = 'lp')
exp(lp_test) # all risks are close to 1 ... (don't know if it's a bad thing or if it should interpreted that way at all)
pred = predict(cv.las, newX = X[train,], type = 'cumhaz') # TRAIN DATA
plot(survfit(surv[test,]~I(lp_test < median(lp_test))), main="Low versus high risk")
