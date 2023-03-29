#' Buckley-James regression for right-censoring survival data with
#' high-dimensional covariates.
#' R code from vignette source `dlbcl.Rnw`
library(bujar)
library('survival')

data('chop')
dim(chop) # 181 patients
data('rchop')
dim(rchop) # 233 patients

# censoring rate in the CHOP data
sum(chop$status==0)/nrow(chop)

rchop = subset(rchop, select=colnames(rchop)%in% colnames(chop))
# add 1 for log-transformation
chop$survtime = chop$survtime + 1

# BJ-LS: BJ boosting with componentwise least squares ----
set.seed(123)
res.lin = bujar(y = log(chop[,1]), cens = chop[,2], x = chop[,-(1:2)],
  tuning = TRUE, cv = TRUE, n.cores = 1, mstop = 10, trace = TRUE)
# number of genes selected with BJ-LS
sum(res.lin$xselect==1)
coef.bj = coef(res.lin)
all(res.lin$coef.bj == coef.bj)
res.lin$learner
# estimated non-zero coefficients (only list 10)
# selected coefficients
coef.bj[abs(coef.bj) > 0]

# plot fit (not so nice)
plot(res.lin)

cutyear = 3

# he hasn't exposed the function but these must be the linear predictors
pred.bj = predict(res.lin, newx = rchop[,-(1:2)])
head(pred.bj)
pred.bj = exp(pred.bj) - 1
response = unname(pred.bj[,1]) # these are the predicted event (death) times of the RCHOP patients
response

# MSE from validation data
mse = sum(((response - rchop[,1])^2)/length(response))
mse
# MSE from training data at the BJ termination
res.lin$mse.bj

#' `glmboost` used as a regression learner
res.lin$res.fit

# low event time (< cutyear) => high risk
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime = rchop$survtime, status = rchop$status, group = group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data = dat.km)
fit.diff

fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km)
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
  lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, 0.3, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ-LS: BJ twin boosting with componentwise least squares ----
res.lin2 = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)], tuning=FALSE,
cv=FALSE, mstop=100, twin=TRUE, mstop2=100, trace = TRUE)
# number of genes selected with BJ-LS
class(res.lin2$res.fit) # bst => gradient boosting package
# bst => https://cran.r-project.org/web/packages/bst/index.html
sum(res.lin2$xselect==1)
coef.bj = coef(res.lin2)
coef.bj[abs(coef.bj)>0]
pred.bj = predict(res.lin2, newx=rchop[,-(1:2)])
head(pred.bj) # lps
pred.bj = exp(pred.bj) - 1 # death time
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff
fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km)
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
  lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, 0.3, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ LASSO ----
# The penalty tuning parameter is fixed at the 20th value in 100 penalty
# sequence values determined within each BJ iteration.
res.lasso = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
learner='enet2', tuning=FALSE, whichlambda = 20, trace = TRUE)
res.lasso$res.fit$call # glmreg
# how many genes selected by BJ-LASSO
sum(res.lasso$xselect==1)
#estimated non-zero coefficients (only list 10)
coef.bj = coef(res.lasso)
coef.bj[abs(coef.bj)>0][1:10]
pred.bj = predict(res.lasso, newx=rchop[,-(1:2)])
pred.bj = exp(pred.bj) - 1
head(pred.bj) # result
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff # not good enough, in the vignette example, he preselected some genes
# before using this method

fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km)
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
  lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, 0.3, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ SCAD ----
res.scad = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
learner='snet', tuning=FALSE, whichlambda=20, trace = TRUE)
# how many genes selected by BJ-SCAD
sum(res.scad$xselect==1)
#estimated non-zero coefficients (only list 10)
coef.bj = coef(res.scad)
coef.bj[abs(coef.bj)>0][1:10]
pred.bj = predict(res.scad, newx=rchop[,-(1:2)])
pred.bj = exp(pred.bj) - 1
head(pred.bj)
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff

fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km)
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
  lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, 0.3, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ-SM: BJ boosting with component-wise smoothing splines ----
set.seed(123)
#' uses `gamboost`
res.ss = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
  learner='pspline', tuning=FALSE, cv=FALSE, mstop=100, trace = TRUE)
# how many genes selected by BJ smoothing splines, only list 10
sum(res.ss$xselect==1)
colnames(res.ss$x)[res.ss$xselect==1][1:10]
pred.bj = predict(res.ss, newx=rchop[,-(1:2)])
pred.bj = exp(pred.bj) - 1
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff

fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km )
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
  lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, 0.3, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ-SM: BJ twin boosting with component-wise smoothing splines ----
set.seed(123)
res.ss2 = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
learner='pspline', tuning=TRUE, cv=TRUE, n.cores=1, mstop=100, twin=TRUE, mstop2=200)
# how many genes selected by BJ twin smoothing splines, only list 10
sum(res.ss2$xselect==1)
colnames(res.ss2$x)[res.ss2$xselect==1][1:10]
pred.bj = predict(res.ss2, newx=rchop[,-(1:2)])
pred.bj = exp(pred.bj) - 1
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff
fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km)
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, .1, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ-Tree: BJ boosting with regression stumps ----
#' like additive model since `degree` = 1
res.tree1 = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
  learner='tree',tuning=TRUE, cv=TRUE, mstop=100, n.cores=8, rng=123, trace = 1)

res.tree2 = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
  learner='tree', tuning=FALSE, cv=FALSE, mstop=100, n.cores=8, rng=123, trace = 1)

#Number of genes selected with tree, only list 10
sum(res.tree1$xselect==1)
class(res.tree1$res.fit) #' `gbm` for trees!
colnames(res.tree1$x)[res.tree1$xselect==1][1:10]
pred.bj = predict(res.tree1, newx=rchop[,-(1:2)])
pred.bj = exp(pred.bj) - 1
head(pred.bj)
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff

fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km)
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
  lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, .1, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ-Tree: BJ twin boosting with regression stumps ----
res.tree2 = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
  learner='tree', tuning=TRUE, cv=TRUE, mstop=1000, twin=TRUE, mstop2=100,
n.cores=1, rng=123)
#Number of genes selected with tree, only list 10
sum(res.tree2$xselect==1)
colnames(res.tree2$x)[res.tree2$xselect==1][1:10]
pred.bj = predict(res.tree2, newx=rchop[,-(1:2)])
pred.bj = exp(pred.bj) - 1
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff

fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km )
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, .1, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))

# BJ-TREE: BJ boosting with regression trees of degree 4 ----
res.tree4 = bujar(y=log(chop[,1]), cens=chop[,2], x=chop[,-(1:2)],
  learner='tree',degree=4, tuning=TRUE, cv=TRUE, mstop=100, rel.inf=TRUE,
  n.cores=1,rng=123)
#Number of genes selected with tree, only list 10
sum(res.tree4$xselect==1)
colnames(res.tree4$x)[res.tree4$xselect==1][1:10]
pred.bj = predict(res.tree4, newx=rchop[,-(1:2)])
pred.bj = exp(pred.bj) - 1
group = cut(pred.bj, breaks=c(-1, cutyear, 100), labels=c('high', 'low'))
dat.km = data.frame(survtime=rchop$survtime, status = rchop$status, group=group)
fit.diff = survdiff(Surv(survtime, status) ~ group, data=dat.km)
fit.diff

fit.surv = survfit(Surv(survtime, status) ~ group, data=dat.km )
plot(fit.surv, xlab='Year past therapy',ylab='Survival probability',
  lty = 1:2, col=c('red','blue'), mark.time=TRUE)
legend(1, .1, c('High risk', 'Low risk'), lty = 1:2, col=c('red','blue'))
