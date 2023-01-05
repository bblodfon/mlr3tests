library(mlr3verse)
library(mlr3proba)
library(survival)
library(timereg)

# task lung ----
task = tsk('lung')
prep = po('encode', method = 'treatment') %>>% po('imputelearner', lrn('regr.rpart'))
task = prep$train(task)[[1]]
task
if (FALSE) {
# better to scale since center value is like reference
pos = po('scale')
task2 = pos$train(list(task))[[1L]]

# survival Aalen model ----
# h(t) = b0(t) + b1(t) * x1 + b2(t) * x2 + ...
?survival::aareg()
fit = survival::aareg(formula = task$formula(), data = task$data())
fit

## plot estimated coef (hazard) functions (coef estimates vary across time - b(t))
plot(fit)

fit2 = survival::aareg(formula = task2$formula(), data = task2$data())
fit2

## NO PREDICT for survival::aareg()!!!

# timereg models ----
data(sTRACE)

?cox.aalen
## Cox-Aalen model ----
out = cox.aalen(Surv(time,status==9)~prop(age)+prop(sex)+
    prop(vf)+prop(chf)+prop(diabetes),data=sTRACE)

# makes Lin, Wei, Ying test for proportionality
summary(out)
par(mfrow=c(2,3))
plot(out,score=1)

timereg::predict.cox.aalen(object = out, newdata = sTRACE)

## Aalen model ----
?aalen
out = aalen(Surv(time,status==9)~age+sex+diabetes+chf+vf,
  sTRACE,max.time=7,n.sim=100, resample.iid = 1)

summary(out)
par(mfrow=c(2,3))
plot(out)
pout<-predict(out,X=rbind(c(1,0,0,0,0),rep(1,5)))
pout$S0 # some probabilities here...
par(mfrow=c(2,2))
plot(pout,multiple=1,se=0,uniform=0,col=1:2,lty=1:2)
plot(pout,multiple=0,se=1,uniform=1,col=1:2)

# mRNA data ----
# Issues on training, see below!
task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA
survival::aareg(formula = task_mRNA$formula(), data = task_mRNA$data()) # integer overflow

timereg::aalen(formula = task_mRNA$formula(), data = task_mRNA$data(), n.sim=50) # didn't finish after some minutes
}

task_mRNA = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')$mRNA
print('start')
timereg::cox.aalen(formula = task_mRNA$formula(), data = task_mRNA$data(), n.sim=50) # fails
