library(BART)
library(mlr3proba)
library(mlr3pipelines)

data(lung)
lung[, 3]
group = -which(is.na(lung[ , 7])) ## remove missing row for ph.karno
times = lung[group, 2]   ##lung$time
delta = lung[group, 3]-1 ##lung$status: 1=censored, 2=dead
delta ##delta: 0=censored, 1=dead
x.train = as.matrix(lung[group, c(4, 5, 7)]) ## matrix of observed covariates
head(x.train)

x.test = matrix(nrow=84, ncol=3) ## matrix of covariate scenarios
dimnames(x.test)[[2]] = dimnames(x.train)[[2]]
i = 1
for(age in 5*(9:15)) for(sex in 1:2) for(ph.karno in 10*(5:10)) {
  x.test[i, ] = c(age, sex, ph.karno)
  i = i+1
}
head(x.test)

pre = surv.pre.bart(times=times, delta=delta, x.train=x.train, x.test=x.test)
head(pre$y.train)
head(pre$tx.train)
head(pre$tx.test)

colnames(pre$tx.train)
colnames(pre$tx.test)
colnames(pre$times)
pre$times
pre$K


x = cbind(as.data.frame(x.train), times, status = delta)
head(x)

t2 = as_task_surv(x = x, event = "status", time = "times")
t2

pre$K == length(t2$unique_times()) # TRUE

po_disc = po("trafotask_survclassif_disctime", cut = t2$unique_times())
po_disc
task_classif = po_disc$train(list(t2))[[1L]]
task_classif

# same number of rows
task_classif$nrow == nrow(pre$tx.train)

# target same
all(task_classif$truth() == pre$y.train)

# long data same
age1 = task_classif$data(cols = "age")[[1]]
age2 = pre$tx.train[,"age"]
all(age1 == age2)

ph.karno1 = task_classif$data(cols = "ph.karno")[[1]]
ph.karno2 = pre$tx.train[,"ph.karno"]
all(ph.karno1 == ph.karno2)

tend1 = task_classif$data(cols = "tend")[[1]]
tend2 = pre$tx.train[, "t"]
all(times1 == times2)

# make x_test with fake (time,status) as these are not used
x_test = cbind(as.data.frame(x.test), times = 1, status = 1)
t3 = as_task_surv(x = x_test, event = "status", time = "times")
pred_task = po_disc$predict(list(t3))[[1L]]

# same nrows
pred_task$nrow == nrow(pre$tx.test)

# long data same
age1 = pred_task$data(cols = "age")[[1]]
age2 = pre$tx.test[,"age"]
all(age1 == age2)

