# https://github.com/mlr-org/mlr3proba/issues/321
library(mlr3verse)
library(mlr3proba)

# veteran task
taskv = as_task_surv(x = survival::veteran, id = 'veteran',
  time = 'time', event = 'status')
poe = po('encode')
taskv = poe$train(list(taskv))[[1L]]
taskv

taskv$col_roles$stratum = 'status'

set.seed(42)
rr = resample(task = taskv, learner = lrn('surv.coxph'),
  resampling = rsmp('cv', folds = 8))

rcll = msr('surv.rcll')
rr$score(rcll) # error

# RCLL code check
dt = as.data.table(rr)
prediction = dt$prediction[[2]]
prediction$score(rcll) # error

out = rep(-99L, length(prediction$row_ids))
truth = prediction$truth
event = truth[, 2] == 1
event_times = truth[event, 1]
cens_times = truth[!event, 1]

# HERE!!!!!
!event # one TRUE only
#prediction$distr[!event] # <= THIS FAILS, 1 column to subset edge case?
prediction$distr[!event]$survival(cens_times) # single number
diag(prediction$distr[!event]$survival(cens_times))
