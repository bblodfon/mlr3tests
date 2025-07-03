# checking for independent censoring: add status as variable and see if its significant for predicting outcome? paper?
library(mlr3verse)
library(mlr3proba)

# Task lung ----
task = tsk('lung')
pre = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task = pre$train(task)[[1]]
task$missings()
task # task with clinical data

# CoxPH (mlr3proba) ----
cox = lrn('surv.coxph')

# Add status indicator as variable
task$cbind(data.frame(status2 = task$status()))
task
cox$train(task)

# Check random censoring assumption (RCA)
summary(cox$model) # status2 p-value is close to 1 => insignificant => RCA SATISFIED
