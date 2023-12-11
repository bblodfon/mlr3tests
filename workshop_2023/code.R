library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(tidyverse)
library(skimr)

# mlr3 - (almost) everything is an object!

# Survival Tasks
mlr_tasks
as.data.table(mlr_tasks)[task_type == 'surv'] # tasks to "play"

?as_task_surv
data = survival::rats
data$sex = factor(data$sex, levels = c("f", "m"))
tsk_rats = as_task_surv(x = data, time = "time",
  event = "status", type = "right", id = "rats")
tsk_rats$head()

lung = survival::lung
lung %>% as_tibble()
skimr::skim(lung)

lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

task = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
task$missings() # missing values!
task$truth()
task$status()
task$times()

# Preprocessing pipeline!
?mlr_pipeops

# Encode factors
poe = po("encode", method = "treatment")

# Model-based missing data imputation
?mlr_pipeops_imputelearner
po_imp = po("imputelearner", learner = lrn('regr.rpart'))
task


# Learners - CoxPH, KM
# how to get info about a model? help()

cox = lrn("surv.coxph")
km = lrn("surv.km")
surv_tree = lrn("surv.rpart")

# Train + test split
part = partition(task)

# CoxPH

# KM

# Prediction types
# distr, crank, lp, response

# Evaluate performance
mlr_measures$keys(pattern = "surv")

# measures
mlr_measures$keys(pattern = "surv")

# discrimination
m = msr("surv.cindex") # harrell's c-index

# calibration
m = msr("surv.dcalib")

# putting it all together to a resample/benchmark


