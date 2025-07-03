# This script proves that we can't use a TaskSurv with interval censoring on a Learner
# that doesn't support this, so we need Learner-Task properties!!!
library(icenReg)
library(survival)
library(mlr3extralearners)

data("miceData")

task = as_task_surv(x = miceData, time = "l", time2 = "u", type = "interval")
lrn("surv.coxph")$train(task) # doesn't work
lrn("surv.rpart")$train(task) # failed, time > 0
l = lrn("surv.kaplan")
l$train(task)

mice2 = miceData
mice2$l = mice2$l + 1
task = as_task_surv(x = mice2, time = "l", time2 = "u", type = "interval")
lrn("surv.rpart")$train(task) # failed

lrn("surv.rpart")$train(task2)
l3 = lrn("surv.rfsrc")
l3$encapsulate(fallback = lrn("surv.kaplan"), method = "callr")
l3$timeout = c(train = 10)

l3$train(task) # fails (C++ error) due to mismatch
# WARN  [17:54:56.597] [mlr3] train: NAs introduced by coercion to integer range
# ERROR [17:54:56.601] [mlr3] train: callr process exited with status -11
