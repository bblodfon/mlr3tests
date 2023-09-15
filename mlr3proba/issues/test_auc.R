library(mlr3proba)

task = tsk("rats")
set.seed(1)
part = partition(task)

keys = mlr_measures$keys("^surv")
auc_keys = keys[grep(x = keys, pattern = "auc")]

# CoxPH
l1 = lrn("surv.coxph")
p1 = l1$train(task, part$train)$predict(task, part$test)
p1

for (auc_msr in msrs(auc_keys)) {
  print(p1$score(auc_msr, task = task, train_set = part$train, learner = l1))
}

# Survival Tree
l2 = lrn("surv.rpart")
p2 = l2$train(task, part$train)$predict(task, part$test)
p2
for (auc_msr in msrs(auc_keys)) {
  # NaN => mlr3 => incompatibility with return type
  print(p2$score(auc_msr, task = task, train_set = part$train, learner = l2))
}

# CoxLasso
library(mlr3extralearners)
l3 = lrn('surv.glmnet', s = 0.01)
task2 = task$clone()$select(c("litter", "rx"))
p3 = l3$train(task2, part$train)$predict(task2, part$test)
for (auc_msr in msrs(auc_keys)) {
  # error => mlr3proba => learner must be Cox
  print(p3$score(auc_msr, task = task2, train_set = part$train, learner = l3))
}
