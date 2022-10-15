# https://github.com/mlr-org/mlr3pipelines/issues/691
library(mlr3verse)

set.seed(42)
data = data.table::data.table(y = runif(100),
  var1 = factor(c(NA, rep('a', 49), rep('b', 49), NA)), # 3 levels
  var2 = c(runif(50, min = 3, max = 5), runif(50)),
  var3 = runif(10))

task = TaskRegr$new("example", data, target = "y")
task$missings()

imp = po('imputelearner', lrn('classif.rpart'))
pre = imp %>>% po('encode', method = 'treatment')

task2 = pre$train(task)[[1L]]
task2$missings() # var1.factor?
task2$data(cols = 'var1.factor')[[1L]] # all zeros

task3 = imp$train(list(task))[[1L]]
task3$col_info # 'factor' level added?

fix = po('fixfactors')
pre2 = imp %>>% fix %>>% po('encode', method = 'treatment')

task4 = pre2$train(task)[[1L]]
task4
task4$data()
task4$col_info
task4$missings()
