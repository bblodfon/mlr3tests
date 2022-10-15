library(mlr3verse)
library(mlr3proba)
library(boot)
library(tictoc)

# task
task = tsk('rats')
poe  = po('encode', method = 'treatment')
task = poe$train(list(task))[[1]]
test_task = task$clone(deep = TRUE)$filter(rows = 201:300)

# learner
cox = lrn('surv.coxph')
cox$train(task, row_ids = 1:200)

cox$predict(test_task)$score()

boot_fun = function(data, index, learner, measure) {
  learner$predict_newdata(data[index])$score(measure)
}

#' `data` is a data.table/data.frame object
boot_ci = function(data, learner, measure = mlr3::msr('surv.cindex'), nthreads = 1, nrsmps = 1000) {
  boot_res = boot::boot(data = data, statistic = boot_fun, R = nrsmps,
    parallel = 'multicore', ncpus = nthreads, learner = learner, measure = measure)

  bootci_res = boot::boot.ci(boot_res, type = c('basic', 'norm', 'perc', 'bca'))

  return(list(boot_res = boot_res, bootci_res = bootci_res))
}

tic()
a = boot_ci(data = test_task$data(), learner = cox, nthreads = 1, nrsmps = 1000)
toc()

tic()
b = boot_ci(data = test_task$data(), learner = cox, nthreads = 4, nrsmps = 1000)
toc()

tic()
c = boot_ci(data = test_task$data(), learner = cox, nthreads = 8, nrsmps = 1000)
toc()
