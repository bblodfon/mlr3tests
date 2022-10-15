library(mlr3)
library(mlr3learners)
library(mlr3benchmark)
library(ggplot2)
suppressMessages(library(dplyr))

# At least 2 learners and 2 datasets!!!

set.seed(1)

task = tsks(c('iris', 'sonar', 'wine', 'zoo'))
learns = lrns(c('classif.featureless', 'classif.rpart', 'classif.xgboost'))

lgr::get_logger('mlr3')$set_threshold('warn')

bm = benchmark(benchmark_grid(task, learns, rsmp('cv', folds = 3)))

# doesn't work
a = bm$aggregate()
ba = BenchmarkAggr$new(a)

# works
a %>% .[, c('task_id', 'learner_id') := .(as.factor(task_id), as.factor(learner_id))]
ba = BenchmarkAggr$new(a)

ba = as.BenchmarkAggr(bm, measures = msrs(c('classif.acc', 'classif.ce')))
class(ba)
?BenchmarkAggr

# Is there are a significant difference in the rankings of the learners over all the tasks?
ba$friedman_test()

# Compare every learner with each other
ba$friedman_posthoc(meas = 'acc')
autoplot(ba, type = 'fn', meas = 'acc')

# mean and error bars across tasks
?autoplot.BenchmarkAggr
autoplot(ba, type = 'mean', meas = 'acc', type = 'mean', level = 0.95)
autoplot(ba, type = 'box', meas = 'acc') # aggregated boxplot across tasks

# Critical Difference plot
autoplot(ba, type = 'cd', meas = 'acc', minimize = FALSE) # acc must be maximized

# xgboost is significantly better than featureless
# xgboost is not significantly better than rpart
# rpart is not significantly better than featureless
