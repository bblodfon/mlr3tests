# https://github.com/mlr-org/mlr3pipelines/issues/694
library(mlr3proba)
library(mlr3pipelines)
task = tsk('lung')

trgmut = po('targetmutate')
trgmut$train(list(task))