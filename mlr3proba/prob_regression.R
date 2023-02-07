library(mlr3verse)
library(mlr3proba)
library(ggplot2)

?mlr_graphs_probregrcompositor

# create our pipeline
l = as_learner(ppl("probregrcompositor",
  learner = lrn("regr.ranger"),
  learner_se = lrn("regr.featureless"),
  dist = "Normal")
)

# train and predict
task = tsk("mtcars")
split = partition(task, ratio = 0.9)
l$train(task, split$train)
p = l$predict(task, split$test)
p # prediction object has distr!!!
