library(mlr3)
library(mlr3proba)
library(mlr3pipelines)

# Univariate Cox

# encode `sex` (two-level factor)
task = tsk("rats")
enc = po("encode", method = "treatment")
task = enc$train(list(task))[[1L]]

# simple filter use
filter = flt("univariate_cox")
filter$calculate(task)
as.data.table(filter)

# transform to p-value
10^(-filter$scores)

# Use filter in a learner pipeline
# Note: `filter.cutoff` is selected randomly and should be tuned.
# The significance level of `0.05` serves as a conventional threshold.
# The filter returns the `-log10`-transformed scores so we transform
# the cutoff as well:
cutoff = -log10(0.05) # ~1.3

graph =
  po("filter", filter = flt("univariate_cox"), filter.cutoff = cutoff) %>>%
  po("learner", lrn("surv.coxph"))
learner = as_learner(graph)

learner$train(task)

# univariate cox filter scores
learner$model$surv.univariate_cox$scores

# only two features had a score larger than the specified `cutoff` and
# were used to train the CoxPH model
learner$model$surv.coxph$train_task$feature_names