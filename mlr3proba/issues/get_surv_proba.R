###################################################
# https://github.com/mlr-org/mlr3proba/issues/273 #
###################################################
library(mlr3verse)
library(dplyr, warn.conflicts = FALSE)

task = tsk('rats')
task$select(c("litter", "rx")) # leave out factor column `sex`

#task_test = task$clone(deep = TRUE)
#task$filter(1:295)
#task_test$filter(296:300) # I did this task split because I can't specify
# `row_ids` in the pipe operator below when training and predicting, but let me
# know if you know of an easier way

# `glmnet` does not compute `distr` on its own so we use the `distrcompositor`
glmnet_lrn = ppl(
  "distrcompositor",
  learner = lrn("surv.glmnet"),
  estimator = "kaplan",
  form = "aft"
) %>% as_learner()

# Now we can do the following:
pred = glmnet_lrn$train(task, row_ids = 1:295)$predict(task, row_ids = 296:300)
pred

# Example from the book (https://mlr3book.mlr-org.com/special-tasks.html#composition)
# doesn't work => `graph_learner = TRUE` is the cause (maybe not a bug)
#glm.distr = ppl("distrcompositor", learner = lrn("surv.glmnet"), estimator = "kaplan", form = "ph", overwrite = FALSE, graph_learner = TRUE)

# so we split it
#glmnet_pipe$train(task) # output may seem worrying...
#pred = glmnet_pipe$predict(task_test)[[1]]

# The following matrix has for every unique time point in the training dataset
# (columns), the survival probability for each of the test rats (rows) - YES!!!
pred$data$distr[,1:16]
all(task$unique_times() == colnames(pred$data$distr))

# SOS ----
# Don't work with
pred$data$distr
# But with:
pred$distr
# Get the same matrix of probabilities on the times of the training dataset with:
all(1 - pred$distr$getParameterValue('cdf') == pred$data$distr)

# `pred$data$distr` is the survival
all(pred$distr$getParameterValue('survival') == pred$data$distr)

# So, if I am correct, the above result is only for the times in the train
# dataset, not in between or later timepoints that I might want. So, let's
# work with the following:
pred$distr

# Get the mean survival probabilities for each test rat? (a (5x1) result)
pred$distr$mean() # Inf?!

# specific timepoints for which I want survival probability for the test rats
times = c(1,10,42,70,120)
pred$distr$mean(times) # Inf!? I was expecting a (5x5) result?
pred$distr$median() # NA!? I was expecting a (5x1) result?
pred$distr$cumHazard(times) # seems okay

# Are the following the survival probabilities at the specific timepoints?
pred$distr$survival(times) # seems okay

?Matdist # doesn't include any documentation on *survival* and *hazard* methods!

##########################################################
# Survival curves for the predicted `task_test` (5 rats) #
##########################################################

learner = glmnet_pipe$pipeops$surv.glmnet$learner_model
class(learner)
# doesn't work?
?plot.LearnerSurv
plot(x = learner, task, fun = "survival", newdata = task_test$data(), ylim = c(0, 1), xlim = c(0,120))

# This seem to work but why it goes only until time =~ 50?
plot(pred$distr, fun = "survival")
# same as:
matplot(t(pred$data$distr), type = 'l')

# let's try another learner
my_learner = lrn("surv.coxph")
my_learner$train(task)
# `ind` is not used anymore?
plot(my_learner, task, fun = "survival", ind = 10)
plot(my_learner, task, fun = "survival", newdata = task_test$data(),
  xlim = c(0,80), ylim = c(0, 1)) # again only until ~50
