library(mlr3verse)
library(ggparty) # needs to be installed for the app to work

task = tsk('spam')
task
all(task$missings() == 0)

if (FALSE) {
  ?autoplot.TaskClassif
  autoplot(task, type = 'pairs')

  # small test
  learner = lrn('classif.rpart', keep_model = TRUE, cp = 0.05)
  learner$param_set
  learner$train(task)
  p = autoplot(learner)
}

data_list = list()
index = 1
complexities = c(0.005, 0.008, 0.01, 0.03, 0.05, 0.1, 0.2)
for (cp in complexities) {
  message('cp = ', cp)
  learner = lrn('classif.rpart', keep_model = TRUE, cp = cp)
  learner$train(task)
  data_list[[index]] = list(cp = cp, learner = learner,
    tree_plot = autoplot(learner))
  index = index + 1
}

# save models and tree plots
saveRDS(data_list, file = 'models.rds')

# subset task to most important features used in the tree plots
features = names(data_list[[1]]$learner$importance())[1:10]
set.seed(42)
subtask = task$data(rows = sample(1:task$nrow, 50), cols = c(features, task$target_names))
saveRDS(subtask, file = 'spam_data.rds')
