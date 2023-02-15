library(mlr3verse)
library(dplyr)

# Create tasks (mtcars) ----
data("mtcars", package = "datasets")
data = mtcars[, 1:3]
str(data)

task_mtcars = as_task_regr(x = data, target = "mpg", id = "cars")
task_mtcars # invokes print()

# get stuff :)
task_mtcars$ncol
task_mtcars$nrow
task_mtcars$row_ids %>% head()
task_mtcars$data() %>% head()
task_mtcars$data(rows = c(1,3,5))
task_mtcars$feature_names
task_mtcars$target_names
task_mtcars$data(rows = c(1,3,5), cols = "mpg")
mtcars_tbl = tibble::as_tibble(task_mtcars$data())

# Doesn't work (and it shouldn't)
as_task_regr(x = iris, target = 'Species')

?autoplot.TaskRegr()
autoplot(task_mtcars, type = 'target')
autoplot(task_mtcars, type = 'pairs')

# Predefined tasks ----
as.data.table(mlr_tasks) %>% filter(task_type == 'surv')

# Two ways to get a predefined task
actg_task  = mlr_tasks$get('actg')
actg_task2 = tsk('actg')

# get help by using key
help("mlr_tasks_mtcars")
help("mlr_tasks_iris")

# BinaryClassIf - set positive class ----
data("Sonar", package = "mlbench")
task_sonar = as_task_classif(x = Sonar, target = 'Class', positive = 'R')
task_sonar$positive
task_sonar$positive = 'M' # change positive class
task_sonar$positive

# Visualize Data (selecting a small number of variables) ----
?mlr3viz::autoplot.TaskClassif
task_sonar$select(cols = head(task_sonar$feature_names, 3)) # use first 3 columns only
task_sonar
autoplot(task_sonar) # default (target frequencies)
autoplot(task_sonar, type = 'duo') # boxplot
autoplot(task_sonar, type = 'pairs')

# Column roles: is it a feature, a target, something else?
task_sonar$col_roles

#' SOS: ADD ROWNAMES in the data and change its role to `names`
#' (not a feature!) for plotting purposes
#' This changes the VIEW on the data
data2 = as.data.table(mtcars[,1:3], keep.rownames = 'car_names')
mtcars_task2 = as_task_regr(x = data2, target = 'mpg', id = 'cars')

mtcars_task2$col_roles # `car_names` is a feature
mtcars_task2$data() %>% as_tibble() # appears in the data as well!

mtcars_task2$set_col_roles(cols = 'car_names', roles = 'name')
mtcars_task2$col_roles # `car_names` is now a name, not a feature
mtcars_task2$data() %>% head() # not in data anymore!

names(mtcars_task2$row_roles)
mtcars_task2$row_roles # Notice the HOLDOUT rows!
#' Usually used for validation/testing => e.g. rows that have NA in the target
#' column can be used for testing but NOT for (supervised) training

# Task mutators ----
task_penguins = tsk("penguins")
help('mlr_tasks_penguins') # nice dataset to play!!!

task_penguins$select(c("body_mass", "flipper_length")) # keep only these features
task_penguins$filter(1:3) # keep only these rows
task_penguins$head()

task_penguins$cbind(data.frame(letters = letters[1:3])) # add column letters
task_penguins$head()

task_penguins$data() %>% as_tibble()

# Task feature renaming ----
task_penguins
task_penguins$rename(old = task_penguins$feature_names,
                     new = paste0(task_penguins$feature_names, '1'))
task_penguins

# Task target mutation ----
?mlr_pipeops_targetmutate
task = tsk("boston_housing")
trgmut = po('targetmutate',
  trafo = function(x) log(x, base = 2),
  inverter = function(x) list(response = 2 ^ x$response)
)
# Note that this example is ill-equipped to work with
# `predict_type == "se"` predictions

trgmut = po('targetmutate', trafo = function(x) { x/2} )
trgmut = po('targetmutate')

task2 = trgmut$train(list(task))$output

# see if it changed target => YES
head(task$data()[['medv']])
head(task2$data()[['medv']])

res = trgmut$predict(list(task))$output # changes target
head(res$data()[['medv']])

## just updating target?
library(mlr3)
trafo_fun = function(x) {
  factor(ifelse(x$Species == "setosa", "setosa", "other"))
  }
po = mlr3pipelines:::PipeOpUpdateTarget$new(param_vals = list(trafo = trafo_fun, new_target_name = "setosa"))
po$train(list(tsk("iris")))
po$predict(list(tsk("iris")))

# Lung dataset (survival) ----
lung = survival::lung
task_lung = as_task_surv(x = survival::lung, time = 'time', event = 'status',
  id = 'lung')

# The same below, conversion from 1/2 to 0/1 for the event/status co-variate is parsed correctly it seems (`mlr3proba` => 0.4.4), but good practise to set it
lung$status = (lung$status == 2L)
task_lung2 = TaskSurv$new("lung", backend = lung, time = "time", event = "status")
