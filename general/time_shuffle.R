library('mlr3pipelines')
library('mlr3proba')
library('paradox')  # for paramset of our new po-class

task = tsk('lung')

# PipeOpTimeShuffle ----

# inherit from PipeOpTaskPreproc. It takes care of
# defining 'Task' (in our case TaskSurv) input and output.
# We have to overwrite the .train_task()/.predict_task()-functions.
PipeOpTimeShuffle = R6::R6Class('PipeOpTimeShuffle', inherit = PipeOpTaskPreproc,
  public = list(
    # default initialize function header for a concrete pipeop class:
    # it is a good idea to have the `id = 'xxx'` and `param_vals = list()`
    # arguments and pass them on to super$initialize.
    initialize = function(id = 'timeshuffle', param_vals = list()) {
      # as a demo I am also including a hyperparameter here
      p = ps(replace = p_lgl(tags = 'required'))
      p$values = list(replace = FALSE)
      super$initialize(id = id, param_set = p, param_vals = param_vals,
        can_subset_cols = FALSE, task_type = 'TaskSurv'
      )
    }
  ),
  private = list(
    .train_task = function(task) {
      pvals = self$param_set$get_values()
      newtime = task$data(cols = 'time')
      if (nrow(newtime) > 1) {  # `sample` 'misbehaves' when 1st argument has len 1!
        newtime$time = sample(newtime$time, replace = pvals$replace)
      }
      # $cbind() overwrites old task columns.
      # I am not sure if this breaks inside resample(),
      # you should test it...
      task$cbind(newtime)
    },
    .predict_task = function(task) task
  )
)

pots = PipeOpTimeShuffle$new()

task$head(2)
pots$train(list(task))[[1]]$head(2) # different
pots$train(list(task))[[1]]$head(2) # different
pots$predict(list(task))[[1]]$head(2) # unchanged during prediction
