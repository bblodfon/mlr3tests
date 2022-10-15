# https://github.com/mlr-org/mlr3pipelines/issues/694
library('mlr3pipelines')
library('mlr3proba')
library('paradox')

# task
task = tsk('lung')
pre = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task = pre$train(task)[[1]]
#task$missings()
#task$head()

# PipeOpSurvShuffle ----
PipeOpSurvShuffle = R6::R6Class('PipeOpSurvShuffle', inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = 'survshuffle', param_vals = list()) {
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
      surv  = task$data(cols = c('time', 'status'))
      if (nrow(surv) > 1) {  # `sample` 'misbehaves' when 1st argument has length 1!
        surv$time   = sample(surv$time,   replace = pvals$replace)
        surv$status = sample(surv$status, replace = pvals$replace)
      }
      # to test if this works...
      task$cbind(surv)
    },
    .predict_task = function(task) task
  )
)

poss = PipeOpSurvShuffle$new()

task$head(4)
poss$train(list(task))[[1]]$head(4) # ok,different

# Make pipeline ----
gr = ppl('greplicate', poss %>>% po('learner', lrn('surv.coxph')), 10)
gr$keep_results = TRUE # SOS => for checking the results of the `poss` PipeOp
gr$plot()

## manual train and test tasks (holdout once)
train_task = task$clone()$filter(rows = 1:200)
test_task  = task$clone()$filter(rows = 201:228)

gr$train(train_task)
## check that targets were shuffled (train data)
train_task$head(4)
gr$pipeops$survshuffle_1$.result[[1L]]$head(4)
gr$pipeops$survshuffle_2$.result[[1L]]$head(4)

res = gr$predict(test_task)
## check that targets were NOT shuffled (test data)
test_task$head(4)
gr$pipeops$survshuffle_1$.result[[1L]]$head(4)
gr$pipeops$survshuffle_2$.result[[1L]]$head(4)

scores = list()
for (i in 1:length(res)) {
  scores[[i]] = res[[i]]$score()
}
cindex = dplyr::bind_rows(scores)
cindex # ok, got the result

# try with resample ----
graph = poss %>>% po('learner', lrn('surv.coxph'))
graph$keep_results = TRUE
learner = as_learner(graph)

rsm = rsmp('holdout', ratio = 0.7)
rsm$instantiate(task)

set.seed(1)
rs = resample(task, learner, rsm, store_models = TRUE, store_backends = TRUE)

## check that targets were shuffled (train data) - CAN'T DO THIS!
train_task2 = task$clone()$filter(rs$resampling$instance$train)
# train_task2 = task$clone()$filter(rsm$instance$train)$head(4) # same
train_task2$head(4)
rs$learners[[1]]$graph_model$pipeops$survshuffle$.result$output$head(4)
# hm, this is different but it is actually the test task (unchanged)
# check the model:
rs$learners[[1]]$graph_model$pipeops$surv.coxph$state$model

set.seed(1)
out = learner$train(train_task2)
out$model$surv.coxph$model

## check that targets were NOT shuffled (test data) - CHECKED
test_task2 = task$clone()$filter(rs$resampling$instance$test)
test_task2$head(4)
rs$score()$learner[[1]]$graph_model$pipeops$survshuffle$.result$output$head(4)

# try with benchmark
learner = as_learner(poss %>>% po('learner', lrn('surv.coxph')))
cop = po('copy', 10) # copy task 10 times
task_list = cop$train(list(task))
bm_grid = benchmark_grid(task_list, learner, rsmp('holdout', ratio = 0.7))
bm = benchmark(bm_grid, store_models = TRUE, store_backends = TRUE)

bm$score(msr('surv.cindex', weight_meth = 'G2'))
