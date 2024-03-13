library(survivalmodels)
set_seed(1234)
library(mlr3)
library(mlr3proba)

## get the `whas` task from mlr3proba
whas <- tsk("whas")

## create our own task from the rats dataset
rats_data <- survival::rats

## convert characters to factors
rats_data$sex <- factor(rats_data$sex, levels = c("f", "m"))
rats <- TaskSurv$new("rats", rats_data, time = "time", event = "status")

## combine in list
tasks <- list(whas, rats)

library(paradox)

search_space <- ps(
  ## p_dbl for numeric valued parameters
  dropout = p_dbl(lower = 0, upper = 1),
  weight_decay = p_dbl(lower = 0, upper = 0.5),
  learning_rate = p_dbl(lower = 0, upper = 1),

  ## p_int for integer valued parameters
  nodes = p_int(lower = 1, upper = 32),
  k = p_int(lower = 1, upper = 4)
)

search_space$extra_trafo <- function(x, param_set) {
  x$num_nodes = rep(x$nodes, x$k)
  x$nodes = x$k = NULL
  return(x)
}

library(mlr3tuning)

create_autotuner <- function(learner) {
  AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = rsmp("holdout"),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = 2),
    tuner = tnr("random_search")
  )
}

## learners are stored in mlr3extralearners
library(mlr3extralearners)

## load learners
learners <- lrns(
  paste0("surv.", c("coxtime", "deephit", "deepsurv", "loghaz", "pchazard")),
  frac = 0.3, early_stopping = TRUE, epochs = 10, optimizer = "adam"
)

# apply our function
learners <- lapply(learners, create_autotuner)


library(mlr3pipelines)

create_pipeops <- function(learner) {
  po("encode") %>>% po("scale") %>>% po("learner", learner)
}

## apply our function
learners <- lapply(learners, create_pipeops)
learners[[1]]

devtools::session_info()

