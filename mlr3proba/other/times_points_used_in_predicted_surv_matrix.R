library(mlr3proba)
library(mlr3extralearners)

lrn_ids = mlr_learners$keys("^surv")
# remove some learners (DL models, take too much time: bart, mboost has issues, etc.)
lrn_ids = lrn_ids[!grepl(pattern = "blackboost|mboost|deep|pchazard|coxtime|priority|dnn|loghaz|gamboost", lrn_ids)]
# remove learners that don't predict `distr`
lrn_ids = lapply(lrn_ids, function(id) {
  learner = lrn(id)
  if ("distr" %in% learner$predict_types) {
    id
  } else {
    NULL
  }
}) |> unlist()

lrn_ids # ~18 survival learners

task = tsk("gbcs")
set.seed(42)
part = partition(task, ratio = 0.5)

# keep different time points sets to check later
train_times = task$unique_times(part$train)
train_event_times = task$unique_event_times(part$train)

test_times = task$times(part$test)
test_status = task$status(part$test)
test_event_times = sort(unique(test_times[test_status == 1]))
test_times = sort(unique(test_times))

all_times = task$unique_times()
all_event_times = task$unique_event_times()

res = lapply(lrn_ids, function(id) {
  print(id)
  learner = lrn(id)

  if (id == "surv.parametric") {
    learner$param_set$set_values(.values = list(discrete = TRUE))
  }

  if (id == "surv.bart") {
    learner$param_set$set_values(
      # low settings to make computation faster
      .values = list(nskip = 1, ndpost = 3, keepevery = 2, mc.cores = 14)
    )
  }

  if (id == "surv.cforect") {
    learner$param_set$set_values(.values = list(cores = 14))
  }

  if (id == "surv.ranger") {
    learner$param_set$set_values(.values = list(num.threads = 14))
  }

  learner$train(task, part$train)
  p = learner$predict(task, part$test)
  times = as.numeric(colnames(p$data$distr))

  # return discrete times for which we have the predicted S(times)
  times
})

names(res) = lrn_ids

# example times:
head(res$surv.aorsf)

which_times = lapply(lrn_ids, function(id) {
  times = res[[id]]
  #print(id)

  lgl_list = suppressWarnings(list(
    train = all(times == train_times),
    train_event = all(times == train_event_times),
    test = all(times == test_times),
    test_event = all(times == test_event_times),
    all = all(times == all_times),
    all_Events = all(times == all_event_times)
  ))

  names(which(mlr3misc::map_lgl(lgl_list, isTRUE)))
})

names(which_times) = lrn_ids

# Results: which time points are used by each learner in the predicted survival matrix?
which_times
