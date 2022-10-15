# https://github.com/mlr-org/paradox/issues/363
library(mlr3verse)
library(mlr3proba)
library(survival)

# Veteran Task
task = as_task_surv(x = veteran, time = 'time', event = 'status')
poe = po('encode')
task = poe$train(list(task))[[1]]
task

learner = lrn('surv.ranger',
  splitrule = to_tune(c('logrank', 'extratrees', 'C', 'maxstat')),
  num.random.splits = to_tune(ps(
    num = p_int(1, 100, depends = splitrule == 'extratrees'),
    .extra_trafo = function(x, param_set) {
      if (x$splitrule == 'extratrees') {
        y = list(num.random.splits = x$num)
      } else {
        y = list(num.random.splits = 1)
      }
      y
    }, .allow_dangling_dependencies = TRUE))
)

learner = lrn('surv.ranger', num.threads = 4, num.trees = 50)
l2 = learner$clone(deep = TRUE)

# This doesn't work!
l2$param_set$values$splitrule = 'maxstat'
l2$param_set$values$num.random.splits = NA # may not be NA!
l2$param_set$default$num.random.splits

# works!!!
search_space = ps(
  splitrule = p_fct(c('logrank', 'extratrees', 'C', 'maxstat')),
  num.random.splits = p_int(1, 100, depends = splitrule == 'extratrees')
)

generate_design_random(search_space, 5)

ranger_at = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  search_space = search_space,
  terminator = trm('evals', n_evals = 30),
  tuner = tnr('random_search')
)

ranger_at$train(task)

res = ranger_at$archive$data %>% as_tibble() %>% pull(x_domain)
bind_rows(res)

# works as well!
learner = lrn('surv.ranger', num.threads = 4, num.trees = 50,
  splitrule = to_tune(c('logrank', 'extratrees', 'C', 'maxstat')),
  num.random.splits = to_tune(p_int(1, 100, depends = splitrule == 'extratrees')))

ranger_at = AutoTuner$new(
  learner = learner,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 30),
  tuner = tnr('random_search')
)

ranger_at$train(task)
