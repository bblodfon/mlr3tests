library(mlr3proba)
library(dplyr)
library(ggplot2)

# task
task = tsk('lung')
pre = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task = pre$train(task)[[1]]

cox = lrn('surv.coxph')

part = mlr3::partition(task)

p = cox$train(task, part$train)$predict(task, part$test)

times = task$unique_event_times()
times = times[times > 30] # filter some to avoid issue with the calculation
scores = numeric(length(times))
index = 1
for (time in times) {
  scores[index] = p$score(msr('surv.cindex', cutoff = time))
  index = index + 1
}

scores_uno = numeric(length(times))
index = 1
for (time in times) { # Uno's C
  scores_uno[index] = p$score(
    msr('surv.cindex', weight_meth = 'G2', cutoff = time),
    task = task,
    train_set = part$train
  )
  index = index + 1
}

t = tibble::tibble(time = times, har = scores, uno = scores_uno)

t %>% tidyr::pivot_longer(cols = c('har', 'uno'), names_to = 'Cindex',
  values_to = 'score') %>%
  ggplot(aes(x = time, y = score, color = Cindex)) +
  geom_point()
