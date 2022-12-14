library(mlr3verse)
library(mlr3proba)
library(survival)
library(ranger)
library(tidyverse)

# Task Lung ----
task = tsk('lung')
preprocess = po('encode') %>>% po('imputelearner', lrn('regr.rpart'))
task = preprocess$train(task)[[1]]
task$missings()

# fit ranger model ----
?ranger::ranger

set.seed(42)
fit = ranger::ranger(formula = NULL, data = task$data(), dependent.variable.name = 'time',
  status.variable.name = 'status')
fit

# S(t) = exp(-chf(t))
# e.g. for first individual:
all(fit$survival[1,] == exp(-fit$chf[1,]))
# chf = cumulative hazard function (total amount of accumulated risk up to time t)

head(fit$chf[1,])
head(fit$unique.death.times)
length(fit$unique.death.times)
dim(fit$survival) # observations x times
#fit$survival[1:5,1:5]

# SOS: native ranger() returns `distr` only!!!!
pred1 = predict(fit, data = task$data(rows = 1:5))

head(pred1$survival)
head(pred1$chf)

# mlr3proba returns crank as well!!! - computed via:
?survivalmodels::surv_to_risk

# simple and interpretable
# expected number of deaths or ENSEMBLE MORTALITY
surv_to_risk = function(x) {
  #assert_surv_matrix(x)
  cumH = -log(x)
  cumH[cumH == Inf] = 0
  rowSums(cumH)
}

res = mlr3proba::.surv_return(times = pred1$unique.death.times, surv = pred1$survival)
head(res$crank)

# calculate crank yourself!
crank2 = rowSums(pred1$chf) # sum CHF over all available times
stopifnot(crank2 == res$crank) # SAME IN mlr3proba and ranger

# different than taking the CHF on the last time in terms of ranking
# See the CHF curves that cross to make an assessment
crank3 = pred1$chf[,186]
rank(crank3) # 3rd, 2nd, 5th => to larger risk
rank(crank2) # 3rd, 5th, 2nd => to larger risk (better overall, see survival curves)

# average number of events expected across all timespan (I think)
rank(rowMeans(pred1$chf)) # 3rd, 5th, 2nd => to larger risk

## Plot S,CHF, cranks ----
surv_tbl = t(pred1$survival) %>%
  `colnames<-` (letters[1:5]) %>%
  as_tibble() %>%
  add_column(times = pred1$unique.death.times, .before = 1)
chf_tbl  = t(pred1$chf) %>%
  `colnames<-` (letters[1:5]) %>%
  as_tibble() %>%
  add_column(times = pred1$unique.death.times, .before = 1)

surv_tbl %>% ggplot(aes(x = times)) +
  geom_line(aes(y = a, color = '1')) +
  geom_line(aes(y = b, color = '2'), linetype = 'dashed') +
  geom_line(aes(y = c, color = '3')) +
  geom_line(aes(y = d, color = '4')) +
  geom_line(aes(y = e, color = '5'), linetype = 'dashed') +
  labs(x = 'Times', y = 'Survival') +
  theme_bw(base_size = 14) + theme(legend.position = 'top')

chf_tbl %>% ggplot(aes(x = times)) +
  geom_line(aes(y = a, color = '1')) +
  geom_line(aes(y = b, color = '2'), linetype = 'dashed') +
  geom_line(aes(y = c, color = '3')) +
  geom_line(aes(y = d, color = '4')) +
  geom_line(aes(y = e, color = '5'), linetype = 'dashed') +
  labs(x = 'Times', y = 'CHF') +
  theme_bw(base_size = 14) + theme(legend.position = 'top')

# mlr3 examples ----
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'C', num.random.splits = 1) # slower
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'extratrees') # seems very fast
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'maxstat') # seems very fast

set.seed(42)
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'logrank')
ranger_lrn$train(task)
ranger_lrn$model

# crank + distr
pred2 = ranger_lrn$predict(task, row_ids = 1:5)
pred2

# crank same
stopifnot(res$crank == pred2$crank)


# survival distributions same
surv_1st = 1 - pred2$distr$getParameterValue('cdf')[1,] %>% unname()
assertthat::are_equal(pred1$survival[1,], surv_1st, tol = 1e-10) # GREAT!

# Tune ranger ----
set.seed(42)
train_indxs = sample(seq_len(task$nrow), 180) # 80% for training
test_indxs  = setdiff(seq_len(task$nrow), train_indxs)
intersect(train_indxs, test_indxs)

ranger_lrn = lrn('surv.ranger', verbose =  FALSE,
  num.trees = to_tune(100, 1500),
  # num.features to choose from when splitting
  mtry.ratio = to_tune(p_dbl(0.01, 1, logscale = TRUE)), # in presence of many features!
  min.node.size = to_tune(p_int(1, 100, logscale = TRUE)),
  splitrule = to_tune(c('logrank', 'maxstat', 'C')),
  num.threads = 4)

dplyr::bind_rows(
  generate_design_random(ranger_lrn$param_set$search_space(), 20)$transpose()
)

ranger_at = AutoTuner$new(
  learner = ranger_lrn,
  resampling = rsmp('cv', folds = 5),
  measure = msr('surv.cindex'),
  terminator = trm('evals', n_evals = 30), # 10 - 100
  tuner = tnr('mbo')
)
ranger_at$train(task, row_ids = train_indxs)

# check if chosen learner has the hps it should (OK!)
ranger_at$learner$model
ranger_at$tuning_result

ranger_at$tuner$surrogate$model

p = ranger_at$predict(task, row_ids = test_indxs)
p
p$score()

autoplot(ranger_at$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(ranger_at$tuning_instance, type = 'performance')

