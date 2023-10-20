library(mlr3verse)
library(mlr3proba)
library(mlr3mbo)
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

surv_tbl %>%
  tidyr::pivot_longer(cols = c('a', 'b', 'c' , 'd', 'e'),
                      names_to = 'Patient',
                      values_to = 'Sprob') %>%
  ggplot(aes(x = times, y = Sprob, color = Patient)) +
  geom_line() +
  labs(x = 'Days', y = 'Survival Probability') +
  theme_bw(base_size = 18) +
  theme(legend.position = 'none')
ggsave(filename = 'mlr3proba/trees/lung_rsf5.png', width = 7, height = 5)

chf_tbl %>% ggplot(aes(x = times)) +
  geom_line(aes(y = a, color = '1')) +
  geom_line(aes(y = b, color = '2'), linetype = 'dashed') +
  geom_line(aes(y = c, color = '3')) +
  geom_line(aes(y = d, color = '4')) +
  geom_line(aes(y = e, color = '5'), linetype = 'dashed') +
  labs(x = 'Times', y = 'CHF') +
  theme_bw(base_size = 14) + theme(legend.position = 'top')

# mlr3 examples ----
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'C',) # slower
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'extratrees', num.random.splits = 1) # seems very fast
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'maxstat') # seems very fast

set.seed(42)
ranger_lrn = lrn('surv.ranger', verbose = FALSE, splitrule = 'logrank', importance = 'permutation')
ranger_lrn$train(task)
ranger_lrn$model
ranger_lrn$importance()

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
part = partition(task, ratio = 0.8, stratify = TRUE)
train_indxs = part$train
test_indxs  = part$test

## hyperparam notes (from survmob)
#' `num.trees` = p_int(100, 1500)
#' `mtry.ratio` = p_dbl(0.1, 0.9) # percentage of features to try at each node split
#' `min.node.size` = p_int(3, 20) # min number of samples per TERMINAL node
#'  - `extratrees` splitrule, favor more randomized trees [Geurts2006]:
#' `num.random.splits` = p_int(1, 100, logscale = TRUE)
#' - `maxstat` splitrule, from paper [Wright2017a]
#' `alpha` = p_fct(c(0.1, 0.3, 0.5, 0.7, 0.9))
#' `minprop` = p_fct(c(0, 0.1, 0.25, 0.4))

ranger_lrn = lrn('surv.ranger', verbose =  FALSE,
  num.trees = to_tune(1, 200),
  # num.features to choose from when splitting
  mtry.ratio = to_tune(p_dbl(0.1, 0.9)), # in presence of many features!
  # control tree size
  min.node.size = to_tune(p_int(3, 20)),
  #splitrule = 'logrank',
  #splitrule = 'extratrees',
  #num.random.splits = to_tune(p_int(1, 10)),
  splitrule = 'maxstat',
  alpha = to_tune(c(0.1, 0.3, 0.5, 0.7, 0.9)), # alpha = 0.5 (default)
  minprop = to_tune(c(0, 0.1, 0.25, 0.4)), # minprop = 0.1 (default)
  num.threads = 4
)

dplyr::bind_rows(
  generate_design_random(ranger_lrn$param_set$search_space(), 20)$transpose()
)

# or alternatively:
ranger_lrn = lrn('surv.ranger', verbose =  FALSE,
  splitrule = 'maxstat', num.threads = 4)

search_space = paradox::ps(
  num.trees = p_int(1, 200),
  mtry.ratio = p_dbl(0.1, 0.9),
  min.node.size = p_int(3, 20),
  alpha = p_fct(c(0.1, 0.3, 0.5, 0.7, 0.9)),
  minprop = p_fct(c(0, 0.1, 0.25, 0.4))
)

at = AutoTuner$new(
  learner = ranger_lrn,
  resampling = rsmp('insample'),
  measure = msr('oob_error'),
  search_space = search_space,
  terminator = trm('evals', n_evals = 30), # 10 - 100
  # result_by_default (best in the archive), result_by_surrogate_design
  # https://mlr3mbo.mlr-org.com/articles/mlr3mbo.html#putting-it-together
  tuner = tnr('mbo'),
  store_models = TRUE # for `oob_error`
)
at$train(task, row_ids = train_indxs)

at$archive
# check if chosen learner has the hps it should (OK!)
min(as.data.table(at$archive)$oob_error)
at$learner$model
at$tuning_result$x_domain[[1]]
at$archive$best()$x_domain[[1]]
at$tuning_result

at$tuner$result_function # check which result function is used =>
# if `best()` somewhere then it's the best (e.g. lower error hpc) in the archive
at$tuner$surrogate$model

p = at$predict(task, row_ids = test_indxs)
p
p$score()

autoplot(at$tuning_instance, type = 'parameter', trafo = TRUE)
autoplot(at$tuning_instance, type = 'performance')
