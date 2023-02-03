# Accelerated Oblique Random Survival Forest Learner
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(survival)
library(aorsf)
library(tidyverse)

# Task Lung ----
task = tsk('lung')
preprocess = po('encode') %>>% po('imputelearner', lrn('regr.rpart'))
task = preprocess$train(task)[[1]]
task$missings()
task

# mlr3 example ----
orsf_lrn = lrn('surv.aorsf')
orsf_lrn$param_set
orsf_lrn$help()


orsf_lrn$train(task, row_ids = 1:200)
orsf_lrn$model
orsf_lrn$importance()

p = orsf_lrn$predict(task, row_ids = 201:228)
p
p$score()

## hyperparams ----
n_tree = to_tune(100, 1500) # default = 500
mtry_ratio = to_tune(0.01, 0.1)
split_min_obs = to_tune(5, 20)
leaf_min_obs = 3 # like ranger

split_min_events = 5
leaf_min_events =	1

n_split =	5	# the number of cut-points assessed when splitting a node in decision trees
n_retry = 3	# try up to 3 times to find a linear combination of predictors to split

control_type = to_tune(p_fct('fast', 'cph', 'net'))

control_fast_do_scale	=	TRUE
control_cph_method = 'efron'
control_cph_eps =	1e-09
control_cph_iter_max = 20
control_net_alpha =	0.5 # alpha in glmnet.  If multiple values of alpha are given, then a penalized model is fit using each alpha value prior to splitting a node. TO TUNE!!!
control_net_df_target =	NULL # Preferred number of variables used in a linear combination.

split_min_stat = 3.841459 # equivalent to p = 0.05
oobag_pred_type =	'surv' # c(none, surv, risk, chf)
importance = 'anova' # c(none, anova, negate, permute) CHOOSE WHICH!!!
oobag_pred_horizon = NULL # Default is the median of the observed times, i.e., oobag_pred_horizon = median(time)
oobag_eval_every = NULL # Default is oobag_eval_every = n_tree
attach_data = TRUE # a copy of the training data will be attached to the output. This is helpful if you plan on using functions like `orsf_pd_oob` or `orsf_summarize_uni` to interpret the forest using its training data. Default is TRUE. MAYBE SET TO FALSE?

# native call ----




# tuning example ----

# fit aorsf model ----
?aorsf::aorsf

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

# SOS: native ranger() returns distr only!!!!
pred1 = predict(fit, data = task$data(rows = 1:5))

head(pred1$survival)
#head(pred1$chf)

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
stopifnot(crank2 == res$crank)

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

