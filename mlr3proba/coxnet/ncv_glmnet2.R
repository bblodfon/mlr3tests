# Nested CV - tune both (lambda, a) parameters (2nd better version)
library(mlr3verse)
library(mlr3proba)
library(survival)
library(dplyr)
library(ggplot2)

# Task Lung ----
# Preprocessing
lung = survival::lung
lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

task_lung = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
task_lung$missings()

# Simple imputation pipeline for missing values
?mlr3pipelines::mlr_pipeops_imputelearner
impute_po = po('imputelearner', lrn('regr.rpart'))
task_lung = impute_po$train(list(task = task_lung))[[1]]

task_lung$missings()
task_lung

# Nested CV: glmnet ----
## First use CV glmnet to get a lambda range to tune
cvglmnet_lrn = lrn("surv.cv_glmnet", alpha = 1, nfolds = 5, type.measure = "C")
cvglmnet_lrn$train(task_lung, row_ids = 1:114)

lrn('surv.glmnet', alpha = 1, lambda.1se)

cvglmnet_lrn$predict(task_lung, row_ids = 115:228)

lambdas = cvglmnet_lrn$model$lambda
min_lambda = min(lambdas)
if (min_lambda > 0.001) min_lambda = 0.001
max_lambda = max(lambdas)
if (max_lambda < 0.3) max_lambda = 0.3
message('[CV glmnet] Min lambda: ', min_lambda, '\n[CV glmnet] Max lambda: ', max_lambda)

## define learner
glmnet_lrn = lrn('surv.glmnet', # standardize = FALSE,
  alpha = to_tune(0, 1),
  lambda = to_tune(p_dbl(min_lambda, max_lambda, logscale = TRUE)))
print(glmnet_lrn$param_set$values)

rand_search_tuner = tnr('random_search') # set `batch_size` for parallelization!!!
inner_folds = 7
inner_resampling = rsmp('cv', folds = inner_folds)
#inner_resampling = rsmp('repeated_cv', repeats = 10, folds = inner_folds) # even more robust - use for smaller datasets
outer_folds = 4
outer_resampling = rsmp('cv', folds = outer_folds) # outer folds (3-10)
harrell_cindex = msr('surv.cindex')
term_evals = 100 # number of hyperparameter configurations searched

future::plan('multisession', workers = 4) # for faster
glmnet_ncv_res = tune_nested(
  method = rand_search_tuner,
  task = task_lung,
  learner = glmnet_lrn,
  inner_resampling = inner_resampling,
  outer_resampling = outer_resampling,
  measure = harrell_cindex,
  term_evals = term_evals)

# Unbiased performance score ----
glmnet_ncv_res$score() # Harrell's C-index on the outer (unseen) folds
glmnet_ncv_res$aggregate()

autoplot(glmnet_ncv_res) +
  ylim(c(0.5,1)) +
  labs(x = 'glmnet', title = paste0('Generalization Error (NCV - ', inner_folds,
    '/', outer_folds, ' in/out #folds, ', term_evals, ' hyperparam configs'))
ggsave(filename = 'mlr3/mlr3proba/coxnet/ncv_gen_error.png', width = 5, height = 5)

# hpo_res => hyperparameter optimization results
# for every hyperparameter configuration we get their CV error
hpo_res = extract_inner_tuning_archives(glmnet_ncv_res) # can make a surface plot with these

hp_data = hpo_res %>% as_tibble() %>%
  select(alpha, x_domain_lambda, surv.cindex) %>%
  rename(lambda = x_domain_lambda)
print(summary(hp_data))

# Fill in a parameter grid
grid = interp::interp(x = hp_data$alpha, y = hp_data$lambda,
  nx = 100, ny = 100, z = hp_data$surv.cindex)
griddf = subset(data.frame(x = rep(grid$x, nrow(grid$z)),
  y = rep(grid$y, each = ncol(grid$z)),
  z = as.numeric(grid$z)),
  !is.na(z))

# Draw the contour plot
scale_fill_brewer_discretised = metR::as.discretised_scale(scale_fill_distiller)

griddf %>% ggplot(aes(x, y, z = z)) +
  metR::geom_contour_fill(aes(fill = stat(level))) +
  scale_fill_brewer_discretised(name = 'C-index', palette = 'RdYlBu', direction = 1) +
  labs(x = 'alpha', y = 'lambda') +
  geom_point(data = hp_data, aes(x = alpha, y = lambda), inherit.aes = F,
    shape = 4, size = 0.01) +
  theme_classic()

# Final glmnet model ----
# with distr prediction
glmnet_graph_lrn = ppl('distrcompositor',
  glmnet_lrn,
  estimator = 'nelson',
  form = 'aft',
  overwrite = TRUE,
  graph_learner = FALSE
) %>% GraphLearner$new(id = 'glmnet')

set.seed(42)
glmnet_at = AutoTuner$new(
  learner = glmnet_graph_lrn,
  resampling = inner_resampling,
  measure = harrell_cindex,
  terminator = eval_trm,
  tuner = rand_search_tuner)

# Train ----
glmnet_at$train(task_lung)

# Predict ----
pred = glmnet_at$predict(task_lung, row_ids = 1:30)
pred

# Measure performance on 'Test' set
# Harrell's C-index (requires crank prediction - so any models in mlr3proba can have it!)
pred$score(harrel_cindex)

# Uno's C-index (requires crank prediction AND TRAIN DATA)
# Usually a bit less then Harrell's C-index? No it depends
uno_cindex = msr('surv.cindex', weight_meth = 'G2')
pred$score(uno_cindex, task = task_lung, train_set = 1:task_lung$nrow)

# Uno's ROC AUC at specific time points (requires `lp` prediction and train dataset)
uno_auc = msr('surv.uno_auc')

uno_auc$param_set$values = list(integrated = FALSE, times = 100)
pred$score(uno_auc, task = task_lung, train_set = 1:task_lung$nrow)

uno_auc$param_set$values = list(integrated = FALSE, times = 500)
pred$score(uno_auc, task = task_lung, train_set = 1:task_lung$nrow)

uno_auc$param_set$values = list(integrated = FALSE, times = 1000)
pred$score(uno_auc, task = task_lung, train_set = 1:task_lung$nrow)

# Survival probabilities at specific time points
times = c(10,250,500,750,1000)
prob_mat = pred$distr$survival(times)
head(prob_mat)

# Other interesting measures (Graf score, Integrated Log Loss)
# Require distr prediction + can get standard errors as well!
# It is suggested in the documentation to use the `task` and `train_set` indexes
graf_score = msr('surv.graf')
pred$score(graf_score, task = task_lung, train_set = 1:task_lung$nrow)
graf_score$param_set$values$se = TRUE
pred$score(graf_score, task = task_lung, train_set = 1:task_lung$nrow)

intlogloss = msr('surv.intlogloss')
pred$score(intlogloss, task = task_lung, train_set = 1:task_lung$nrow)
intlogloss$param_set$values$se = TRUE
pred$score(intlogloss, task = task_lung, train_set = 1:task_lung$nrow)
