# Nested CV - tune both (lambda, a) parameters
library(mlr3verse)
library(mlr3proba)
library(survival)
library(tictoc)
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
#?mlr3pipelines::mlr_pipeops_imputehist
impute_graph = po("copy", 1) %>>% po("imputehist")
task_lung = impute_graph$train(task_lung)[[1]]
task_lung$missings()
task_lung

# Nested CV: glmnet ----
glmnet_lrn = lrn('surv.glmnet')

# SOS: use `tune_nested`!

# to tune: alpha and lambda
glmnet_lrn$param_set$values = list(
  alpha = to_tune(0.5, 1),
  lambda = to_tune(p_dbl(0.001, 0.1))
)
harrel_cindex = msr('surv.cindex')
num_configs = 50 # change to more configurations if needed
eval_trm = trm("evals", n_evals = num_configs)
rand_search_tuner = tnr("random_search")

inner_folds = 5
inner_resampling = rsmp('cv', folds = inner_folds) # inner folds (5-10)
#resampling = rsmp('repeated_cv', repeats = 5, folds = 10) # even more robust - use for larger datasets

glmnet_tuner = AutoTuner$new(
  learner = glmnet_lrn,
  resampling = inner_resampling,
  measure = harrel_cindex,
  terminator = eval_trm,
  tuner = rand_search_tuner
)

outer_folds = 4
outer_resampling = rsmp("cv", folds = outer_folds) # outer folds (3-10)

set.seed(42)
glmnet_ncv_res = resample(
  task = task_lung,
  learner = glmnet_tuner,
  resampling = outer_resampling,
  store_models = TRUE
)

# Unbiased performance score ----
glmnet_ncv_res$score() # Harrell's C-index
glmnet_ncv_res$aggregate()
autoplot(glmnet_ncv_res) +
  ylim(c(0.5,1)) +
  labs(x = 'glmnet', title = paste0('Generalization Error (NCV - ', inner_folds,
    '/', outer_folds, ' in/out #folds, ', num_configs, ' hyperparam configs'))
ggsave(filename = 'mlr3/mlr3proba/coxnet/ncv_gen_error.png', width = 5, height = 5)

# best hyperparameters and their CV error
# extract_inner_tuning_results(glmnet_ncv_res)
in_tune_res = extract_inner_tuning_archives(glmnet_ncv_res) # can make a surface plot with these

cv_data = in_tune_res %>% as_tibble() %>%
  select(alpha, lambda, surv.cindex)

# Make a hyperparameter contour surface plot
grid = interp::interp(x = cv_data$alpha, y = cv_data$lambda, z = cv_data$surv.cindex)
griddf = subset(data.frame(x = rep(grid$x, nrow(grid$z)),
                           y = rep(grid$y, each = ncol(grid$z)),
                           z = as.numeric(grid$z)),
                           !is.na(z))

griddf %>% ggplot(aes(x, y, z = z)) +
  geom_raster(aes(fill = z), interpolate = FALSE) +
  metR::geom_contour_fill(aes(fill = stat(level))) +
  scale_fill_brewer_discretised(name = 'Measure', palette = 'RdYlBu', direction = 1) +
  labs(x = 'X', y = 'Y') +
  theme_classic()

griddf %>% ggplot(aes(x, y, z = z)) +
  metR::geom_contour_fill(aes(fill = stat(level))) +
  scale_fill_brewer_discretised(name = 'Measure', palette = 'RdYlBu', direction = 1) +
  labs(x = 'X', y = 'Y') +
  # geom_point(size = 0.1) + # doesn't look nice, points are in a grid
  theme_classic()

# Final glmnet model ----
# with distr prediction
glmnet_graph_lrn = ppl('distrcompositor',
  glmnet_lrn,
  estimator = "nelson",
  form = "aft",
  overwrite = TRUE,
  graph_learner = FALSE
) %>% GraphLearner$new(id = 'glmnet')

set.seed(42)
glmnet_at = AutoTuner$new(
  learner = glmnet_graph_lrn,
  resampling = inner_resampling,
  measure = uno_cindex,
  terminator = eval_trm,
  tuner = rand_search_tuner)

# Train ----
glmnet_at$train(task_lung)

# Predict ----
pred = glmnet_at$predict(task_lung, row_ids = 1:30)
pred

# Measure performance on 'Test' set
# Harrell's C-index (requires crank prediction)
pred$score(harrel_cindex)

# Uno's C-index (requires crank prediction and train dataset)
# Usually a bit less then Harrell's C-index?
uno_cindex = msr('surv.cindex', weight_meth = 'G2')
pred$score(uno_cindex, task = task_lung, train_set = 1:task_lung$nrow)

# Uno's ROC AUC at specific time points (requires lp prediction and train dataset)
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
