#'##################################################
# Benchmark different baseline survival estimators #
#'##################################################

library(mlr3verse)
library(mlr3extralearners)
library(survival)
library(dplyr)
library(ggplot2)

# Lung Dataset ----
# Preprocessing
lung = survival::lung
lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog)

task_lung = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')

# Imputation graph for missing values
impute_graph = po("copy", 1) %>>% po("imputehist")
task_lung = impute_graph$train(task_lung)[[1]]
task_lung$missings()
task_lung

# Learners
# k for inner loop of nested CV = 7
cv_glmnet_lrn = lrn("surv.cv_glmnet", alpha = 1, nfolds = 7, type.measure = "C")

estimators = list(km = 'kaplan', na = 'nelson')
forms = list(aft = 'aft', ph = 'ph', po = 'po')
dc_str = 'distrcompositor'
# https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.html
learner_ids = c(paste0(estimators$km, '_', forms$aft), paste0(estimators$km, '_', forms$ph),
  paste0(estimators$km, '_', forms$po), paste0(estimators$na, '_', forms$aft),
  paste0(estimators$na, '_', forms$ph), paste0(estimators$na, '_', forms$po))

## Kaplan-Meier estimator
cv_glmnet_distr_km_aft = ppl(
  dc_str, learner = cv_glmnet_lrn, estimator = estimators$km, form = forms$aft
) %>% GraphLearner$new(id = learner_ids[1])

cv_glmnet_distr_km_ph = ppl(
  dc_str, learner = cv_glmnet_lrn, estimator = estimators$km, form = forms$ph
) %>% GraphLearner$new(id = learner_ids[2])

cv_glmnet_distr_km_po = ppl(
  dc_str, learner = cv_glmnet_lrn, estimator = estimators$km, form = forms$po
) %>% GraphLearner$new(id = learner_ids[3])

## Nelson-Aalen estimator
cv_glmnet_distr_na_aft = ppl(
  dc_str, learner = cv_glmnet_lrn, estimator = estimators$na, form = forms$aft
) %>% GraphLearner$new(id = learner_ids[4])

cv_glmnet_distr_na_ph = ppl(
  dc_str, learner = cv_glmnet_lrn, estimator = estimators$na, form = forms$ph
) %>% GraphLearner$new(id = learner_ids[5])

cv_glmnet_distr_na_po = ppl(
  dc_str, learner = cv_glmnet_lrn, estimator = estimators$na, form = forms$po
) %>% GraphLearner$new(id = learner_ids[6])

# All learners in a list
learners = list(cv_glmnet_distr_km_aft, cv_glmnet_distr_km_ph, cv_glmnet_distr_km_po,
  cv_glmnet_distr_na_aft, cv_glmnet_distr_na_ph, cv_glmnet_distr_na_po)
names(learners) = learner_ids

# Nested CV ----
# k for outer loop of nested CV = 7
design = benchmark_grid(tasks = task_lung, learners, resamplings = rsmp("cv", folds = 7))
# simple holdout
#design = benchmark_grid(tasks = task_lung, learners, resamplings = rsmp("holdout", ratio = 0.85))

design$resampling[[1]]$train_set(1) %>% length() # 195 for training (6 out of 7 folds)
design$resampling[[1]]$test_set(1) %>% length() # 33 for testing (1 out of 7 folds)

# Do the benchmark! ----
bm = benchmark(design, store_models = TRUE)
bm

# Results ----
autoplot(bm, measure = msr('surv.cindex')) +
  ggpubr::theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(filename = 'mlr3/mlr3proba/coxnet/comp_estimators_coxnet.pdf', width = 7, height = 5)

measures = msrs(c("surv.intlogloss", "surv.cindex"))
agg_res = bm$aggregate(measures) # mean scores default aggregation for a quick view
agg_res

# Get prediction results for each model
pred_res = bm$score(measures = msr("surv.cindex"))
head(pred_res)

aggregated_res = pred_res %>%
  group_by(learner_id) %>%
  summarize(median_cindex = median(surv.cindex),
    avg_cindex = mean(surv.cindex), sd_cindex = sd(surv.cindex)) %>%
  select(learner_id, median_cindex, avg_cindex, sd_cindex)
aggregated_res

# Note: it's the the mean C-index values that are reported by `bm$aggregate()`
all(agg_res$surv.cindex == aggregated_res$avg_cindex)

# Model Selection ----
# Find best model (best median C-index across external CV)
aggregated_res %>% arrange(desc(median_cindex))

best_learner_id = aggregated_res %>%
  slice_max(median_cindex, with_ties = FALSE) %>%
  pull(learner_id)
best_learner_id

# Checks ----

# Best learner results
res_best_learner = pred_res %>%
  as_tibble() %>%
  filter(learner_id == best_learner_id) # 7 outer loop resamplings
res_best_learner

# models fitted
for (learner in res_best_learner$learner) {
  print(learner$graph_model$pipeops$surv.cv_glmnet$learner_model$model)
}

# Pick one CV resampling result
cv1_res_best_learner = res_best_learner %>%
  slice_max(surv.cindex, with_ties = FALSE)
cv1_res_best_learner

train_rows = cv1_res_best_learner$resampling[[1]]$train_set(1) # 1st outer loop NCV train set
test_rows = cv1_res_best_learner$resampling[[1]]$test_set(1) # 1st outer loop NCV test set
intersect(train_rows, test_rows)

# the already trained model in the above CV resampling
cv1_res_best_learner$learner[[1]]$graph_model$pipeops$surv.cv_glmnet$learner_model$model
cv1_res_best_learner$learner[[1]]$graph_model$pipeops$surv.cv_glmnet$learner_model$model$lambda.1se
cv1_res_best_learner$learner[[1]]$graph_model$pipeops$surv.cv_glmnet$learner_model$selected_features()
# coefficients
cv1_res_best_learner$learner[[1]]$graph_model$pipeops$surv.cv_glmnet$learner_model$model %>% coef()

# the predictions in the above test set
predictions = cv1_res_best_learner$prediction[[1]]
predictions
predictions$score() == cv1_res_best_learner$surv.cindex
# the above score is great, because
# performance (C-index) on test set if higher than the train set
# (average C-index across CV folds)

# same test rows (of course)
all(predictions$row_ids == test_rows)

# CONFIRM: I get the SAME predictions on the outer test set
# cv.glmnet automatically uses `lambda.1se`
cv1_res_best_learner$learner[[1]]$graph_model$pipeops$surv.cv_glmnet$is_trained
new_predictions = cv1_res_best_learner$learner[[1]]$predict(task_lung, row_ids = test_rows)
all(new_predictions$crank == predictions$crank)
all(new_predictions$lp == predictions$lp)
all(new_predictions$distr$survival(10) == predictions$distr$survival(10))

# make you own same best learner and check if you get the same results
# Sadly NOT THE SAME results due to not being able to set.seed() during CV!
my_learner = learners[[best_learner_id]]$clone(deep = TRUE)
my_learner$graph_model$pipeops$surv.cv_glmnet$is_trained # FALSE

# train
my_learner$train(task_lung, row_ids = train_rows)
my_learner$graph_model$pipeops$surv.cv_glmnet$is_trained # TRUE

my_learner$graph_model$pipeops$surv.cv_glmnet$learner_model$model
my_learner$graph_model$pipeops$surv.cv_glmnet$learner_model$model$lambda.1se

# predictions
pred = my_learner$predict(task_lung, row_ids = test_rows)
pred$lp != predictions$lp # different due to different seed values

# Get final model ----

# focus on best learner from nested CV
best_learner_id
best_learner = learners[[best_learner_id]]$clone(deep = TRUE)
best_learner

# Train best learner on the WHOLE DATASET (using glmnet's CV for optimal lambda
# on the full dataset)
?cv.glmnet # see `glmnet.fit`
?predict.cv.glmnet
best_learner$train(task_lung)
# results of CV (CV-error)
best_learner$graph_model$pipeops$surv.cv_glmnet$learner_model$model
# best lambda
best_learner$graph_model$pipeops$surv.cv_glmnet$learner_model$model$lambda.1se
# fitted model on the WHOLE DATASET
best_learner$graph_model$pipeops$surv.cv_glmnet$learner_model$model$glmnet.fit
best_learner$graph_model$pipeops$surv.cv_glmnet$learner_model$model %>% coef()
best_learner$graph_model$pipeops$surv.cv_glmnet$learner_model$selected_features()
# ok we are good

pred1 = best_learner$predict(task_lung, row_ids = 42)
pred1$score() # 0.5 (no other individuals)
times = c(1,10,100,1000,10000,100000) # does this differ between the different estimators?
pred1$distr$survival(times)
1 - pred1$distr$getParameterValue('cdf') # for plotting survival curves

# Plot survival curves (42th individual) ----
res = list()
for (learner in learners) {
  my_lrn = learner$clone(deep = TRUE)

  # fit every learner to the whole dataset and let cv_glmnet find the best lambda!
  my_lrn$train(task_lung)

  res[[my_lrn$id]] = my_lrn$predict(task_lung, row_ids = 42)
}

tbl = sapply(res, function(pred) {
  # survival probabilities on the training set time points
  1 - pred$distr$getParameterValue('cdf')
})

tbl = tbl %>%
  as_tibble() %>%
  tibble::add_column(times = task_lung$unique_times(), .before = 1)

tbl %>%
  tidyr::pivot_longer(cols = starts_with('kaplan') | starts_with('nelson'), names_to = 'estimator', values_to = 'surv_prob') %>%
  ggplot(aes(x = times, y = surv_prob)) +
  geom_line(aes(color = estimator, linetype = estimator)) +
  labs(title = 'cv.glmnet (lung dataset - 42nd individual)', x = 'Days',
    y = 'Survival Probability') +
  ggpubr::theme_classic2()
ggsave(filename = 'mlr3/mlr3proba/coxnet/surv_prob_estimators.pdf', width = 7, height = 5)

# Compare all learners ----
res = list()
for (learner in learners) {
  my_lrn = learner$clone(deep = TRUE)

  # fit every learner to the whole dataset and let cv_glmnet find the best lambda!
  my_lrn$train(task_lung)

  # store predictions on the whole dataset as well
  res[[my_lrn$id]] = my_lrn$predict(task_lung)
}

for (r in res) {
  print(r$score())
}

# all around the same
rr = sapply(res, function(r) r$score(measures = msr('surv.intlogloss')))
rr
