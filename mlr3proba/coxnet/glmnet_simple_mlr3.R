#################################
# Simple glmnet learner example #
#################################
library(mlr3verse)
library(glmnet)
library(dplyr)
library(survival)

data(CoxExample)
x = CoxExample$x
dim(x) # (1000,30)
colnames(x) = paste0('x', 1:30) # 30 covariates
x[1:5, 1:5]
dim(CoxExample$y) # (1000, 2)

# make a common data frame
cox_example = dplyr::bind_cols(x, CoxExample$y)
head(cox_example)

# Create survival task
task_cox_example = as_task_surv(x = cox_example, time = "time",
  event = "status", id = "CoxExample")
task_cox_example$censtype # right censoring by default

?autoplot.TaskSurv
# kaplan-meier plot
autoplot(task_cox_example)

# create learner and train it
glmnet_lrn = lrn('surv.glmnet')
glmnet_lrn$train(task_cox_example, row_ids = 1:995)
glmnet_lrn$model

# Predict: use default lambda (s=0.01)
lambda_param = glmnet_lrn$param_set %>% as.data.table() %>% as_tibble() %>% filter(id == 's')
lambda_param$default[[1]] # 0.01
lambda_param$tags[[1]] # used for predicting only
pred = glmnet_lrn$predict(task_cox_example, row_ids = 996:1000)
pred$score(measures = msr('surv.cindex'))

# Predict: set your own lambda
my_lambda = 0.07752803
glmnet_lrn$param_set$values = list(s = my_lambda)
glmnet_lrn$param_set$values
glmnet_lrn$param_set$get_values(tags = 'predict')

pred2 = glmnet_lrn$predict(task_cox_example, row_ids = 996:1000)
pred2
pred2$score(measures = msr('surv.cindex')) # better lambda!
pred2$lp # linear predictors
exp(pred2$lp) # risks

# get coefficients for a particular lambda
selected_features = glmnet_lrn$selected_features()
selected_features
coef_tbl = glmnet_lrn$model %>%
  coef(my_lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = 'coef_name') %>%
  dplyr::rename(value = `1`) %>%
  filter(coef_name %in% selected_features) # keep only the non-zeroed features
coef_tbl
# can take the absolute value and plot something like importance

# To Predict survival you need `distrcompositor`
glmnet_lrn2 = ppl(
  "distrcompositor",
  learner = lrn("surv.glmnet"),
  estimator = "kaplan", # 'nelson' or 'kaplan'
  form = "ph"
) %>% as_learner()

# SAME AS:
glmnet_lrn2 = ppl(
  "distrcompositor",
  learner = lrn("surv.glmnet"),
  estimator = "kaplan", # 'nelson' or 'kaplan'
  form = "ph"
) %>% GraphLearner$new()

# train
glmnet_lrn2$graph_model$is_trained # FALSE
glmnet_lrn2$param_set$values = list(surv.glmnet.s = my_lambda) # put awesome lambda
glmnet_lrn2$train(task_cox_example, row_ids = 1:995)
glmnet_lrn2$graph_model$is_trained # TRUE

pred3 = glmnet_lrn2$predict(task_cox_example, row_ids = 996:1000)

pred3$score() # same score as before since it's the same coxnet model basically

# get the glmnet model and features and coefficients
glmnet_lrn2$graph_model$pipeops$surv.glmnet$learner_model$model
glmnet_lrn2$graph_model$pipeops$surv.glmnet$learner_model$model %>% coef(my_lambda)
glmnet_lrn2$graph_model$pipeops$surv.glmnet$learner_model$selected_features()

# Predict survival probabilities of the test data for new time points
times = c(0.1, 1, 10, 15, 20, 70, 120)
pred3$distr$survival(times)
pred3$distr$cumHazard(times)

# Integrated ROC AUC for test set
pred3$score(msr('surv.uno_auc'), task = task_cox_example, train_set = 1:995)
