#'#########################
# Missing Data Imputation #
#'#########################
library(mlr3verse)
library(mlr3proba)
library(survival)
library(dplyr)

# Lung Dataset ----
# Preprocessing
lung = survival::lung
lung$status = (lung$status == 2L) # 2 is death so convert to 1
lung = lung %>% select(-inst) # remove Institution code (irrelevant for us)
lung$ph.ecog = as.integer(lung$ph.ecog) # there is one missing value, we have to let the imputer know to impute an integer and not any numeric!

task_lung = as_task_surv(x = lung, time = 'time', event = 'status', id = 'lung')
# Inspect missing variables
task_lung$missings()
task_lung$col_info
missing_cols = names(which(task_lung$missings() > 0))
missing_cols # 5 columns have missing values

# Using selectors ----
imp_hist = po("imputehist", affect_columns = selector_intersect(
  selector_missing(), selector_type('numeric')
  )
)
imp_hist$param_set$values$affect_columns(task_lung) # you see which features will be affected
task_lung$col_info

imp_hist$train(list(task_lung))[[1L]]$missings() # ph.ecog is an integer column so it wasn't affected
imp_hist$state$affected_cols
task4 = imp_hist$train(list(task_lung))[[1L]]
task4$missings()

imp_learner = po('imputelearner', lrn('regr.rpart'), # regr => integer or numeric features
  context_columns = selector_all(), # use all features for training
  affect_columns = selector_missing()) # impute only on missing columns
task5 = imp_learner$train(list(task_lung))[[1L]]
task5$missings() # all were imputed

mlr_reflections$task_feature_types

imp_learner2 = po('imputelearner', lrn('regr.rpart')) #, # regr => integer or numeric features
  #affect_columns = selector_intersect(selector_type('factor'), selector_missing()))
task6 = imp_learner2$train(list(task_lung))[[1L]]
task6

# https://mlr-org.com/gallery/2020-01-30-impute-missing-levels/
# New task with missing indicator columns ("dummy columns")
imp_missind = po("missind")
task_ext = imp_missind$train(list(task_lung))[[1]]
tail(task_ext$data())

# New task with imputed values on every column
imp_num = po("imputehist", affect_columns = selector_type("numeric"))
imp_hist = po("imputehist", affect_columns = selector_missing())

new_task1 = imp_num$train(list(task = task_lung))[[1]]
new_task2 = imp_hist$train(list(task = task_lung))[[1]]
new_task1$missings() # ph.ecog was left untouched! YAY
new_task2$missings() # ph.ecog was taken care of! YAY
tail(new_task1$data())
head(new_task2$data())

unique(new_task2$data()$ph.ecog) # ph.ecog okay (0-5 integer score)
unique(new_task2$data()$ph.karno) # some non-standard values, but ok

imp_num$state$model
imp_num$state$affected_cols
imp_hist$state$affected_cols

# GRAPH that includes missing feature columns (DON'T CARE!!!)
graph = po("copy", 2) %>>%
  gunion(list(imp_missind, imp_hist)) %>>%
  po("featureunion")
graph$plot()

# Imputation graph
impute_graph = po("copy", 1) %>>% po("imputehist")
# Test that it works as expected
task_lung2 = impute_graph$train(task_lung)[[1]]
task_lung2$missings()
task_lung2
unique(task_lung2$data()$ph.ecog)

# Add cv_glmnet learner
cv_glmnet_distr_kaplan_aft = ppl(
  "distrcompositor",
  learner = lrn("surv.cv_glmnet", alpha = 1, nfolds = 10, type.measure = "C"),
  estimator = "kaplan",
  form = "aft"
)

# Make learner
learner_graph = impute_graph %>>% cv_glmnet_distr_kaplan_aft %>% as_learner()
learner_graph$graph_model$plot()

cv_glmnet_distr_kaplan_aft$train(task_lung) # does not work due to NA's in training data
learner_graph$train(task_lung, row_ids = 4:task_lung$nrow) # Now works!

pred = learner_graph$predict(task_lung, row_ids = 1:3)
pred$score()

# Penguin Dataset ----

# Imputation example
task = tsk("penguins")
task$missings()
task$col_info

# Add missing indicator columns ("dummy columns") to the Task
# Note that factor column sex is not affected
?mlr_pipeops_missind
pom = po("missind")
# Simply pushes the input forward
nop = po("nop")
# Imputes numerical features by histogram.
pon = po("imputehist", id = "imputer_num")
# combines features (used here to add indicator columns to original data)
pou = po("featureunion")
# Impute categorical features by fitting a Learner ("classif.rpart") for each feature.
pof = po("imputelearner", lrn("classif.rpart"), id = "imputer_fct", affect_columns = selector_type("factor"))

impgraph2 = list(
  pom,
  nop
) %>>% pou
plot(impgraph2)

impgraph = list(
  pom,
  nop
) %>>% pou %>>% pof %>>% pon

impgraph$plot()

# apply it to task
new_task = impgraph$train(task)[[1]]
new_task$missings()

# ImputeLearner ----
task = tsk('penguins')
task$missings()
task$col_info

?mlr_pipeops_imputelearner
imp_lrn = po('imputelearner', lrn('regr.rpart'), # Regression learner => imputes only integer or numeric features
  affect_columns = selector_name('body_mass')) # only impute this specific column

task2 = imp_lrn$train(list(task))[[1]]
task2$missings()
task2$data()[is.na(bill_depth)] # body_mass is filled in
task$data()[is.na(bill_depth)]  # compared to before
imp_lrn$state$affected_cols # which will be the target
imp_lrn$state$context_cols  # which predictors will be used in general
imp_lrn$state$model$body_mass$train_task

imp_lrn = po('imputelearner', lrn('regr.rpart'))
task3 = imp_lrn$train(list(task))[[1]]
task3$missings()
imp_lrn$state$affected_cols
imp_lrn$state$context_cols
imp_lrn$state$model$year # year is included because all `affected_columns` are included (all numeric ones that is)
imp_lrn$state$model$year$model

all(task3$data()[,year] == task$data()[,year])

# impute only columns with missing values
imp_lrn2 = po('imputelearner', lrn('regr.rpart'),
  affect_columns = selector_missing())
task4 = imp_lrn2$train(list(task))[[1]]
task4$missings() # all numeric column filled in!

names(task$missings())[task$missings() != 0] # 4 numeric columns with NAs
imp_lrn2$state$affected_cols
imp_lrn2$state$model$flipper_length

## Examples ----
imp_regr.tree = po('imputelearner', learner = lrn('regr.rpart'), # regr => integer or numeric features
  #context_columns = selector_all(), # use all features for training (default)
  affect_columns = selector_missing()) # impute only on missing columns (that are numeric)

imp_classif.xgboost = po('imputelearner',
  learner = lrn('classif.xgboost', nthread = 6, nrounds = 100, eta = 0.01),
  context_columns = selector_all(), # all features for training the imputed learner
  affect_columns = selector_missing() # only the missing columns are imputed
)

imp_regr.xgboost = po('imputelearner',
  learner = lrn('regr.xgboost', nthread = 12, nrounds = 100, eta = 0.01),
  affect_columns = selector_missing() # only the missing columns are imputed
)
