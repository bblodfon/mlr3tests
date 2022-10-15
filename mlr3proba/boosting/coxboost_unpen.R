# Setting up unpenalized clinical features in CoxBoost using mlr3
library(mlr3verse)
library(mlr3proba)

# mRNA Task ----
# ~10000 features
tasks = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/tasks.rds')
task_mRNA = tasks$mRNA

# train and test indexes
res = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/results/xgboost/xgboost_cox.rds')
train_indx = res$train_indx
test_indx  = res$test_indx

# Clinical task ----
task_clinical = readRDS(file = '/home/john/repos/pancaim/paad-survival-bench/data/task_clinical.rds')

# Learner ----
coxboost_lrn = lrn('surv.coxboost',
  standardize = FALSE, # data already standardized
  return.score = FALSE, # don't need this in the output
  stepno = 100,
  criterion = 'pscore',
  unpen.index = 1:length(task_clinical$feature_names) # add clinical features as mandatory
)

# but have the clinical features also first, then the mRNA ones
task = task_clinical$clone(deep = TRUE)
task$cbind(task_mRNA$data(cols = task_mRNA$feature_names))
task$ncol == (task_mRNA$ncol + task_clinical$ncol - 2) # don't include targets (time, status) twice
task$truth()

# check that the unpenalized features used were the clinical ones
coxboost_lrn$train(task, train_indx)
coxboost_lrn$model
coxboost_lrn$model$n # 100 patients in train set
summary(coxboost_lrn$model)

# first 10 features (8 clinical, 2 mRNA)
head(coxboost_lrn$model$xnames, 10)
coxboost_lrn$model$unpen.index

coefs = coef(coxboost_lrn$model)
feat_sel = coefs[which(coefs != 0)]
length(feat_sel)
sort(abs(feat_sel), decreasing = TRUE)

# test predictions
coxboost_lrn$predict(task, test_indx)$score()
