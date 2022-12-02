library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)

# test classification ----
learner = mlr3::lrn("classif.xgboost")
learner$help()

# Train learner with early stopping on spam data set
task = tsk("spam")
task # 4601
length(task$row_roles$use) # 4601
length(task$row_roles$test) # 0

# Split task into training and test set
split = partition(task, ratio = 0.8)
task$set_row_roles(split$test, "test")
task # 3680!
length(task$row_roles$use) # 3680 => roles are set exclusively (you can't have a row with multiple roles!!!)
length(task$row_roles$test)

data = task$data(cols = task$feature_names)
dim(data)

# Set early stopping parameter
learner = lrn("classif.xgboost", verbose = 1, nrounds = 100,
              early_stopping_rounds = 25,
              early_stopping_set = "test")

# Train learner with early stopping
learner$train(task, row_ids = 1:4000)

# Veteran example ----
vet_task = as_task_surv(x = survival::veteran, time = 'time', event = 'status')
poe = po('encode')
vet_task = poe$train(list(vet_task))[[1]]
task_vet = vet_task$clone(deep = TRUE)
task_vet # 137
length(task_vet$row_roles$test) # 0

split2 = partition(task_vet, ratio = 0.8)
task_vet$set_row_roles(split2$test, "test")
task_vet$row_roles # some rows are now test!
task_vet # 109
length(task_vet$row_roles$test)
surv_xgboost = lrn('surv.xgboost', nrounds = 1000, verbose = 1,
              early_stopping_rounds = 10)
surv_xgboost$help()
surv_xgboost$train(task_vet)

surv_xgboost = lrn('surv.xgboost', nrounds = 1000, verbose = 1,
                   early_stopping_rounds = 100,  eta = 0.01,
                   early_stopping_set = 'test')
surv_xgboost$train(task_vet)

# Lung example ----
task_lung = tsk('lung')
task_lung = mlr3pipelines::po('encode')$train(list(task_lung))[[1]]
task_lung

split = partition(task_lung, ratio = 0.8)
task_lung$set_row_roles(split$test, "test")

## make another watchlist based on part of the (train/use) data!
task = task_lung$clone(deep = TRUE)
set.seed(42)
row_ids = sample(1:task$nrow, 25)
row_ids

data = task$data(rows = row_ids, cols = task$feature_names)
target = task$data(rows = row_ids, cols = task$target_names)
data
target

targets = task$target_names
targets
label = target[[targets[1]]] # time
status = target[[targets[2]]] # status
label
status

# objective:cox ----
label[status != 1] = -1L * label[status != 1]
data = xgboost::xgb.DMatrix(
  data = as.matrix(data),
  label = label)
data

# TO REMEMBER ----
#' 3 ways to set a watchlist validation set
#' 1) Set `watchlist` directly to a test set (very manual)
#' 2) Set `early_stopping_set = 'test'` (row_cols$test will be used, easiest)
#' 3) Worse `early_stopping_set = 'train'` (check train data)
learner = lrn("surv.xgboost", eta = 0.01, nrounds = 1000, verbose = 1,
  early_stopping_rounds = 100,
  early_stopping_set = "test", # set this to check train and test set => no need for watchlist then
  #early_stopping_set = "train", # set this one for old behavior (check only train set)
  watchlist = list(sub_train = data)) # `sub_train` or whichever you want to set here!
learner$train(task_lung) # row_ids ?

# objective:aft ----
data = task$data(rows = row_ids, cols = task$feature_names)
target = task$data(rows = row_ids, cols = task$target_names)
targets = task$target_names
label = target[[targets[1]]] # time
status = target[[targets[2]]] # status

y_lower_bound = y_upper_bound = label
y_upper_bound[status == 0] = Inf

data2 = xgboost::xgb.DMatrix(as.matrix(data))
xgboost::setinfo(data2, "label_lower_bound", y_lower_bound)
xgboost::setinfo(data2, "label_upper_bound", y_upper_bound)

learner2 = lrn("surv.xgboost", eta = 0.01, objective = 'survival:aft',
  nrounds = 1000, verbose = 1, early_stopping_rounds = 100,
  early_stopping_set = "none", watchlist = list(sub_train = data2))

learner2$train(task_lung)
# See Issue ----
# https://github.com/mlr-org/mlr3learners/issues/257
