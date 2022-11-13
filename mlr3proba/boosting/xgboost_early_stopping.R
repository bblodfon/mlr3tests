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

split = partition(task_lung, ratio = 0.8)
task_lung$set_row_roles(split$test, "test")
learner = lrn("surv.xgboost", eta = 0.01,
              nrounds = 1000, verbose = 1,
              early_stopping_rounds = 100,
              early_stopping_set = "test"
)
task_lung
learner$train(task_lung, row_ids = 1:200) # row_ids ?

learner2 = lrn("surv.xgboost", eta = 0.01, objective = 'survival:aft',
    nrounds = 1000, verbose = 1,
    early_stopping_rounds = 100,
    early_stopping_set = "test"
)

learner2$train(task_lung)

  # from code directly ----
task_vet # 109, some in test roles!

task = task_vet$clone(deep = TRUE)
data = task$data(cols = task$feature_names)
dim(data) # 109
target = task$data(cols = task$target_names)
dim(target) # 109

targets = task$target_names
targets # time, status
label = target[[targets[1]]]
label # times
status = target[[targets[2]]]
status

## cox objective ----
label[status != 1] = -1L * label[status != 1]
data = xgboost::xgb.DMatrix(
  data = as_numeric_matrix(data),
  label = label)
data # ok for the 'use' data

test_data = task$data(rows = task$row_roles$test, cols = task$feature_names)
test_data
dim(test_data) # 28 rows

test_target = task$data(rows = task$row_roles$test, cols = task$target_names)
test_target

test_label = test_target[[task$target_names[1]]] # time
test_status = test_target[[task$target_names[2]]]
test_label
test_status

test_label[test_status != 1] = -1L * test_label[test_status != 1]
test_data = xgboost::xgb.DMatrix(
  data = as_numeric_matrix(test_data),
  label = test_label)
test_data

