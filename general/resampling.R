library(mlr3verse)
library(tibble)

task = tsk("penguins")
learner = lrn("classif.rpart")

# Resamplings Overview ----
as.data.table(mlr_resamplings)

# Holdout Example ----
rsmp_hold = rsmp("holdout")
# mlr_resamplings$get('holdout')
rsmp_hold # Instantiated: FALSE (not yet applied to a dataset/task)

# change train/test ratio to 0.8
rsmp_hold$param_set$values = list(ratio = 0.8)
rsmp_hold$param_set$values
rsmp_hold

## Apply resampling to task ----
set.seed(42)
rsmp_hold$instantiate(task) # SOS!!!
rsmp_hold # Instantiated: TRUE
length(rsmp_hold$instance$train) # train indices
head(rsmp_hold$train_set(i = 1))
length(rsmp_hold$instance$test)  # test indices (69)
head(rsmp_hold$test_set(i = 1))

# check ratio that is 80:20
length(rsmp_hold$instance$train)/rsmp_hold$task_nrow
length(rsmp_hold$instance$test)/rsmp_hold$task_nrow

rr = resample(task, learner, rsmp_hold, store_models = TRUE)
rr

rr$learners[[1]]$model
rr$learners[[1]]$importance()
rr$predictions()[[1]] # on 69 penguis :)

measures = msrs(c('classif.acc', 'classif.bacc'))
rr$aggregate(measures)
rr$score(measures) # same, 1 sampling iteration

# CV Example ----
learner = lrn('classif.rpart', maxdepth = 3, predict_type = 'prob')
rsmp_cv = rsmp('cv', folds = 5)
rsmp_cv$param_set$values
rsmp_cv$instantiate(task)

for (fold in 1:5) {
  train_ratio = round(100*length(rsmp_cv$train_set(fold))/rsmp_cv$task_nrow, digits = 2)
  test_ratio  = round(100*length(rsmp_cv$test_set(fold))/rsmp_cv$task_nrow, digits = 2)
  print(paste0('Fold ', fold, ', %Train: ', train_ratio, ', %Test: ', test_ratio))
}

rs = resample(task, learner, rsmp_cv, store_models = TRUE) # this does the training work!
autoplot(rs) # the 5 CV errors in a boxplot
?autoplot.ResampleResult()

measures = msrs(c('classif.acc', 'classif.ce', 'classif.bacc'))
res = rs$score(measures)
res

rs$aggregate(measures) # the means of the previous last 3 columns
mean(res$classif.acc)
mean(res$classif.ce)
mean(res$classif.bacc)

sd(res$classif.acc)
sd(res$classif.ce)
sd(res$classif.bacc)

# useful checks
rs$warnings
rs$errors
rs$rsmp_cv

# retrieve a model - there are 5 of them, as many as the folds!
# only available if `store_model` = TRUE
model = rs$learners[[1]]$model
model

# extract predictions for all resampling iterations
rs$prediction()       # all predictions merged into a single Prediction object
rs$predictions()[[1]] # predictions of first resampling iteration

# keep only specified resampling iterations
# (for some reason you wanna throw the others out!)
rs$filter(c(1, 3))
rs
rs$aggregate(measures)
rs$score(measures)

# Repeated CV Example ----
# Let's do 5x mode than above simple CV with 5 folds:
repeated_cv_rsmp = rsmp('repeated_cv', repeats = 5, folds = 5)
repeated_cv_rsmp$instantiate(task)

learner = lrn('classif.rpart', maxdepth = 3, predict_type = 'prob')
rs_repeated_cv = resample(task, learner, repeated_cv_rsmp, store_models = TRUE)
res_repeated_cv = rs_repeated_cv$score(measures)
autoplot(rs_repeated_cv, measure = msr('classif.bacc'))

# Bootstrap Example ----
# We will go for 25 bootstraps as above with repeated CV
set.seed(42)
bootstrap = rsmp("bootstrap", repeats = 25, ratio = 1)
# ratio controls the size of the training set in each resampling
# SOS => `ratio` < 1 results in more test samples
bootstrap$instantiate(task)

# smart way to encode the training samples in each bootstrap resampling (count Matrix)
head(bootstrap$instance$M)
colSums(bootstrap$instance$M) # smaller
bootstrap$task_nrow

# Checks:
# 1) Test and Train are DISJOINT
# 2) Their sum always amounts to the total samples of the dataset
for (boot_iter in 1:25) {
  stopifnot(!intersect(bootstrap$train_set(boot_iter), bootstrap$test_set(boot_iter)))
  stopifnot(
    length(c(unique(bootstrap$train_set(boot_iter)), bootstrap$test_set(boot_iter))) == bootstrap$task_nrow
  )
}

# Bootstrap rule of 64% (percentage of elements in training data on average)
l = list()
for (boot_iter in 1:25) {
  l[[boot_iter]] = length(unique(bootstrap$train_set(boot_iter)))/bootstrap$task_nrow
}
print(paste0('Mean: ', round(mean(unlist(l)), 2), ', sd: ', round(sd(unlist(l)), 2)))

rs_boot = resample(task, learner, bootstrap, store_models = TRUE)
autoplot(rs_boot, measure = msr('classif.bacc'))
res_boot = rs_boot$score(measures)
res_boot

# Insampling (ALL TRAIN, ALL TEST!) ----
insample = rsmp("insample")
insample$instantiate(task)
insample

# ALL SAMPLES ARE USED FOR TRAINING AND PREDICTION!
setequal(insample$train_set(1), insample$test_set(1))
rs_insample = resample(task, learner, insample, store_models = TRUE)
res = rs_insample$score(measures)
res

# Stratified CV Example ----
# SOS: FIRST NOT STRATIFIED:
task_gc = tsk("german_credit")
cv3 = rsmp('cv', folds = 8)
cv3$instantiate(task_gc)

dt = merge(cv3$instance, task_gc$data()[, row_id := .I], by = "row_id")
# per CV-fold ratio "bad:good" credit risk
dt[, .(class_ratio = sum(credit_risk == "bad") / sum(credit_risk == "good")), by = fold]
# original ratio "bad:good" credit risk
dt[, .(class_ratio = sum(credit_risk == "bad") / sum(credit_risk == "good"))]

# NOW: STRATIFY
# SOS => stratum has to be a feature or target variable that is DISCRETE
task_gc$set_col_roles(cols = "credit_risk", add_to = "stratum")
task_gc
cv4 = rsmp('cv', folds = 8)
cv4$instantiate(task_gc)

dt = merge(cv4$instance, task_gc$data()[, row_id := .I], by = "row_id")
dt[, .(class_ratio = sum(credit_risk == "bad") / sum(credit_risk == "good")), by = fold]
dt[, .(class_ratio = sum(credit_risk == "bad") / sum(credit_risk == "good"))]
# STRATIFED: YES!!!

rr = resample(task_gc, learner, cv4, store_models = TRUE)

?autoplot.ResampleResult()
autoplot(rr, measure = msr("classif.auc"))
autoplot(rr, measure = msr("classif.acc"), type = 'histogram')
autoplot(rr, type = "roc")
autoplot(rr, type = "prc")
autoplot(rr, type = "prediction") # needs task with only two features :)

# Stratify on survival status ----
task = tsk('lung')
task

## Original task censoring ----
#' censoring distr = proportion of 0's
status = task$truth()[,2]
status %>%
  as_tibble() %>%
  summarise(censored = sum(value == 0)/n())
# 27.6%

#' @description Get censoring distribution per data split (e.g. fold)
#' @param rsmp [mlr3::Resampling]
#' @param status original task's `status` indicator variable
cens_distr = function(rsmp, status) {
  if (rsmp$id == 'cv') {
    as_tibble(rsmp$instance) %>%
      add_column(status = status[rsmp$instance$row_id]) %>%
      group_by(fold) %>%
      summarise(censored = sum(status == 0)/n())
  } else if (rsmp$id == 'repeated_cv') {
    rep_list = as_tibble(rsmp$instance) %>%
      group_by(rep) %>%
      group_split()

    lapply(rep_list, function(tbl) {
      tbl %>%
        add_column(status = status[tbl$row_id]) %>%
        group_by(fold) %>%
        summarise(censored = sum(status == 0)/n())
    })
  } else if (rsmp$id == 'holdout') {
    inst = dplyr::bind_rows(
      tibble(row_id = rsmp$instance$train, fold = 'train'),
      tibble(row_id = rsmp$instance$test, fold = 'test')
    )
    inst %>%
      add_column(status = status[inst$row_id]) %>%
      group_by(fold) %>%
      summarise(censored = sum(status == 0)/n())
  }
}

## Holdout censoring per fold (train/test) ----
# holdout resampling
rsmp_holdout = rsmp('holdout', ratio = 0.8)
rsmp_holdout$instantiate(task)
rsmp_holdout

cens_distr(rsmp_holdout, status)

## CV censoring per fold ----
# CV resampling
rsmp_cv = rsmp('cv', folds = 10)
rsmp_cv$instantiate(task)
rsmp_cv

cens_distr(rsmp_cv, status)

## Repeated-CV censoring per fold ----
# Repeated-CV resampling
rsmp_rcv = rsmp('repeated_cv', folds = 7, repeats = 3)
rsmp_rcv$instantiate(task)
rsmp_rcv
cens_distr(rsmp_rcv, status)
# view all folds and repeats in one:
dplyr::bind_rows(cens_distr(rsmp_rcv, status), .id = 'rep') %>% as.data.frame()

## Status Stratification and checks ----
task2 = task$clone()
task2$col_roles$stratum = 'status'
task2$strata
# 63/228 = 0.276 # censored (low, but what to do!)

# check if folds are stratified by status
rsmp_holdout$instantiate(task2)
cens_distr(rsmp_holdout, status) # YAY!

rsmp_cv$instantiate(task2)
cens_distr(rsmp_cv, status) # YAY!

rsmp_rcv = rsmp('repeated_cv', folds = 7, repeats = 4)
rsmp_rcv$instantiate(task2)
get_cens_distr(rsmp_rcv, status) # YAY!
dplyr::bind_rows(cens_distr(rsmp_rcv, status), .id = 'rep') %>% as.data.frame()

## mlr3::partition test ----
part = partition(task, ratio = 0.8, stratify = F) # task is not stratified (doesn't work)
#' With `stratify = T` it does stratification on status:
#' From the `mlr3` code => https://github.com/mlr-org/mlr3/blob/HEAD/R/partition.R
#' it get dispatched to => https://github.com/mlr-org/mlr3proba/blob/main/R/partition.R
part = partition(task2, ratio = 0.8, stratify = F) #' `task2` is pre-stratified so it's okay
part = partition(task, ratio = 0.8, stratify = T)
# the above also works, but I don't know how the task is stratified

#' Proof that it stratifies per `status` with `stratify = TRUE`
taskl = tsk('lung')
taskv = as_task_surv(x = survival::veteran, id = 'veteran',
  time = 'time', event = 'status')

task = taskl$clone() # change task
status = task$truth()[,2]
ratio = 0.9 # play with this
times = 500
stratify = TRUE # play with this
res = list()
for (i in 1:times) {
  part = partition(task, ratio = ratio, stratify = stratify)

  res[[i]] = tibble::tibble(
    train = unname(prop.table(table(task$truth(rows = part$train)[,2]))[1]),
    test  = unname(prop.table(table(task$truth(rows = part$test )[,2]))[1])
  )
}
#' censoring distribution stats (on train and test set)
#' the test set is (usually) smaller so more liable to get 'unstratified' results
#' `test.sd` will be larger than 0 for `stratified = FALSE` (proof)
#' `test.mean` will be different than the 'original' cens. distr:

# original censoring distr:
unname(prop.table(table(status))[1])

# resampling results:
dplyr::bind_rows(res) %>%
  summarise(across(everything(), list(mean = mean, sd = sd),
    .names = "{.col}.{.fn}"))

#' `time` doesn't seem to be stratified, I checked with cutting into bins:
prop.table(table(cut(x = sort(task$truth()[,1]),
  breaks = c(0,35,112,1000))))
prop.table(table(cut(x = sort(task$truth(rows = part$train)[,1]),
  breaks = c(0,35,112,1000))))
prop.table(table(cut(x = sort(task$truth(rows = part$test)[,1]),
  breaks = c(0,35,112,1000))))

# Stratify by status + sex ----
sex = task$data(cols = 'sex')[['sex']]
sex
prop.table(table(sex))
## 40% females, 60% males (almost balanced, will serve as an example though)

## Holdout censoring and sex stratification check ----
rsmp_holdout$instantiate(task)

inst = dplyr::bind_rows(
  tibble(row_id = rsmp_holdout$instance$train, fold = 'train'),
  tibble(row_id = rsmp_holdout$instance$test,  fold = 'test')
)
inst %>%
  add_column(status = status[inst$row_id]) %>%
  add_column(sex = sex[inst$row_id]) %>%
  group_by(fold) %>%
  summarise(censored = sum(status == 0)/n(), males = sum(sex == 'm')/n())
# Mix of things

task2 = task$clone()
task2$col_roles$stratum = c('status', 'sex')
task2$strata # good to check this!!! => 4 subpopulations
rsmp_holdout$instantiate(task2)

inst = dplyr::bind_rows(
  tibble(row_id = rsmp_holdout$instance$train, fold = 'train'),
  tibble(row_id = rsmp_holdout$instance$test,  fold = 'test')
)
inst %>%
  add_column(status = status[inst$row_id]) %>%
  add_column(sex = sex[inst$row_id]) %>%
  group_by(fold) %>%
  summarise(censored = sum(status == 0)/n(), males = sum(sex == 'm')/n()) # YAY

## CV censoring and sex stratification check ----
rsmp_cv = rsmp('cv', folds = 15)
rsmp_cv$instantiate(task)
as_tibble(rsmp_cv$instance) %>%
  add_column(status = status[rsmp_cv$instance$row_id]) %>%
  add_column(sex = sex[rsmp_cv$instance$row_id]) %>%
  group_by(fold) %>%
  summarise(censored = sum(status == 0)/n(), males = sum(sex == 'm')/n()) # MIX

rsmp_cv$instance %>%
  as_tibble() %>%
  count(fold) # balanced number of folds

rsmp_cv$instantiate(task2)
as_tibble(rsmp_cv$instance) %>%
  add_column(status = status[rsmp_cv$instance$row_id]) %>%
  add_column(sex = sex[rsmp_cv$instance$row_id]) %>%
  group_by(fold) %>%
  summarise(censored = sum(status == 0)/n(), males = sum(sex == 'm')/n())
#' Better, but not perfect due to sub-populations in `$strata` being unbalanced
#' and the number of folds being too large (e.g. >5)
#' (SOS) => The larger the number of folds, the more difference between
#' stratified and un-stratified CV

rsmp_cv$instance %>%
  as_tibble() %>%
  count(fold) # number of folds differs a bit (as per doc)
