library(mlr3extralearners)
library(mlr3pipelines)
library(mlr3proba)
library(distr6)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

task_lung = tsk('lung')

# convert time from days to weeks to ease computational burden
d = task_lung$data()
#d$time = ceiling(d$time/7) # to weeks
d$time = ceiling(d$time/30.44) # to months
task_lung = as_task_surv(d, time = 'time', event = 'status', id = 'lung')
task_lung$label = "Lung Cancer"

# BART supports factors but due to outputting importance scores per each dummy level
# we leave it to the user to encode them as they please
poe = po('encode', method = 'treatment')
task = poe$train(list(task_lung))[[1]]
task
task$missings() # BART supports missing values

set.seed(42)
part = partition(task, ratio = 0.9)

# basic example, test 1 dimension in surv.array
#learner = lrn("surv.bart", nskip = 1, ndpost = 1, # K = 50,
#  keepevery = 1, mc.cores = 1)
# more posterior draws
learner = lrn("surv.bart", nskip = 250, ndpost = 50, # K = 50,
  keepevery = 10, mc.cores = 10, which.curve = 50)
learner$train(task, row_ids = part$train)
p = learner$predict(task, row_ids = part$test)
p

# changing `which.curve` only affects crank calculation:
# change to last posterior:
# learner = lrn("surv.bart", nskip = 250, ndpost = 50, # K = 50,
#  keepevery = 10, mc.cores = 10, which.curve = 50) # which.curve = "mean"
# learner$train(task, row_ids = part$train)
# p = learner$predict(task, row_ids = part$test)
# p
# p$score() is much worse in that case!

p$distr # Arrdist
distr6::gprm(p$distr, "which.curve") # 0.5 => median (always)

surv_array = 1 - distr6::gprm(p$distr, "cdf")
dim(surv_array) # 3d survival array!!!

# same array data, ie conversion works from cdf to survival (1 - cdf)
testthat::expect_equal(p$data$distr, surv_array)

# explain array data: 3rd patient in the test set, 12 month (1 year), all posterior survival probabilities?
p$data$distr[3, 12, ]

# Test metrics ----
# metrics that require a survival matrix `distr` or `crank` (like a risk)
measures = mlr3::msrs(as.data.table(mlr_measures)[
  predict_type %in% c('distr', 'crank') & startsWith(key, 'surv')
]$key)

for (measure in measures) {
  print(p$score(measure, task = task, train_set = part$train, learner = learner))
}

# Uno's C-index
# p$score(msr('surv.cindex', weight_meth = 'G2'), task = task, train_set = part$train)

# Test metrics one low number of samples ----
# Test 1 observation
p2 = p$clone()$filter(part$test[3])
p3 = p$clone()$filter(part$test[1:2])
p2$distr # Arrdist
p3$distr # Arrdist

for (measure in measures) {
  print(p2$score(measure))
  print(p3$score(measure))
}

# Resampling ----
# works no problem
task$col_roles$stratum = 'status' # stratify by status
task$strata
rr = resample(task, learner, resampling = rsmp("cv", folds = 5), store_backends = TRUE)
rr$errors
rr$warnings

# combine predictions
pred = rr$prediction()
pred
pred$score(measures)

scores = rr$score(measures = measures)
scores

rr$aggregate(measures = measures)

# VARIOUS CHECKS ----
# Stored objects
surv.test = learner$model$surv.test
surv.test.mean = learner$model$surv.test.mean

# number of posterior draws
M = learner$model$model$ndpost
M # some extra!
# number of test observations
N = length(part$test)
N
# number of unique time points in the train set
K = learner$model$model$K
K
stopifnot(K == length(task$unique_times(rows = part$train))) # not the same if parameter K has been used upon learner initialization
# the total actual times are also available in the `$model$model` slot:
times = learner$model$model$times
times
length(times) # same as K

# Assertions
# Full posterior prediction matrix from package
# check that it is the same used inside the prediction object in the `p$data$distr`
stopifnot(all(dim(surv.test) == c(M, K * N)))
N * K * M # total size of 3d array

surv.array = aperm(array(surv.test, dim = c(M, K, N), dimnames = list(NULL, times, NULL)), c(3, 2, 1))
dim(surv.array)
surv.array2 = 1 - distr6::gprm(p$distr, 'cdf') # CDF array prediction from learner
dim(surv.array2)

expect_equal(surv.array, surv.array2) # YES
expect_equal(surv.array, p$data$distr) # YES

# Mean posterior prediction matrix
surv.mat = matrix(surv.test.mean, nrow = N, ncol = K, byrow = TRUE)
colnames(surv.mat) = times
head(surv.mat)
dim(surv.mat)

# Extracting `Matdist`
matd_mean = p$distr[,"mean"] # Matdist
matd_mean
matd_median = p$distr[, 0.5]   # Matdist
matd_median

# mean
surv.mat2 = t(matd_mean$survival(times))
dim(surv.mat2)
# median from matrix
surv.mat3 = t(matd_median$survival(times))
dim(surv.mat3)
# median from array
surv.mat4 = t(p$distr$survival(times))
dim(surv.mat4)

expect_equal(surv.mat, surv.mat2) # YES, mean surv
expect_equal(surv.mat, surv.mat3) # NO, mean vs median (YES, if one posterior draw!!!)
expect_equal(surv.mat3, surv.mat4) # YES, median surv

# Uncertainty Quantification in Survival Prediction ----
# choose 2 patients with the worst and the best survival time in the test set
death_times = p$truth[,1]
sort(death_times)

worst_indx = which(death_times == min(death_times))[1] # died first
best_indx  = which(death_times == max(death_times))[1] # died last

patient_ids = c(worst_indx, best_indx)
patient_ids # which patient IDs
death_times = death_times[patient_ids]
death_times # 1st is worst, 2nd is best

# subset Arrdist
arrd = p$distr[patient_ids]
arrd

# We use the `$distr` interface and the `$survival` property to
# get survival probabilities and quantile CIs

# choose time interval (months)
months = seq(1, 36) # 1 month - 3 years
median_qci = arrd$survival(months)

colnames(median_qci) = paste0(patient_ids, "_median")
median_qci = as_tibble(median_qci) %>% add_column(month = months)
head(median_qci)
# verify: 1st is worst, 2nd is best

# just for checking
# matd_median = arrd[,0.5] # median
# matd_median$survival(months) # same as with arrd
# matd_mean = arrd[,"mean"] # mean (if needed)
# matd_mean$survival(months)

#' The ubsetting function `[.Arrdist` using a quantile number (0-1) extracts a
#' `Matdist` based on the cdf
#' Survival is 1 - cdf, so low and upper bounds are reversed
low_qci  = arrd[, 0.975]$survival(months) # 2.5% bound
high_qci = arrd[, 0.025]$survival(months) # 97.5% bound
colnames(low_qci)  = paste0(patient_ids, "_low")
colnames(high_qci) = paste0(patient_ids, "_high")
low_qci  = as_tibble(low_qci)
high_qci = as_tibble(high_qci)

surv_tbl =
  bind_cols(low_qci, median_qci, high_qci) %>%
  pivot_longer(cols = !month, values_to = "surv",
    names_to = c("patient_id", ".value"), names_sep = "_") %>%
  relocate(patient_id)
surv_tbl # with survival probabilities and CI bounds

my_colors = c("#E41A1C", "#4DAF4A")
names(my_colors) = patient_ids

# Draw a survival curve for the first patient in the test set with
# uncertainty quantified
surv_tbl %>%
  #mutate(patient_id = as.factor(patient_id)) %>%
  ggplot(aes(x = month, y = median, fill = patient_id)) +
  geom_step() +
  xlab('Time (Months)') +
  ylab('Survival Probability') +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.3) +
  geom_vline(xintercept = death_times[1], linetype = 'dashed', color = my_colors[1]) +
  geom_vline(xintercept = death_times[2], linetype = 'dashed', color = my_colors[2]) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = my_colors) +
  guides(fill = guide_legend(title = "Patient ID"))
