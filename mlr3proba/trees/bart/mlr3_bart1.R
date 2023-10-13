library(mlr3extralearners)
library(mlr3pipelines)
library(mlr3proba)
library(tidyverse)
library(testthat)
library(tictoc)
# library(dplyr)
# library(tidyr)
# library(ggplot2)

set.seed(42)
task_lung = tsk('lung')

# convert time from days to weeks to ease computational burden
d = task_lung$data()
d$time = ceiling(d$time/7)
task_lung = as_task_surv(d, time = 'time', event = 'status', id = 'lung')
task_lung$label = "Lung Cancer"
task_lung

# BART supports factors but due to outputting importance scores per each dummy level
# we leave it to the user to encode them as they please
poe = po('encode', method = 'treatment')
task_lung = poe$train(list(task_lung))[[1]]
task_lung
task_lung$missings() # has missing values

# encode + impute
pre = poe %>>% po('imputelearner', lrn('regr.rpart'))
task = pre$train(task_lung)[[1]]
task$missings() # no missing values

# K coarsens the `times` to quantiles
learner = lrn("surv.bart", nskip = 250, ndpost = 100, # K = 50,
  keepevery = 10, mc.cores = 15) # default values

# split to train and test sets
set.seed(42)
part = partition(task)

# Train
learner$train(task, row_ids = part$train)
learner$timings["train"]

# Importance: average number of times a feature has been used in the trees
learner$importance()

# Test (75 patients)
p = learner$predict(task, row_ids = part$test)
p$score() # C-index

#' Mean survival probabilities for the first 6 patients at given time points
#' `p` has the mean survival probabilities
p$distr$survival(times = c(1,100,300,600))[,1:6]

# number of posterior draws
M = learner$model$model$ndpost
M # some extra!
# number of test observations
N = length(part$test)
# number of unique time points in the train set
K = learner$model$model$K
K
stopifnot(K == length(task$unique_times(rows = part$train))) # not the same if parameter K has been used upon learner initialization
# the total actual times are also available in the `$model$model` slot:
times = learner$model$model$times
length(times) # CAN BE LESS
# length(learner$model$times) # don't use this one, it's the non-unique times!

# Full posterior prediction matrix
surv.test = learner$model$surv.test # maybe store in the state?
stopifnot(all(dim(surv.test) == c(M, K * N)))
N * K * M

# Mean posterior prediction matrix
cdf = distr6::gprm(p$distr, "cdf")
cdf2 = t(p$distr$cdf(times = times))
expect_equal(cdf, cdf2)

mean_post_surv = 1 - cdf
expect_equal(mean_post_surv, 1 - cdf2) # check!

# Example data
if (FALSE) {
  m = 4  # Number of draws
  n = 2  # Number of patients
  k = 3  # Number of times
  mat = matrix(1:(m * n * k), nrow = m, ncol = n * k)
  arr = array(mat, dim = c(m, k, n))
  arr
  res = aperm(arr, c(3, 2, 1))
  res
}

# res = list(surv.test = surv.test, M = M, N = N, K = K)
# saveRDS(res, file = 'res.rds')
# res = readRDS(file = "res.rds")

#' Store full posterior `surv.test` matrix in an `distr6::Arrdist` object
?distr6::Arrdist

# correct, but makes a list and takes much longer
# res = apply(surv.test, 1, function(row) {
#   matrix(row, nrow = N, ncol = K, byrow = TRUE, dimnames = list(NULL, times))
# }, simplify = FALSE)

surv.array = aperm(array(surv.test, dim = c(M, K, N), dimnames = list(NULL, times, NULL)), c(3, 2, 1))
dim(surv.array) # 75 (pat) x 134 (times) x 1005 (post_draws)
# 1 - surv.array # cdf

tic()
darr = distr6::as.Distribution(1 - surv.array, fun = "cdf",
  decorators = c("CoreStatistics", "ExoticStatistics"))
toc()
distr6::gprm(darr, "which.curve")
tic()
darr2 = distr6::Arrdist$new(cdf = 1 - surv.array, which.curve = "mean",
  decorators = c("CoreStatistics", "ExoticStatistics"))
toc()
distr6::gprm(darr2, "which.curve")

# bottleneck => takes a lot of time! for large K*N*M
cdf = 1 - aperm(array(surv.test, dim = c(M, K, N),
  dimnames = list(NULL, times, NULL)), c(3, 2, 1))
tic() # this one, the conversion to pdf
pdf = aperm(apply(cdf, c(1, 3), function(.y) c(.y[1], diff(.y))), c(2, 1, 3))
toc()

mat_dist = darr[,"mean"]
# mat_dist = darr[,1]
expect_equal(mat_dist$survival(times), 1 - p$distr$cdf(times = times))
distr6::sprm(darr, lst = list(which.curve = "mean"))
expect_equal(darr$survival(times), 1 - p$distr$cdf(times = times))
expect_equal(darr2$survival(times), 1 - p$distr$cdf(times = times))

# REST OF ANALYSIS FOR MLR3 GALLERY POST

# Choose two patients with very different time of deaths
p$truth[1:4,1] # 1st => early death, 4nd => late death
p$truth[1:4,2]

# Get the posterior survival function estimates for the 1st and 4th test patient
# for all time points (from the train set) - see Sparapani (2021), pages 34-35
# We reformat the matrix to a tibble containing for every time point, the median
# survival estimate as well as the lower and upper bounds of the 95% quantile
# credible interval

# `surv.test` => full prediction matrix
# times => the time points of the train set
get_survCI = function(surv.test, times, patient_id) {
  K = length(times)
  indices = ((patient_id - 1)*K + 1):(patient_id*K)

  surv.test[, indices] %>%
    as.data.frame() %>%
    `colnames<-` (times) %>%
    summarise(across(everything(), list(
      median   = ~ median(.),
      low_qi   = ~ quantile(., 0.025),
      high_qi  = ~ quantile(., 0.975)
    ))) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("times", ".value"),
      names_pattern = "(^[^_]+)_(.*)" # everything until the first underscore
    ) %>%
    mutate(times = as.numeric(times)) %>%
    tibble::add_column(id := as.factor(!!patient_id))
}

surv_data = bind_rows(
  get_survCI(surv.test, times, patient_id = 1),
  get_survCI(surv.test, times, patient_id = 4)
)

my_cols = c("#E41A1C", "#377EB8")

# Draw a survival curve for the first patient in the test set with
# uncertainty quantified
surv_data %>%
  ggplot(aes(x = times, y = median, fill = id)) +
  geom_step() +
  xlab('Time (Days)') +
  ylab('Survival Probability') +
  geom_ribbon(aes(ymin = low_qi, ymax = high_qi), alpha = 0.3) +
  geom_vline(xintercept = p$truth[[1]], linetype = 'dashed', color = my_cols[1]) +
  geom_vline(xintercept = p$truth[[4]], linetype = 'dashed', color = my_cols[2]) +
  theme_bw() +
  scale_fill_manual(values = my_cols) +
  guides(fill = guide_legend(title = "Patient ID"))
