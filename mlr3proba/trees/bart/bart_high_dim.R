# Summary: BART can be used with high-dim datasets
library(mlr3proba)
library(mlr3extralearners)
library(dplyr)
library(ggplot2)

# Data ----
# 1000 mRNA preprocessed features from TCGA PAAD study
t = readRDS(file = gzcon(url('https://github.com/bblodfon/paad-survival-bench/blob/main/data/task_mRNA_flt.rds?raw=True')))
d = t$data()
# days to months
d$time = ceiling(d$time/30.44)
task = mlr3proba::as_task_surv(d, time = 'time', event = 'status', id = 'mRNA')
part = partition(task, ratio = 0.8)

# Train + Test ----
# learner = lrn("surv.bart", nskip = 250, ndpost = 100, keepevery = 50, mc.cores = 10)
learner = lrn("surv.bart", nskip = 250, ndpost = 100, keepevery = 50,
               mc.cores = 10, sparse = TRUE, importance = "prob")
learner$train(task, part$train)

imp = learner$importance() # sorted
P = 1000
plot(imp, # col = c(rep(2, 5), rep(1, P-5)),
  main=paste0('N:', length(part$train), ', P:', P, ', thin:', 50),
  ylab='Selection Probability', #ylim=c(0, 0.3),
  pch=1+45*(imp <= 1/P))
lines(c(0, 1000), c(1/P, 1/P))

# selected (Prob < 1/P) => 46, otherwise 1 (selected)
print(table(1+45*(imp <= 1/P))) # 75 features selected

p_test = learner$predict(task, part$test)
p_test$distr
measures = msrs(c("surv.cindex", "surv.rcll", "surv.graf"))
p_test$score(measures)

# MCMC convergence ----
# predictions on the train set
p_train = learner$predict(task, row_ids = part$train)

z_list = list()
index = 1

# choose 10 patients from the train set randomly and make a list
ids = as.list(sample(length(part$train), 10))

z_list = lapply(ids, function(id) {
  post_surv = 1 - t(distr6::gprm(p_train$distr[id], "cdf")[1,,])
  BART::gewekediag(post_surv)$z # get the z-scores
})

# plot the z scores vs time for all patients
dplyr::bind_rows(z_list) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  mutate(name = as.numeric(name)) %>%
  ggplot(aes(x = name, y = value)) +
  geom_point() +
  labs(x = "Time (months)", y = "Z-scores") +
  # add critical values for a = 0.05
  geom_hline(yintercept = 1.96, linetype = 'dashed', color = "red") +
  geom_hline(yintercept = -1.96, linetype = 'dashed', color = "red") +
  theme_bw(base_size = 14)
