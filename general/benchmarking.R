library(mlr3verse)
suppressMessages(library(dplyr))
library(ggplot2)

# Less logging
lgr::get_logger("bbotk")$set_threshold("warn")
lgr::get_logger("mlr3")$set_threshold("warn")

# Add progress bars
library(progressr)
handlers(global = TRUE)
# show in non-interactive sessions (e.g. via Rscript)
# export R_PROGRESSR_ENABLE=TRUE # works as well!
options(progressr.enable = TRUE)
handlers("progress")

# Initiate the benchmark design in a grid-like manner
design = benchmark_grid(
  tasks = tsks(c("spam", "german_credit", "sonar")),
  learners = lrns(c("classif.ranger", "classif.rpart", "classif.featureless"),
    predict_type = "prob", predict_sets = c("train", "test")),
  resamplings = rsmps("cv", folds = 3)
)

# Execute it
bmr = benchmark(design)

quit(status = 1)

measures = list(
  msr("classif.auc", predict_sets = "train", id = "auc_train"),
  msr("classif.auc", id = "auc_test")
)

measures = list(
  msr("classif.auc"), msr("classif.ce")
)

tab = bmr$aggregate(measures)
print(tab)

# ranger was better in all datasets!
tab %>% as_tibble() %>%
  group_by(task_id) %>%
  arrange(desc(auc_test)) %>%
  group_split()

?autoplot.BenchmarkResult
autoplot(bmr, measure = msr("classif.ce")) + # default
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

autoplot(bmr, measure = msr("classif.auc")) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# ROC and PR for only a task
bmr_small = bmr$clone(deep = TRUE)$filter(task_id = 'german_credit')
autoplot(bmr_small, type = 'roc')
autoplot(bmr_small, type = 'prc')

rr = tab[task_id == "german_credit" & learner_id == "classif.ranger"]$resample_result[[1]]
print(rr)

measure = msr("classif.auc")
rr$aggregate(measure)
perf = rr$score(measure)
perf

# get the CV iteration with worst AUC
index = which.min(perf$classif.auc)
# get the corresponding learner
rr$learners[[index]]

head(rr$resampling$train_set(index))
