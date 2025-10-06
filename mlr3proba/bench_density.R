library(mlr3)
library(mlr3proba) # v0.8.3
library(mlr3viz)

tasks = tsks(c("precip", "faithful"))
dens_keys = mlr_learners$keys("^dens")
learners = lrns(.keys = dens_keys)
lapply(learners, function(.x) {
  .x$encapsulate(method = "evaluate", fallback = default_fallback(.x))
  })

bm_grid = benchmark_grid(
  tasks = tasks,
  learners = learners,
  resamplings = rsmp("repeated_cv", folds = 5, repeats = 5)
)

bmr = benchmark(design = bm_grid, store_models = TRUE)

bmr1 = bmr$clone(deep = TRUE)$filter(task_ids = "precip")
autoplot(bmr1)
tab1 = bmr1$aggregate()
tab1[order(dens.logloss)]

bmr2 = bmr$clone(deep = TRUE)$filter(task_ids = "faithful")
autoplot(bmr2)
tab2 = bmr1$aggregate()
tab2[order(dens.logloss)]
