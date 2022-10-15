library(mlr3verse)
library(mlr3fselect)
library(tidyverse)

set.seed(42)
future::plan('multisession')
#future::plan('sequential')

# task ----
task = tsk('pima')
task
task$data()
task$missings()

# FSelectInstanceSingleCrit ----
learner = lrn('classif.rpart')
rsmp_cv = rsmp('cv', folds = 5)
measure = msr('classif.ce')
evals20 = trm('evals', n_evals = 20)

instance = FSelectInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp_cv,
  measure = measure,
  terminator = evals20
)
instance

# FSelectors ----
mlr_fselectors

## Desing Points ----
# UnivariateFS
?mlr_fselectors_design_points # user provides feature sets

# make univariate feature selection matrix design
n_features = length(task$feature_names)
mat = matrix(data = FALSE, n_features, n_features)
for (index in 1:n_features) {
  mat[index, index] = TRUE
}
dt = data.table::data.table(mat)
colnames(dt) = task$feature_names
dt

fselector = fs('design_points', design = dt)
instance$clear()
fselector$optimize(instance)

# check
instance$archive
as.data.table(instance$archive) %>%
  as_tibble() %>%
  dplyr::filter(glucose == TRUE)
instance$result_feature_set # glucose chosen
#instance$result_x_search_space

## same thing done with AutoFSelector +
## you get trained model on the whole dataset with best feature subset
at = AutoFSelector$new(
  learner = learner,
  resampling = rsmp_cv,
  measure = measure,
  terminator = evals20,
  fselector = fs('design_points', design = dt)
)
at$train(task)
at$fselect_result

## Exhaustive Search ----
?mlr_fselectors_exhaustive_search # generate all possible feature sets
#' hp => `max_features` says choose up to `max_features` each time
#' expect 8, as in design points method
fselector = fs('exhaustive_search', max_features = 1)
#' expect 8 + choose(8,2) = 28 => 36 feature subsets
fselector = fs('exhaustive_search', max_features = 2)

instance$clear()
fselector$optimize(instance)

as.data.table(instance$archive) %>%
  as_tibble() %>%
  arrange(classif.ce)
instance$result_feature_set

## Random Search ----
#' random choose feature sets of size UP TO `max_features`
?mlr_fselectors_random_search
fselector = fs('random_search', max_features = 3, batch_size = 3)

at_rand = AutoFSelector$new(
  learner = learner,
  resampling = rsmp_cv,
  measure = measure,
  terminator = trm('evals', n_evals = 10),
  fselector = fs('random_search', max_features = 2)
)
at_rand$train(task)
at_rand$fselect_result
as.data.table(at_rand$archive)

## Sequential Selection ----
?mlr_fselectors_sequential # forward or backward

# NOTE => p features, can try up to p(p+1)/2 models with these strategies,
# a kinda large number for p > 1000

## 8 features
length(task$feature_names)

### forward strategy ----
# start from null model, try (8 + 7 + 6 + 5 + 4 = 30) feature subsets
sfs = fs('sequential', strategy = 'sfs', max_features = 5) # `min_features` = 1 (default)
#' adding `min_features` = 3, will do `choose(8,3)` = 56 3-element subsets first,
#' so it becomes very computational intensive

at_sfs = AutoFSelector$new(
  learner = learner,
  resampling = rsmp_cv,
  measure = measure,
  terminator = trm('none'),
  fselector = sfs
)

at_sfs$train(task)
at_sfs$fselect_result
as.data.table(at_sfs$archive)

sfs$optimization_path(at_sfs$fselect_instance) # first result of each batch

### backward strategy ----
# start from full model, try (1 + 8 + 7 + 6 = 22) feature subsets
sbs = fs('sequential', strategy = 'sbs', min_features = 5)
at_sbs = AutoFSelector$new(
  learner = learner,
  resampling = rsmp_cv,
  measure = measure,
  terminator = trm('none'),
  fselector = sbs
)
at_sbs$train(task)
at_sbs$fselect_result
as.data.table(at_sbs$archive)

sbs$optimization_path(at_sbs$fselect_instance) # first result of each batch

## RFE (Recursive Feature Elimination) ----
?mlr_fselectors_rfe
#' start using all features, get importance scores (so use with RFs, trees, xgboost)
#' from learner, remove features, repeat until `n_features` are left

#' `feature_fraction`/`feature_number`/`subset_sizes` control how many features to remove in each iteration (or keep) and are mutually exclusive
#' See: https://github.com/mlr-org/mlr3fselect/blob/HEAD/R/FSelectorRFE.R#L108
#' `feature_number` comes first, `subset_sizes` second, and last `feature_fraction`

# Calculate how many subsets will be run:
n = 10465 # number of total features
n_features = 2 # run until these number of features
feature_fraction = 0.8
feature_number = 5

# first way with `feature_number`:
seq(from = n - feature_number, to = n_features, by = -feature_number)
# second way (manual subsets):
subset_sizes = c(10000, 9000, 8000, 5000, 500, 100, 20, 6, 4)
# third way:
unique(floor(cumprod(c(n, rep(feature_fraction, log(n_features / n) / log(feature_fraction))))))[-1]

at = AutoFSelector$new(
  learner = learner,
  resampling = rsmp_cv, # rsmp('repeated_cv', repeats = 10, folds = 5),
  measure = measure,
  terminator = trm('none'), # not necessary to set
  #fselector = fs('rfe', n_features = 2, feature_number = 1),
  #fselector = fs('rfe', subset_sizes = c(50, 40, 30, 20, 10, 2)),
  fselector = fs('rfe', feature_fraction = 0.65, n_features = 2),
  store_models = TRUE, # this is needed!
)
at$train(task)
at$fselect_result
as.data.table(at$archive)

at$learner$model

## Genetic Algo Search ----
?mlr_fselectors_genetic_search
?genalg::rbga.bin()

gas = fs('genetic_search')
gas$param_set

### hps
#' https://github.com/mlr-org/mlr3fselect/blob/HEAD/R/FSelectorGeneticSearch.R#L63
#' passes args `size` = #n_features and `evalFunc`, both from `instance` object
#' `iters`, `zeroToOneRatio`, `mutationChance` should be checked on the data
suggestion = list() # optional list of suggested chromosomes (DON'T USE)
popSize = 200 # should be enough?
elitism = NA # default => keep 20% of `popSize` in the next generation to perform crossover
iters = 100000 # number of generations (set your own criteria for stopping)
zeroToOneRatio = 10 # 10 zeros for every 1 one (for mutations and initialization)
mutationChance = NA # [0,1] => by default it will set it to `1/(#features+1)` !!!

at_ga = AutoFSelector$new(
  learner = learner,
  resampling = rsmp_cv,
  measure = measure,
  terminator = trm('evals', n_evals = 20), # < 100000, so stops at 20
  fselector = fs('genetic_search', zeroToOneRatio = 5)
)
at_ga$train(task)
at_ga$fselect_result
as.data.table(at_ga$archive)

autoplot(at_ga$fselect_instance, type = 'performance')

n_features = length(task$feature_names)
dt = at_ga$archive$data
dt2 = dt[, colnames(dt) %in% task$feature_names, with = FALSE]
dt2

sum(duplicated(dt2))/nrow(dt2) # % duplicated subsets proposed by GA
rowSums(dt2)
summary(rowSums(dt2)/n_features) # mean % of total included features in each subset

#' NOTES
#' larger `zeroToOneRatio` => less features are selected
#' larger `mutationChance` (mutation prob) => more search, slower convergence

# check subsets selected
res = genalg::rbga.bin(size = n_features, popSize = 200, iters = 20,
  mutationChance = NA, zeroToOneRatio = 5,
  evalFunc = function(string=c()) {
    returnVal = 1 / sum(string);
    returnVal
  }
)
tail(res$population[,]) # I think one run takes the last row

## Shadow variables ----
# See [Thomas2017]
?mlr_fselectors_shadow_variable_search

at_shadow = AutoFSelector$new(
  learner = learner,
  resampling = rsmp_cv,
  measure = measure,
  terminator = trm('evals', n_evals = 20),
  fselector = fs('shadow_variable_search')
)
at_shadow$train(task)
at_shadow$fselect_result
as.data.table(at_shadow$archive)

# AutoFSelector ----
?mlr3fselect::AutoFSelector

# like AutoTuner
at = AutoFSelector$new(
  learner = lrn('classif.rpart'),
  resampling = rsmp('cv', folds = 5),
  measure = msr('classif.ce'),
  terminator = trm('evals', n_evals = 20),
  fselector = fs('sequential')
)
at
at$fselector

rs = resample(task = task, learner = at, resampling = rsmp('cv', folds = 5),
  store_models = TRUE)

grid = benchmark_grid(
  task = tsk('pima'),
  learner = list(at, lrn('classif.rpart')),
  resampling = rsmp('cv', folds = 5)
)

bmr = benchmark(grid, store_models = TRUE)
res = bmr$score()
res
bmr$aggregate(msrs(c('classif.ce','time_train')))

res$learner[[1]]$fselect_result
extract_inner_fselect_results(bmr)
extract_inner_fselect_archives(bmr)

# Auto tuning + FSelection ----
# TOO MUCH!!!
## An auto-tuning learner
search_space = ps(cp = p_dbl(1e-04, 1, logscale = TRUE))
at = AutoTuner$new(
  learner = lrn('classif.rpart'),
  resampling = rsmp('cv', folds = 3),
  measure = msr('classif.ce'),
  terminator = trm('evals', n_evals = 5),
  tuner = tnr('random_search'),
  search_space = search_space
)

## An auto-feature-selection learner
afs = AutoFSelector$new(
  learner = at,
  resampling = rsmp('cv', folds = 5),
  measure = msr('classif.ce'),
  terminator = trm('evals', n_evals = 20),
  fselector = fs('genetic_search')
)

rr = resample(task = tsk('pima'), learner = afs,
  resampling = rsmp('cv', folds = 3), store_models = TRUE)
rr$score()
extract_inner_fselect_results(rr)
extract_inner_fselect_archives(rr)
