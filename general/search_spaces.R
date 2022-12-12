library(mlr3verse)
library(paradox)
library(data.table)

# Examples ----
my_ps = ps(
  a = p_int(lower = -1, upper = 1),
  b = p_dbl(lower = 0, upper = 10),
)
my_ps2 = ps(
  a = p_int(lower = 0, upper = 10),
  b = p_fct(levels = LETTERS[1:10])
)

res = paradox::generate_design_grid(my_ps, resolution = 5)
res
# resolution for categorical parameters is ignored!
res2 = paradox::generate_design_grid(my_ps2, resolution = 5)
res2
res3 = paradox::generate_design_random(my_ps2, n = 10)
res3

my_ps3 = ps(a = p_dbl(0,1))
paradox::generate_design_grid(param_set = my_ps3, resolution = 1)
paradox::generate_design_grid(param_set = my_ps3, resolution = 2)
paradox::generate_design_grid(param_set = my_ps3, resolution = 5)
paradox::generate_design_grid(param_set = my_ps3, resolution = 10)

res = paradox::generate_design_grid(param_set = my_ps3, resolution = 15)
res
# check equidistance/linear scale
# SOS: resolution/grid means equal distances/linear interpolation by default
diff(res$data$a)

param_set = ps(
  a = p_dbl(1, 10),
  b = p_dbl(1, 10, trafo = log),
  c = p_dbl(1, 10, trafo = exp),
  d = p_dbl(log(1), log(10), trafo = exp),
  e = p_dbl(1, 10, logscale = TRUE) # e == d
  #f = p_uty(p_dbl(1, 10)) # doesn't work
)

summary(generate_design_random(param_set, 10000)$data) # no transformation
summary(rbindlist(generate_design_random(param_set, 10000)$transpose()))

# Make a parameter that is a simple function of another, e.g. p1 = 2*p2
pam = ParamSet$new(
  params = list(
    ParamInt$new(id = "z", lower = -3, upper = 3),
    ParamDbl$new(id = "x", lower = 0, upper = 1)
  )
)

pam = ps(#z = p_int(-3,3), x = p_dbl(0,1),
  .extra_trafo = function(x, param_set) {
    x$z = 2*(x$x) # overwrite z
    x
  })

bind_rows(generate_design_random(pam, 5)$transpose())

# glmnet lambda uty - Try to get lambda in a log scale in glmnet
tuning_space = lts("classif.glmnet.rbv2")
learner = tuning_space$get_learner()
learner

learner$param_set$values
learner$param_set$values$lambda

# doesn't work
learner$param_set$values$lambda = to_tune(0.001, 0.3, logscale = TRUE)

# works!!!
learner$param_set$values$lambda = to_tune(p_dbl(0.001, 0.3, logscale = TRUE))
learner$param_set$values$lambda = to_tune(p_dbl(log(0.001), log(0.3), trafo = exp))

# untransformed
summary(generate_design_random(learner$param_set$search_space(), 1000)$data)
# transformed
summary(rbindlist(generate_design_random(learner$param_set$search_space(), 1000)$transpose()))

impute_graph = po("imputehist")
task_pima = impute_graph$train(list(tsk("pima")))[[1]]
task_pima$missings()
task_pima

instance = tune(
  method = "random_search",
  task = task_pima,
  learner = learner,
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 100)

as.data.table(instance$archive) %>% as_tibble() %>%
  select(alpha, x_domain_alpha, s, x_domain_s, lambda, x_domain_lambda) %>% summary()
# careful choose the domain_x for the transformed values!

#'###############
# SVM tuning ----
#'###############
search_space = ps(
  cost = p_dbl(lower = 0.1, upper = 10),
  kernel = p_fct(levels = c("polynomial", "radial"))
)

library("data.table")
generate_design_grid(search_space, 3)

# Make parameter vector ----
search_space = ps(
  class.weights = p_dbl(0.1, 0.9,
    trafo = function(x) c(spam = x, nonspam = 1 - x))
)
generate_design_grid(search_space, 3)$transpose()

# Specific tuning factor values ----
search_space = ps(
  cost = p_fct(c(0.1, 3, 10)),
  kernel = p_fct(c("polynomial", "radial"))
)
rbindlist(generate_design_grid(search_space, 3)$transpose())
typeof(search_space$params$cost$levels) # character

# Parameter Dependencies
search_space = ps(
  cost = p_dbl(-1, 1, trafo = function(x) 10^x),
  kernel = p_fct(c("polynomial", "radial")),
  degree = p_int(1, 3, depends = kernel == "polynomial") # degree depends on kernel
)
generate_design_grid(search_space, 3)
data.table::rbindlist(generate_design_grid(search_space, 3)$transpose(), fill = TRUE) # needs both

# to_tune a Learner ----
learner = lrn("classif.svm")
learner$param_set # has already defined hyperparameters
learner$param_set$deps # and dependencies!

learner$param_set$values$kernel = "polynomial" # for example
learner$param_set$values$degree = to_tune(lower = 1, upper = 3)
learner$param_set$values$shrinking = to_tune() # logical, already bounded

learner$param_set$search_space()
generate_design_grid(learner$param_set$search_space(), 3)$data

learner$param_set$values$type = "C-classification" # needs to be set because of a bug in paradox
learner$param_set$values$cost = to_tune(c(val1 = 0.3, val2 = 0.7)) # specific values for tuning
learner$param_set$values$shrinking = to_tune(p_lgl(depends = cost == "val2")) # only on `val2`

learner$param_set$search_space()
generate_design_grid(learner$param_set$search_space(), 3)$data # not ok
rbindlist(generate_design_grid(learner$param_set$search_space(), 3)$transpose(), fill = TRUE) # ok

learner$param_set$values$cost = NULL
learner$param_set$values$shrinking = NULL
learner$param_set$values$kernel = to_tune(c("polynomial", "radial"))

learner$param_set$search_space()
rbindlist(generate_design_grid(learner$param_set$search_space(), 3)$transpose(), fill = TRUE)

# unmet dependency => set to other!
library(mlr3extralearners)
library(paradox)
learner = lrn('surv.ranger',
  splitrule = to_tune(c('logrank', 'extratrees', 'C', 'maxstat')),
  num.random.splits = to_tune(p_int(1, 100, depends = splitrule == 'extratrees'))) # only for `extratrees`, but not 1 otherwise!

generate_design_random(learner$param_set$search_space(), 5)

library(paradox)

my_ps = paradox::ps(
  minsplit = p_int(1, 64, logscale = TRUE), # logscale = TRUE
  cp = p_dbl(1e-04, 1)
)

my_ps
my_ps$set_id = "john"
my_ps
my_psc = ParamSetCollection$new(list(my_ps))
my_psc
