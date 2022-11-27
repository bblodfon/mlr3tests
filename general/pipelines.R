library(mlr3verse)
library(mlr3pipelines)
library(tidyverse)

# Find PipeOps ----
as.data.table(mlr_pipeops) %>% as_tibble()
as.data.table(mlr_pipeops)[1:3,c('key','input.type.train')] # [rows,cols,group]
as.data.table(mlr_pipeops)[,key]

# PCA + Filter Variance + Learner ----
## Scaling and PCA ----
scale_po   = po("scale") # center = TRUE, scale = TRUE
scale_po$param_set

?mlr_pipeops_pca
pca        = po("pca", center = FALSE)
pca$param_set # center = FALSE, scale = FALSE

task = tsk('iris')
pca_po = po('pca', rank. = 2) # data is only centered before PCA
pca_po$train(list(task))[[1]]$data()[1:3]

# SOS: preprocessing is part of the ML pipeline and the state is there to be
# used with new data and $predict()
pca_po$train(list(task$clone()$filter(2:150)))[[1]]$data()[1:3]
pca_po$state # has kept rotation matrix to use later

# new data
single_line_task = task$clone()$filter(1)
pca_po$predict(list(single_line_task))[[1]]$data()

## Variance filter ----
?mlr_pipeops_filter
filter     = po("filter",
  filter = mlr3filters::flt("variance"),
  filter.frac = 0.5) # keep half of the features
# or: param_vals = list(filter.frac = 0.5)

flt = mlr3filters::flt("variance")
?mlr_filters_variance
flt$calculate(tsk('pima'))
flt$scores

?mlr_pipeops_learner
learner_po = po("learner", learner = lrn("classif.rpart"))

pca = po("pca")
graph = scale_po %>>% pca %>>% filter %>>% learner_po
plot(graph)

glrn = GraphLearner$new(graph)
glrn = as_learner(graph) # same
glrn
glrn$graph$pipeops

task = tsk("iris")
rs = resample(task, glrn, resampling = rsmp("cv"), store_models = TRUE)

## Mutate ----
# for feature engineering (add or change features)
?mlr_pipeops_mutate
mutate_po = po("mutate")

constant = 1
mutate_po$param_set$values$mutation = list(
  Sepal.Length_plus_constant = ~ Sepal.Length + constant,
  Sepal.Area = ~ Sepal.Width * Sepal.Length,
  Petal.Area = ~ Petal.Width * Petal.Length,
  Sepal.Area_plus_Petal.Area = ~ Sepal.Area + Petal.Area
)

mutate_po$train(list(tsk("iris")))[[1]]$data()

# Constructing Graphs ----
## Linear ----
mutate = po("mutate") # create new features
filter = po("filter",
  filter = mlr3filters::flt("variance"),
  param_vals = list(filter.frac = 0.5))

## easier
graph = mutate %>>% filter
graph$plot()

## Manual ----
# and also harder and prone to error
graph = Graph$new()$
  add_pipeop(mutate)$ # add nodes (operations)
  add_pipeop(filter)$
  add_edge('mutate', 'variance') # add edges (operation flow)
graph$plot()

graph$add_pipeop(po('pca')) # adds it in parallel
graph$plot()
graph$add_pipeop(po('pca', id = 'pca2')) # needs new id!!!
graph$plot()
graph$train(task) # nice, get 3 datasets in the end output

## gunion ----
gr = po("copy", outnum = 2) %>>%
  gunion(list(po("scale"), po("pca"))) %>>%
  po("featureunion", innum = 2)
gr$plot(html = FALSE)

# Modeling Example ----
mutate = po("mutate")
filter = po("filter",
  filter = mlr3filters::flt("variance"),
  param_vals = list(filter.frac = 0.5))
# learner as pipeop
learner = po('learner', learner = lrn('classif.rpart'))

graph = mutate %>>% filter %>>% learner
graph$plot()

## Can do train and predict:
task = tsk("iris")
graph$train(task)
graph$predict(task)

## But better to encapsulate the graph object as a learner:
grlrn = as_learner(graph)
grlrn
rs = resample(task, grlrn, rsmp("cv", folds = 3))
rs$score()

# Changing hyperparameters ----
grlrn$param_set
grlrn$param_set$values$variance.filter.frac = 0.25
grlrn$param_set$values$classif.rpart.cp = 0.05
rs2 = resample(task, grlrn, rsmp("cv", folds = 3))
rs2$score()

# Tuning ----
library(paradox)
ps = ps(
  classif.rpart.cp = p_dbl(lower = 0, upper = 0.05),
  variance.filter.frac = p_dbl(lower = 0.25, upper = 1)
)

paradox::generate_design_lhs(ps, n = 10)

library("mlr3tuning")

instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = grlrn,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  search_space = ps,
  terminator = trm("evals", n_evals = 20)
)

tuner = tnr("random_search")
tuner$optimize(instance)
instance
instance$result_learner_param_vals
instance$result_y

## Hyperparameter plots ----
?autoplot.TuningInstanceSingleCrit()
autoplot(instance) # type = marginal
autoplot(instance, type = 'parameter')
autoplot(instance, type = 'performance') # nice overall one
autoplot(instance, type = 'parallel') # parallel coordinates plot, like a Sankey Diagram
autoplot(instance, type = 'points') # YES!
autoplot(instance, type = 'surface') # YES! contour surface

# Branching ----
?mlr_pipeops_branch
branch_po = po('branch', options = c('nop', 'pca', 'scale'))
unbranch_po = po('unbranch', options = c('nop', 'pca', 'scale'))

disconn_graph = gunion(graphs = list(
  po('nop'), # simply push input forward
  po('pca'),
  po('scale')
))
disconn_graph$plot() # disconnected graph!

graph = branch_po %>>% disconn_graph
graph$plot()

graph = branch_po %>>% disconn_graph %>>% unbranch_po
graph$plot()

# note branch.selection choose one path only to go into!
graph$param_set$values$branch.selection

graph$train(task)[[1]] # nop selection (nothing happened to the data)
graph$param_set$values$branch.selection = 'pca'
graph$train(task)[[1]] # pca done!!!

# Chunking ----
## when datasets is too large to fit into memory, split it into chunks,
## train each on the same learner and aggregate predictions on predict()
chks = po("chunk", outnum = 4) # outnum => number of chunks created
lrns = ppl("greplicate", po("learner", lrn("classif.rpart")), 4)
mjv = po("classifavg", 4)
pipeline = chks %>>% lrns %>>% mjv
pipeline$plot(html = FALSE)

task = tsk("iris")
train.idx = sample(seq_len(task$nrow), 120)
test.idx = setdiff(seq_len(task$nrow), train.idx)
intersect(train.idx, test.idx)

pipelrn = as_learner(pipeline)
pipelrn$train(task, train.idx)$
  predict(task, train.idx)$
  score()

# Bagging ----
?mlr_pipeops_subsample
## bootstrap:
boot = po('subsample', param_vals = list(frac = 1, replace = TRUE))
boot$param_set

unique(task$data())
unique(boot$train(list(task))[[1]]$data()) # around 90 rows ~ 60-64%

single_pred = po("subsample", frac = 0.7) %>>%
  po("learner", lrn("classif.rpart"))
single_pred$plot()
?mlr_graphs_greplicate
pred_set = ppl("greplicate", single_pred, 10) # copy in parallel 10 times
pred_set$plot()
bagging = pred_set %>>%
  po("classifavg", innum = 10) # innum is not needed
bagging$plot()

baglrn = as_learner(bagging)
baglrn$train(task, 1:120)
baglrn$predict(task, 121:150)$score()

## Using pipeline bagging ----
?mlr_graphs_bagging
lrn_po = po("learner", lrn("classif.rpart"))
gr = ppl('bagging', lrn_po, 3, averager = po("classifavg", collect_multiplicity = TRUE))
rs = resample(task, GraphLearner$new(gr), rsmp("holdout"))
rs$score()

baggedlrn = as_learner(gr)
baggedlrn$train(task, 1:120)
baggedlrn$predict(task, 121:150)$score()

# Stacking

## learner_cv => do CV on a learner, keep only predictions as features
?mlr_pipeops_learner_cv
task = tsk("iris")
learner = lrn("classif.rpart")

lrncv_po = po("learner_cv", learner$clone())
lrncv_po$param_set
lrncv_po$learner$predict_type = "response"
lrncv_po$train(list(task))[[1]]$data()
lrncv_po$learner$predict_type = "prob"
lrncv_po$train(list(task))[[1]]$data() # keeps the target

nop = mlr_pipeops$get("nop")

graph = gunion(list(
  lrncv_po,
  nop
)) %>>% po("featureunion")

graph$plot()

graph$train(task)
#graph$pipeops$classif.rpart$learner$predict_type = "prob"
graph$train(task)[[1]]$data() # adds CV predictions as features

# Stacking ----
## stacking simple
lrn = lrn("classif.rpart")
lrn_0 = po("learner_cv", lrn$clone())
lrn_0$id = "rpart_cv"
lrn_0

stack = gunion(list(lrn_0, po("nop"))) %>>%
  po("featureunion", 2) %>>%
  po("learner", lrn$clone())
stack$plot()

stacklrn = as_learner(stack)
stacklrn$train(task, 1:120)
stacklrn$predict(task, 121:150)$score()

## multiple stacking (more powerful?)
rprt = lrn("classif.rpart", predict_type = "prob")
glmn = lrn("classif.glmnet", predict_type = "prob")

# Create Learner CV Operators
?mlr_pipeops_learner_cv
lrn_0 = po("learner_cv", rprt, id = "rpart_cv_1")
lrn_0$param_set$values$maxdepth = 5L
lrn_1 = po("pca", id = "pca1") %>>% po("learner_cv", rprt, id = "rpart_cv_2")
lrn_1$param_set$values$rpart_cv_2.maxdepth = 1L
lrn_2 = po("pca", id = "pca2") %>>% po("learner_cv", glmn)

# Union them with a PipeOpNULL to keep original features
level_0 = gunion(list(lrn_0, lrn_1, lrn_2, po("nop", id = "NOP1")))
level_0$plot()

# Cbind the output 3 times, train 2 learners but also keep level
# 0 predictions
level_1 = level_0 %>>%
  po("featureunion", 4) %>>%
  po("copy", 3) %>>% # COPY 3 TIMES THE NEW DATASET
  gunion(list(
    po("learner_cv", rprt, id = "rpart_cv_l1"),
    po("learner_cv", glmn, id = "glmnt_cv_l1"),
    po("nop", id = "NOP_l1")
  ))
level_1$plot()

# Column-bind predictions, train a final learner
level_2 = level_1 %>>%
  po("featureunion", 3, id = "u2") %>>%
  po("learner", rprt, id = "rpart_l2")

# Plot the resulting graph
level_2$plot(html = FALSE)

multistacklrn = as_learner(level_2)
multistacklrn$
  train(task, 1:120)$
  predict(task, 121:150)$
  score()

## Fast 2-level Stacking :)
?mlr_graphs_stacking
base_learners = list(
  lrn("classif.rpart", predict_type = "prob"),
  lrn("classif.kknn", predict_type = "prob")
)
super_learner = lrn("classif.log_reg")

graph_stack = pipeline_stacking(base_learners, super_learner)
graph_stack$plot()
graph_learner = as_learner(graph_stack)
graph_learner

task = tsk("german_credit")
graph_learner$train(task)
graph_learner$predict(task)$score()
