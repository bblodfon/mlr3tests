library(prioritylasso)
n = 50;p = 300
nzc = trunc(p/10) # number of zero coefficients
x = matrix(rnorm(n*p), n, p)
beta = rnorm(nzc)
fx = x[, seq(nzc)]%*%beta/3
hx = exp(fx)
# survival times:
ty = rexp(n,hx)
# censoring indicator:
tcens = rbinom(n = n, prob = 0.3, size = 1)
library(survival)
y = Surv(ty, 1-tcens)
blocks = list(a=1:20, b=21:200, c=201:300) # p in 1-300
# run prioritylasso:
model = prioritylasso::prioritylasso(
  X = x, Y = y,
  family = "cox", type.measure = "deviance",
  blocks = blocks,
  block1.penalization = TRUE, lambda.type = "lambda.min", standardize = TRUE,
  #mcontrol = missing.control(handle.missingdata = "impute.offset"), # for `impute.offset` to work
  nfolds = 5
)
model$blocks

# generate all block orderings
block_permutations = lapply(combinat::permn(names(blocks)), function(x) blocks[x])

model2 = prioritylasso::cvm_prioritylasso(
  X = x, Y = y,
  family = "cox", type.measure = "deviance",
  blocks = block_permutations, alpha = 0,
  block1.penalization = TRUE, lambda.type = "lambda.min", standardize = TRUE,
  nfolds = 5
)
model2$best.blocks.indices # get best indices
model2$best.model # YAY - USE BEST MODEL WHICH HAS BEEN ALREADY FITTED, PREDICTION WORKS
predict(model2$best.model)

## Adaptive STEP
Otherwise, you the two-step fast procedure to find the priority of blocks - detailed here:
# https://github.com/YingxiaLi2023/multi-omics-data/blob/main/Functions/Functions_AnalysisCluster_2.R#L595
and here:
# https://github.com/BoevaLab/Multi-omics-noise-resistance/blob/95608421728e0a166bd889990b8b354c66ef557e/noise_resistance/R/utils/utils.R#L81




model = prioritylasso::prioritylasso(
  X = x, Y = y,
  family = "cox", type.measure = "deviance",
  blocks = model2$best.blocks.indices,
  block1.penalization = TRUE, lambda.type = "lambda.min", standardize = TRUE,
  nfolds = 5
)
#model
testthat::expect_equal(model$blocks, model2$best.blocks.indices)

newdata = x[1:3,]
newdata[1:2, 1:20] = NA
newdata[sample(length(newdata), size = 10)] = NA # not supported for single values

#' `handle.missingtestdata` => how it works? Works only with blockwise missing values!
#' - "none" => stops/erros
#' - "omit.prediction" => NA for these observations
#' - "set.zero" => sets 0 for variable of observation, `lp`'s are returned normally then
#' - "impute.block" => works only if model was fitted with `handle.missingdata = "impute.offset"`
predict(model, newdata = newdata, type = "link", handle.missingtestdata = "impute.block")

# `lp_train`
as.numeric(predict(model))

# 20 new samples
newdata = matrix(rnorm(20*p), 20, p)
dim(newdata)
lp_test = as.numeric(predict(model, newdata = newdata))
lp_test

# newdata
as.numeric(predict(model, type = "link")) # lp

library(mlr3proba)

# with mlr3extralearners
task = as_task_surv(x = data.frame(cbind(x, y)), event = "status")
test_task = as_task_surv(x = data.frame(cbind(newdata, y[1:3])), event = "status")
part = partition(task, ratio = 0.85)
l = lrn("surv.priority_lasso", blocks = blocks, block1.penalization = TRUE,
        lambda.type = "lambda.min", standardize = TRUE, nfolds = 5)
l$train(task, row_ids = part$train)
l$model$train_status
sum(l$model$coefficients != 0)
l$selected_features()

p = l$predict(task, row_ids = part$test)
p
