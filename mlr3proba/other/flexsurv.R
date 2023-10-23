library(mlr3verse)
#> Loading required package: mlr3
library(mlr3extralearners)

l = lrn("surv.flexible")

task_lung = tsk('lung')
preprocess = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
t = preprocess$train(task_lung)[[1]]
split = partition(t)

# works
set.seed(0)
l$train(t, row_ids = split$train)
# fails
set.seed(42)
l$train(t, row_ids = split$train)

## 1) using extralearners method
pred = l$predict(t, row_ids = split$test)


## 2) using flexsurv method
# get survival probabilities
p = predict(l$model, type = "surv", newdata = t$data(split$test), times = )$.pred

# check all times the same
times = p[[1]]$.time
x = all(unlist(lapply(p, function(.x) all.equal(times, .x$.time)))) # TRUE
ut = unique(times)

if (x) {
  # remove survival probabilities at duplicated time points 
  dup = !duplicated(times)
  surv = t(vapply(
    p, function(.x) .x$.pred_survival[dup],
    numeric(length(ut))
  ))
  colnames(surv) = ut
  
  # create Matdist
  out = distr6::as.Distribution(1 - surv, fun = "cdf", decorators = "ExoticStatistics")
}

## 3) compare
matplot(as.data.frame(round(pred$distr$survival(1:100), 4)), type = "l")
matplot(as.data.frame(round(out$survival(1:100), 4)), type = "l")

