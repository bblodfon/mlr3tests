# Issue with weighted averaging is now fixed!!!!!

library(mlr3verse)
library(mlr3proba)

?mlr_graphs_survaverager
mlr_graphs$keys(pattern = '^surv')

# Prediction Weighted Averaging
task = tsk("rats")

#task = tsk("rats")$filter(sample(300, 5)) # ???
p1 = lrn("surv.kaplan")$train(task)$predict(task)
p2 = lrn("surv.coxph")$train(task)$predict(task)

p1
p2

survaverager = po("survavg", param_vals = list(weights = c(0.2, 0.8)))
survaverager2 = po("survavg") # (0.5, 0.5)

pred = survaverager$predict(list(p1, p2))[[1]]
pred2 = survaverager2$predict(list(p1, p2))[[1]]

# OK: cranks are averaged
all((0.2*p1$crank + 0.8*p2$crank)/1 == pred$crank)
all((p1$crank + p2$crank)/2 == pred2$crank)

# But distr cdfs are not?
res1 = as.numeric(p1$distr$cdf(50)) # 50 here is the timepoint X: F(x) = P(x <= X)
res2 = as.numeric(p2$distr$cdf(50))
comb_res_1 = as.numeric(pred$distr$cdf(50))
comb_res_2 = as.numeric(pred2$distr$cdf(50))

library(testthat)
all(0.2*res1 + 0.8*res2 == comb_res_1)
all((res1 + res2)/2 == comb_res_1)
all((res1 + res2)/2 == comb_res_2)

v1 = distr6::VectorDistribution$new(distribution = "Binomial",
  params = list(
    list(prob = 0.1, size = 1),
    list(prob = 0.6, size = 2)
  )
)

v2 = distr6::VectorDistribution$new(distribution = "Binomial",
  params = list(
    list(prob = 0.1, size = 3),
    list(prob = 0.6, size = 4)
  )
)

mv1 = distr6::mixturiseVector(list(v1, v2))
v1$cdf(1:5)
v2$cdf(1:5)
mv1$cdf(1:5)

mv2 = distr6::mixturiseVector(list(v1, v2), weights = c(0,1))
v2$cdf(1:5)
mv2$cdf(1:5)


library(mlr3verse)
library(mlr3proba)
task = tsk("rats")
p1 = lrn("surv.kaplan")$train(task)$predict(task)
p2 = lrn("surv.nelson")$train(task)$predict(task)
p3 = lrn("surv.coxph")$train(task)$predict(task)

p1
p2
p3

poc = mlr3pipelines::po("survavg")
poc_unequal = mlr3pipelines::po("survavg", param_vals = list(weights = c(0.3, 0.4, 0.3)))

library(testthat)
p = poc$predict(list(p1, p2, p3))$output
expect_equal(p$crank, (p1$crank + p2$crank + as.numeric(p3$crank)) / 3)
expect_equal(as.numeric(p$distr$cdf(100)),
  as.numeric((p1$distr$cdf(100) + p2$distr$cdf(100) + p3$distr$cdf(100)) / 3))

# OK? differences are small?
pp = poc_unequal$predict(list(p1, p2, p3))$output
expect_equal(pp$crank, as.numeric(p1$crank * 0.3 + p2$crank * 0.4 + p3$crank * 0.3))
expect_equal(as.numeric(pp$distr$cdf(100)),
  as.numeric((p1$distr$cdf(100) * 0.3 + p2$distr$cdf(100) * 0.4 + p3$distr$cdf(100) * 0.3)))
