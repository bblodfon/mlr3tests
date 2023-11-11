library(mlr3extralearners)
library(mlr3proba)

task = tsk("rats")
l1 = lrn("surv.parametric")
l2 = lrn("surv.kaplan")
p = l1$train(task)$predict(task)
p2 = l2$train(task)$predict(task)

# AFT test
p$filter(row_ids = c(20,37))
class(p$data$distr)
p$data$distr
p$filter(row_ids = 20)
class(p$data$distr)
p$filter(row_ids = 0) # issue solved
p$filter(row_ids = 20) # issue
class(p$data$distr) # Distribution
p$data$distr

# matrix test
p2$filter(row_ids = c(20,37))
p2$data$distr # 2 rows
p2
p2$filter(row_ids = 20)
p2$data$distr # 1 row
p2$filter(row_ids = 0) # no issue
p2
p2$filter(row_ids = 20) # no issue
p2$filter(row_ids = 20) # can do it again
class(p2$data$distr)
p2
p2$filter(row_ids = 0) # no issue

# Matdist test
p3 = l2$train(task)$predict(task)
p3$data$distr = p3$distr
class(p3$data$distr)
p3$filter(row_ids = c(20,37))
p3$data$distr # 2 rows
p3$filter(row_ids = 20)
p3$data$distr # WeightDisc
class(p3$data$distr)
p3$filter(row_ids = 0) # issue solved
p3
p3$filter(row_ids = 20) # issue
