library(SurvMetrics)
#library(mlr3verse)
library(mlr3proba)

# Generate Datasets ----
set.seed(42)

## PH satisifed ----
d1 = SurvMetrics::SDGM1()
t1 = as_task_surv(x = d1, time = 'time', event = 'status', id = 'd1')
t1

km = lrn('surv.kaplan')
cox = lrn('surv.coxph')

p1 = km$train(t1, row_ids = 1:180)$predict(t1, row_ids = 181:200)
p2 = cox$train(t1, row_ids = 1:180)$predict(t1, row_ids = 181:200)

p1$score()
p2$score()
rcll = msr('surv.rcll')
ibrier = msr('surv.graf', proper = TRUE)
p1$score(rcll)
p2$score(rcll)
p1$score(ibrier)
p2$score(ibrier) # CoxPH does better (RCLL, IBS)

## PH severely violated ----
d2 = SurvMetrics::SDGM3()
t2 = as_task_surv(x = d2, time = 'time', event = 'status', id = 'd2')
t2

p1 = km$train(t2, row_ids = 1:180)$predict(t1, row_ids = 181:200)
p2 = cox$train(t2, row_ids = 1:180)$predict(t1, row_ids = 181:200)

p1$score()
p2$score() # look at that c-index!
p1$score(rcll)
p2$score(rcll) # CoxPH rcll worse
p1$score(ibrier)
p2$score(ibrier) # Brier not so
