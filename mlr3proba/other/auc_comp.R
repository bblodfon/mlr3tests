library(timeROC)
library(riskRegression)
library(survAUC)
library(aorsf)
library(survival)
library(ranger)

times = c(1000, 2000, 3000)

# Train vs test
set.seed(42)
train_rows = sample(nrow(pbc_orsf), 0.8 * nrow(pbc_orsf))

train_data = pbc_orsf[train_rows,]
test_data  = pbc_orsf[-train_rows,]

# fit
forest = ranger::ranger(Surv(time, status) ~ ., data = train_data)
# test data predictions
test_risk = predictRisk(forest, newdata = test_data, times = times)
dim(test_risk)
1 - test_risk # survival
# train data predictions
train_risk = predictRisk(forest, newdata = train_data, times = times)
dim(train_risk)
1 - train_risk

# AUC: test set
Score(
  object = list(test_risk),
  formula = Surv(time, status) ~ 1,
  data = test_data, # data is just used to find (time, status)
  times = times,
  metrics = 'auc'
)$AUC$score

# AUC: train set
Score(
  object = list(train_risk),
  formula = Surv(time, status) ~ 1,
  data = train_data, # data is just used to find (time, status)
  times = times,
  metrics = 'auc'
)$AUC$score

# Only train
forest = ranger::ranger(Surv(time, status) ~ ., data = pbc_orsf)
res = predictRisk(forest, newdata = pbc_orsf, times = times)
dim(res) # patients/obs x times
nrow(pbc_orsf)

cstat_riskregression_rsf = Score(
  object = list(res),
  formula = Surv(time, status) ~ 1,
  data = pbc_orsf[,1:3], # data is just used to find (time, status)
  times = times,
  metrics = 'auc'
)

cstat_timeroc <- timeROC(
  T = pbc_orsf$time,
  delta = pbc_orsf$status,
  marker = pbc_orsf$bili,
  cause = 1,
  weighting = "marginal",
  times = times,
  iid = TRUE
)

cstat_riskregression <- Score(
  object = list(pbc_orsf$bili),
  formula = Surv(time, status) ~ 1,
  data = pbc_orsf,
  times = times,
  metrics = 'auc'
)

cstat_survAUC <- survAUC::AUC.uno(
  Surv.rsp = Surv(pbc_orsf$time, pbc_orsf$status),
  Surv.rsp.new = Surv(pbc_orsf$time, pbc_orsf$status), # test = train data here
  times = times,
  lpnew = pbc_orsf$bili
)

cstat_timeroc$AUC - cstat_riskregression$AUC$score$AUC
# SAME RESULTS! USE riskRegression!
#> t=1000 t=2000 t=3000
#>      0      0      0
cstat_survAUC$auc - cstat_timeroc$AUC
# slightly different


