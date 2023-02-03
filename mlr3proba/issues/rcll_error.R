# https://github.com/mlr-org/mlr3proba/issues/311
library(mlr3verse)
library(mlr3proba)
library(tibble)

set.seed(0)
vet_obs = survival::veteran
vet_obs$status = 1 # all dead

vet_cen = survival::veteran
vet_cen$status = 0 # all censored

task_cen = as_task_surv(x = vet_cen, time = 'time', event = 'status')
task_obs = as_task_surv(x = vet_obs, time = 'time', event = 'status')
poe = po('encode')
task_cen = poe$train(list(task_cen))[[1L]]
task_obs = poe$train(list(task_obs))[[1L]]

learner = lrn('surv.ranger', verbose = FALSE,
  id = 'SurvivalForestLogRank',
  label = 'Random Forest (Logrank splitrule)',
  fallback = lrn('surv.kaplan'),
  num.threads = 4,
  splitrule = 'logrank',
  num.trees = 50,
  mtry.ratio = 0.8,
  min.node.size = 3,
  importance = 'permutation'
)

rcll = msr('surv.rcll')
ibrier_proper = msr('surv.graf', proper = TRUE)
ibrier_improper = msr('surv.graf')
dcal = msr('surv.dcalib', B = 10, chisq = FALSE)
uno_c = msr('surv.cindex', weight_meth = 'G2')
harrell_c = msr('surv.cindex')

grid = benchmark_grid(tasks = list(task_obs), learners = learner,
  resamplings = rsmp('cv', folds = 6))
bm = benchmark(design = grid, store_models = TRUE)
dt = as.data.table(bm)

data_list = list()
index = 1
for(p in dt$prediction) {
  tsk_id = dt$task[[index]]$id
  task = dt$task[[index]]
  #cen_num = sum(p$truth[,2] == 0) # test set censored individuals
  cen_num = sum(task$truth()[,2] == 0)
  train_set = dt$resampling[[index]]$train_set(dt$iteration[index])

  print(p$score(msr('oob_error'), learner = dt$learner[[index]]))

  print(p$score(rcll))
  ibrier_proper_score = p$score(ibrier_proper, task = task, train_set = train_set)
  ibrier_proper_test_score = p$score(ibrier_proper)
  ibrier_improper_score = p$score(ibrier_improper, task = task, train_set = train_set)
  ibrier_improper_test_score = p$score(ibrier_improper)

  dcal_score = p$score(dcal)
  unoc_score = p$score(uno_c, task = task, train_set = train_set)
  harc_score = p$score(harrell_c)

  data_list[[index]] = tibble(tsk_id = tsk_id, cen_num = cen_num,
    ibrier_proper_score = ibrier_proper_score,
    ibrier_proper_test_score = ibrier_proper_test_score,
    ibrier_improper_score = ibrier_improper_score,
    ibrier_improper_test_score = ibrier_improper_test_score,
    dcal_score = dcal_score, unoc_score = unoc_score, harc_score = harc_score)

  index = index + 1
}

res = dplyr::bind_rows(data_list)
res

# So the following two also fail
bm$score(rcll)
bm$score(msr('oob_error')) # when all are censored, this returns NaN
bm$aggregate(rcll)
