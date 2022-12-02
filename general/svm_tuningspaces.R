library(mlr3verse)

task = tsk('pima')

# some configs are very slow on this dataset?
base_svm = mlr3tuningspaces::lts(lrn('classif.svm'))
base_svm$param_set$values$type = 'C-classification' # needs to be explicitly set
base_svm$param_set$values

svm_at = AutoTuner$new(
  learner = base_svm,
  resampling = rsmp('cv', folds = 5),
  measure = msr('classif.ce'),
  terminator = trm('evals', n_evals = 10),
  tuner = tnr('random_search')
)

future::plan('multisession')
svm_at$train(task, row_ids = train_indx)
future::plan('sequential')