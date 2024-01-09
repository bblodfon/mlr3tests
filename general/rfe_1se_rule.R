library(mlr3)
library(mlr3fselect)

# instance ----
instance = fselect(
  fselector = fs("rfe", feature_number = 1, n_features = 1),
  task = tsk("pima"),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("cv", folds = 3),
  measures = msr("classif.ce"),
  term_evals = 10,
  callbacks = clbk("mlr3fselect.one_se_rule")
)

instance$archive
instance$result
instance$result$importance[[1]]

# AutoFSelector ----
at = AutoFSelector$new(
  fselector = fs("rfe", feature_number = 1, n_features = 1),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("cv", folds = 3),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 10),
  callbacks = clbk("mlr3fselect.one_se_rule")
)

at$train(tsk("penguins"))
as.data.table(at$archive)[,.(classif.ce, n_features)][order(classif.ce)]
at$fselect_result
at$fselect_result$importance[[1]]
ce = as.data.table(at$archive)[,.(classif.ce)][[1]]
se = sd(ce) / sqrt(length(ce))
se

# auto_fselector ----
at = auto_fselector(
  fselector = fs("rfe", feature_number = 1, n_features = 1),
  learner = lrn("classif.rpart"),
  resampling = rsmp ("cv", folds = 3),
  measure = msr("classif.ce")
  #callbacks = clbk("mlr3fselect.one_se_rule"),
)
at$train(tsk("penguins"))
