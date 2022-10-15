library(mlr3proba)
library(mlr3extralearners)

task = tsk('rats')
task$select(cols = c('litter', 'rx'))

ranger_lrn = lrn('surv.ranger')
p = ranger_lrn$train(task)$predict(task)
# no `lp` prediction
p

# SOS: Can't get ROC AUC for ranger learner!!!
p$score(msr('surv.uno_auc'), task, train_set = 1:task$nrow)

# all ROC AUCs measures require `lp` prediction type
for (measure in mlr_measures$keys(pattern = '^surv')) { # '^surv.*auc'
  print(paste0(measure, ' : ',mlr_measures$get(measure)$predict_type))
}

# Answer: there is time-dependent (distr) C-index (Antolini's C-index)