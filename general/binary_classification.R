library(mlr3verse)

data("Sonar", package = "mlbench")
task = as_task_classif(Sonar, target = "Class", positive = "M")

learner = lrn("classif.rpart", predict_type = "prob")
pred = learner$train(task)$predict(task)
C = pred$confusion
print(C)

?autoplot.PredictionClassif()

autoplot(pred, type = 'roc')
autoplot(pred, type = 'prc')
