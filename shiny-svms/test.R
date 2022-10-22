library(mlr3verse)
library(tidyverse)

# subset iris dataset and save as a binary classification task
task = tsk('iris')

iris_data = task$data() %>%
  filter(Species != 'setosa') %>% # make target binary
  select(c('Petal.Length', 'Petal.Width', 'Species')) %>% # keep 2 features and target column
  mutate(Species = factor(Species, levels = c('versicolor', 'virginica'))) %>%
  unique(by = c('Petal.Length', 'Petal.Width')) # remove common rows

iris_task = TaskClassif$new(id = 'Iris-subtask',
  backend = iris_data, target = 'Species',
  positive = 'virginica')
saveRDS(iris_task, file = 'iris_task.rds')

# test SVM model train
#' The cost comes from the quadratic optimization problem formalization
#' for the support vector classifier. min(1/2 x ||b||^2 + C x Sum(ξ))
#'
#' C large => ξ(i) slack variables small => no violations are allowed (hard, narrower margin) => small #SVs => low bias, high variance => overfitting (push for best possible accuracy)
#'
#' C small => ξ(i) slack variables large => violations are allowed (soft, wider margin) => large #SVs => high bias, low variance => underfitting
cost = 10

learner = lrn('classif.svm', kernel = 'linear', type = 'C-classification',
               cost = cost, scale = FALSE)
learner$train(iris_task)
svm_model = learner$model
sum(svm_model$nSV)

# plot model (old way)
cf = coef(svm_model)
cf
plot(Petal.Width ~ Petal.Length, data = iris_data, col = Species)
intercept = -cf[1]/cf[3]
slope     = -cf[2]/cf[3]
M = 1/cf[3] # margin
intercept
slope
M
abline(intercept, slope, col = 'red') # Decision boundary (hyperplane)
# plot margin and mark support vectors
abline(intercept - M, slope, col = 'blue')
abline(intercept + M, slope, col = 'blue')
points(svm_model$SV, pch = 5, cex = 1, col = 'purple') # doesn't work?

# plot model (ggplot)
w = t(svm_model$coefs) %*% svm_model$SV
w
#calculate slope and intercept of decision boundary from weight vector and svm model
slope_1 = -w[1]/w[2]
intercept_1 = svm_model$rho/w[2]
M_1 = 1/w[2]
stopifnot(slope_1 == slope, intercept_1 == intercept, M_1 == M)

iris_data %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  # add Support Vectors
  geom_point(data = as.data.frame(svm_model$SV),
    aes(x = Petal.Length, y = Petal.Width),
    color = '#ff80ff', size = 7, alpha = 0.3) +
  # add decision boundary
  geom_abline(slope = slope_1, intercept = intercept_1, color = 'purple', size = 1) +
  # add margin boundaries
  geom_abline(slope = slope_1, intercept = intercept_1 - M_1, linetype = 'dashed') +
  geom_abline(slope = slope_1, intercept = intercept_1 + M_1, linetype = 'dashed') +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c('#f18384', '#89b7dc'))

