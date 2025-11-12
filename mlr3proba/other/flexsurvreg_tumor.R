library(flexsurv) # For flexsurvreg() and the tumor dataset
library(survival) # For Surv() object and concordance()
library(caTools)  # For sample.split() for a stratified train/test split

data("tumor", package = "pammtools")

tumor$charlson_score = NULL

set.seed(123) # Set a seed for reproducibility
# Stratify the split based on the 'status' to ensure similar event rates in both sets
split = sample.split(tumor$status, SplitRatio = 0.7)
train_set = subset(tumor, split == TRUE)
test_set  = subset(tumor, split == FALSE)

cat("Training set size:", nrow(train_set), "\n")
cat("Test set size:", nrow(test_set), "\n")

mod = flexsurvreg(
  formula = Surv(days, status) ~ complications + age + sex + transfusion + metastases + resection,
  anc = list(shape = ~ complications),
  data = train_set,
  dist = "weibull"
)

mod2 = flexsurvreg(
  formula = Surv(days, status) ~ complications + age + sex + transfusion + metastases + resection,
  anc = list(shape = ~ complications),
  data = train_set,
  dist = "weibullPH"
)

mod3 = flexsurvspline(
  formula = Surv(days, status) ~ age + sex + transfusion + metastases + resection + complications,
  k = 0,
  anc = list(gamma1 = ~ complications),
  data = train_set
)

mod4 = flexsurvspline(
  formula = Surv(days, status) ~ age,
  k = 0,
  anc = list(gamma1 = ~ age),
  data = train_set
)

coef(mod)
coef(mod2)
coef(mod3)
coef(mod4)
# gamma0 is scale
# gamma1 is exp(scale)

#mod$dlist$name
#mod2$dlist$name

p1 = predict(mod, newdata = test_set, type = "lp")
p2 = predict(mod2, newdata = test_set, type = "lp")
p3 = predict(mod3, newdata = test_set, type = "lp")
p4 = predict(mod4, newdata = test_set, type = "lp")
#head(p1)
#head(p2)
# same ranks for mod2 and mod3:
cor(p2$.pred_link, p3$.pred_link, method = "kendall")

concordance(
  Surv(test_set$days, test_set$status) ~ p1$.pred_link
)$concordance

concordance(
  Surv(test_set$days, test_set$status) ~ p2$.pred_link, reverse = TRUE
)$concordance

concordance(
  Surv(test_set$days, test_set$status) ~ p3$.pred_link, reverse = TRUE
)$concordance

concordance(
  Surv(test_set$days, test_set$status) ~ p4$.pred_link, reverse = TRUE
)$concordance

# mlr3 way to test `flexsurvspline`
library(mlr3extralearners)
library(mlr3proba)

train_task = as_task_surv(train_set, time = "days", event = "status")
test_task = as_task_surv(test_set, time = "days", event = "status")

learner = lrn("surv.flexible", k = 0, anc = list(gamma1 = ~ age), formula = Surv(days, status) ~ age)
learner = lrn("surv.flexible", k = 0, anc = list(gamma1 = ~ complications),
              formula = Surv(days, status) ~ age + sex + transfusion + metastases + resection + complications)

learner$train(train_task)
learner$model

coef(learner$model) - coef(mod2)
coef(learner$model) - coef(mod3) # same as mod3

p = learner$predict(test_task)
p$score()
