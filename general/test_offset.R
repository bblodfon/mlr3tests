# Summary: `offset = ...` doesn't work as it should, epseically during prediction

# Simulated data
df = data.frame(x = 1:10, y = rpois(10, lambda = 5), offset_val = runif(10))
df2 = data.frame(x = 1:10, y = rpois(10, lambda = 5), offset = runif(10))

data = data.frame(x = 1:10, y = rpois(10, lambda = 5))
offset1 = runif(10)
data_with_offset = cbind(data, offset1)

task = as_task_regr(x = data_with_offset, target = "y", id = "test")
task$set_col_roles(cols = "offset1", roles = "offset")
task
part = partition(task)

l = lrn("regr.lm")
l$train(task, part$train)
l$model

# same:
model = lm(y ~ ., data = data[part$train, ], offset = offset1[part$train])
model
predict(l$model, newdata = data.frame(x = 1:2, offset1 = 1))

predict(l$model, newdata = data_with_offset[1, ])
predict(model, newdata = data_with_offset[1, ])

model = glm(y ~ x, data = data, family = poisson(), offset = offset)
model$offset

# test task$offset
predict(model, newdata = data.frame(x = rep(1:3), offset = 0))
data.frame(x = 3)

# Fit a GLM with an offset (used only during prediction)
model = glm(y ~ ., data = df, family = poisson(), offset = df$offset_val)
# I get 10 predictions!
predict(model, newdata = data.frame(x = 3))

# LOGISTIC REGRESSION
set.seed(42)
data = data.frame(x = 1:10, y = rpois(10, lambda = 5))
offset1 = runif(10)
offset2 = offset1
data_with_offset = cbind(data, offset1)
rm(offset1)
# have to use the formula like this:
#' formula: RHS : `feature_names + offset(name of offset col)`
#' `task$data()` + `offset_col`
model1 = glm(y ~ x + offset(offset1), data = data_with_offset, family = poisson())
model1
# works strangely like this as well
# First DEFINE offset1 = ...
offset1 = offset2
model = glm(y ~ . + offset(offset1), data = data, family = poisson())
model # same as model1
model$offset
model$model
model

predict(model, type = "response", newdata = data.frame(x = 3:4, offset1 = 0))
predict(model, type = "response", newdata = data.frame(x = 3:4, offset1 = 1))
# same as:
predict(model, type = "response", newdata = data.frame(x = 3:4, offset1 = 0)) * exp(1)

