# Single train/test split
set.seed(42)
n = 200
p_signal = 5
p_noise = 50

# Simulate data: true "signal" features have a stronger
# relationship with the outcome than noise features
X_signal = matrix(rnorm(n * p_signal), nrow = n, ncol = p_signal)
true_coef = runif(p_signal, 0.5, 1.5)
linear_pred = X_signal %*% true_coef
y = linear_pred + rnorm(n, sd = 0.5)

# Add noise features
X_noise = matrix(rnorm(n * p_noise), nrow = n, ncol = p_noise)
# X => first noise features, last signal features
X = cbind(X_noise, X_signal)
colnames(X) = c(paste0("noise", 1:p_noise), paste0("signal", 1:p_signal))
X[1:5, 1:5]

blocks = list(noise = 1:p_noise, signal = (p_noise + 1):(p_noise + p_signal))
blocks
# Alternative: prioritize signal features first
blocks_adaptive = list(signal = (p_noise + 1):(p_noise + p_signal), noise = 1:p_noise)
blocks_adaptive

# Split into training (80%) and test (20%)
train_idx = sample(1:n, 0.8 * n)
X_train = X[train_idx, ]
y_train = y[train_idx]
X_test = X[-train_idx, ]
y_test = y[-train_idx]

library(prioritylasso)
# "n" = noise, "s" = signal, so "ns" prioritizes noise first,
# "sn" prioritizes signal first
fit_ns = prioritylasso(
  X = X_train,
  Y = y_train,
  blocks = blocks,
  family = "gaussian",
  type.measure = "mse",
  nfolds = 5,
  standardize = TRUE
)
fit_sn = prioritylasso(
  X = X_train,
  Y = y_train,
  blocks = blocks_adaptive,
  family = "gaussian",
  type.measure = "mse",
  nfolds = 5,
  standardize = TRUE
)

# check models
fit_ns$nzero # got the coefficients on the 2nd fit
fit_ns$coefficients # noise first
fit_ns$X |> colnames() |> head() # noise first

fit_sn$nzero # got the coefficients on the 1st fit
fit_sn$coefficients # signal first, but these are the SAme coefficients
fit_ns$X |> colnames() |> head() # noise first

# Predictions
pred_ns = predict(fit_ns, newdata = X_test)
pred_sn = predict(fit_sn, newdata = X_test)

# MSE
mse_ns = mean((y_test - pred_ns)^2)
mse_sn = mean((y_test - pred_sn)^2)
# 2nd fit (signal first) should have lower MSE than 1st fit (noise first) or equal!
print(c(mse_ns, mse_sn))

# Correction: reorder newdata columns according to the block order in the model??? (but features within the block might be in a different order... also we shouldn't generally change the order of features between train and test data - mlr3 checks for this)
pred_sn2 = predict(fit_sn, newdata = X_test[, unlist(fit_sn$blocks), drop = FALSE])
mean((y_test - pred_sn2)^2) # yep, even lower than `mse_ns` as expected!

# fix in mlr3extralearners 
withr::local_seed(42)
n = 200
p_signal = 5
p_noise = 50

# Signal block: variables correlated with logistic regression outcome
X_signal = matrix(rnorm(n * p_signal), nrow = n, ncol = p_signal)
true_coef = runif(p_signal, 0.5, 1.5)
linear_pred = X_signal %*% true_coef
prob = plogis(linear_pred)
y = rbinom(n, 1, prob) # binary outcome

# Noise block: pure random noise
X_noise = matrix(rnorm(n * p_noise), nrow = n, ncol = p_noise)

# Combine blocks: put noise block first, signal block second (the opposite of desired order)
X = cbind(X_noise, X_signal)
colnames(X) = c(paste0("noise", 1:p_noise), paste0("signal", 1:p_signal))

# Define blocks list (noise block first, then signal)
blocks = list(
  noise = 1:p_noise,
  signal = (p_noise + 1):(p_noise + p_signal)
)

task = as_task_classif(x = data.frame(X, y), id = "test", target = "y", positive = "1")
part = partition(task)

# first fit priority lasso model where the blocks are taken as defined
lrn_non_adapt = lrn(
  "classif.priority_lasso",
  type.measure = "class",
  blocks = blocks,
  nfolds = 5L,
  predict_type = "prob"
)
lrn_non_adapt$id = "PL_non_adaptive"

# now fit priority block-adaptive model
lrn_adapt = lrn(
  "classif.priority_lasso",
  type.measure = "class",
  blocks = blocks,
  adaptive.order = TRUE,
  nfolds = 5L,
  predict_type = "prob"
)
lrn_adapt$id = "PL_adaptive"

expect_list(lrn_adapt$model$blocks, len = 2)
expect_equal(names(lrn_adapt$model$blocks), c("signal", "noise"))
expect_true(lrn_adapt$model$block.penalty.factors[1] <= lrn_adapt$model$block.penalty.factors[2])

bm_grid = benchmark_grid(
  tasks = list(task),
  learners = list(lrn_adapt, lrn_non_adapt),
  resamplings = list(rsmp("repeated_cv", folds = 5L, repeats = 5L))
)

bm = benchmark(bm_grid, store_models = TRUE)
# Adaptive better than non-adaptive!
bm$aggregate(msrs(c("classif.ce", "classif.logloss", "classif.auc")))

