# install if needed
# install.packages(c("randomForestSRC", "sensitivity", "survival"))

library(randomForestSRC)
library(sensitivity)
library(survival)

# ------------------------------
# 1. Example data: veteran (survival package)
# ------------------------------
data(veteran)
head(veteran)

# We'll use a subset of features for simplicity
X <- veteran[, c("age", "diagtime", "karno")]
Y <- Surv(veteran$time, veteran$status)

# ------------------------------
# 2. Fit Random Survival Forest
# ------------------------------
set.seed(123)
rsf_model <- rfsrc(Surv(time, status) ~ age + diagtime + karno,
                   data = veteran, ntree = 200)

# ------------------------------
# 3. Define function f(x) = RSF prediction
# ------------------------------
# Let's extract survival probability at t = 365 days (1 year)
f_pred <- function(Xnew) {
  Xnew <- as.data.frame(Xnew)
  colnames(Xnew) <- c("age", "diagtime", "karno")  # ensure correct names
  pred <- predict(rsf_model, newdata = Xnew)$survival
  # find index of time ~ 365
  t_index <- which.min(abs(rsf_model$time.interest - 365))
  return(pred[, t_index])  # survival probability at 365 days
}

# ------------------------------
# 4. Setup Sobol sensitivity analysis
# ------------------------------
p <- ncol(X)
param_ranges <- apply(X, 2, range)

# generate design matrices with correct names
X1 <- as.data.frame(sapply(1:p, function(j) runif(500, param_ranges[1, j], param_ranges[2, j])))
colnames(X1) <- colnames(X)

X2 <- as.data.frame(sapply(1:p, function(j) runif(500, param_ranges[1, j], param_ranges[2, j])))
colnames(X2) <- colnames(X)

sobol_design <- sobol(
  model = f_pred,
  X1 = X1,
  X2 = X2,
  order = 3, nboot = 50
)

# ------------------------------
# 5. Results
# ------------------------------
print(sobol_design)

# Plot sensitivity indices
plot(sobol_design, main = "Sobol Indices (RSF Predictions)")
