library(MASS)

# Parameters
n <- 200
p <- 500
p_signal <- 10

# Features
X <- matrix(rnorm(n * p), n, p)
beta <- c(rnorm(p_signal, 0.5, 0.2), rep(0, p - p_signal))

# Linear predictor
eta <- X %*% beta

# Survival times (Exponential baseline)
lambda <- 0.1
U <- runif(n)
T <- -log(U) / (lambda * exp(eta))

# Censoring times
C <- rexp(n, rate = 0.05)

# Observed data
time <- pmin(T, C)
status <- as.numeric(T <= C)

