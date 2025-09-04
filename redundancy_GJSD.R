# Jensen-Shannon redundancy for a feature matrix
# X = n x p matrix (rows = samples, cols = features)
# bins = number of histogram bins
# returns redundancy in [0,1]
js_redundancy <- function(X, bins = 30) {
  # ensure matrix
  X <- as.matrix(X)
  p <- ncol(X)

  # common grid across all features
  rng <- range(X, na.rm = TRUE)
  breaks <- seq(rng[1], rng[2], length.out = bins + 1)

  # compute histograms normalized to sum = 1
  hist_list <- apply(X, 2, function(col) {
    h <- hist(col, breaks = breaks, plot = FALSE)$counts
    h <- h + 1e-12   # smoothing to avoid zeros
    h / sum(h)
  })

  # mixture distribution (average of all histograms)
  M <- rowMeans(hist_list)

  # Shannon entropy function
  entropy <- function(p) -sum(p * log2(p))

  # compute entropies
  H_M <- entropy(M)
  H_P <- mean(apply(hist_list, 2, entropy))

  # Generalized JSD (normalized to [0,1])
  GJSD <- (H_M - H_P) / log2(2)

  # Redundancy score = 1 - diversity
  R <- 1 - GJSD
  return(R)
}


set.seed(1)
X <- matrix(rnorm(200*10), nrow = 200, ncol = 10) # 200 samples, 10 features
js_redundancy(X)

X <- matrix(runif(200*10), nrow = 200, ncol = 10)
js_redundancy(X)

set.seed(42)
n <- 2000

X <- cbind(
  rnorm(n, 0, 1),                         # Normal(0,1)
  rnorm(n, 5, 1),                         # Normal(5,1) — shifted
  runif(n, -2, 2),                        # Uniform
  rexp(n, rate = 1),                      # Exponential
  rt(n, df = 3),                          # Student-t (heavy tails)
  rlnorm(n, meanlog = 0, sdlog = 0.6),    # Log-normal
  rbeta(n, 2, 5),                         # Beta(2,5) — skewed
  rbeta(n, 5, 2),                         # Beta(5,2) — opposite skew
  sample(c(rnorm(n, -2, 0.5),             # Bimodal mixture
           rnorm(n,  2, 0.5))),
  (rnorm(n)^2),                           # Chi-square(1)-like (nonnegative, skewed)
  pmin(pmax(rnorm(n, 0, 1) +              # Truncated normal (bounded)
            rnorm(n, 0, 0.5), -2), 2),
  2*sin(seq(0, 12*pi, length.out = n)) +  # Sinusoidal + noise
    rnorm(n, 0, 0.3)
)

colnames(X) <- c(
  "N01","N51","Unif","Exp","t3","LogNorm",
  "Beta25","Beta52","Bimodal","ChiLike","TruncNorm","Sine"
)
js_redundancy(X)
