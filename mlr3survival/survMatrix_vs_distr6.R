library(lobstr) # for object sizes
library(distr6)
library(survdistr) # maybe we will change the name
library(microbenchmark)

# make a large survival matrix
set.seed(42)
nobs = 100
ntimes = 1000
x = matrix(runif(nobs * ntimes), nrow = nobs, ncol = ntimes)
# Sort each row in decreasing order to make it a survival matrix
x = t(apply(x, 1, function(row) sort(row, decreasing = TRUE)))
colnames(x) = 1:ntimes

# CONSTRUCTION
microbenchmark(
  # R6-based
  distr6 = as.Distribution(1 - x, fun = "cdf",
                           decorators = c("CoreStatistics", "ExoticStatistics")),
  # S3-based
  survMatrix = survMatrix(x),
  unit = "ms"
)

# OBJECT SIZE
survmat = survMatrix(x)

# this is what is used in `mlr3proba::PredictionSurv()` under the hood whenever
# using the `$distr` field, which is used in all mlr3proba measures (unless we
# bypass and use directly the `x` survival matrix)
survdistr6 = as.Distribution(1 - x, fun = "cdf",
                             decorators = c("CoreStatistics", "ExoticStatistics"))
# the decorators are for $survival(), $cumHazard() etc. functions that we need
survmat
survdistr6
obj_size(survmat) # < 1MB
obj_size(survdistr6) # ~ 3MB

# GET SURVIVAL ----
# ..for in-between points, constant interpolation
# speedup factor of ~30
new_times = 0:100 + 0.03
microbenchmark(
  distr6 = t(survdistr6$survival(new_times)),
  distr6_no_transpose = survdistr6$survival(new_times),
  survMatrix = survival(survmat, times = new_times),
  times = 100#,
#  check = "equal"
)

# We now also support linear interpolation
survmat$inter_type = "linear"
microbenchmark(
  distr6 = t(survdistr6$survival(new_times)),
  survMatrix = survival(survmat, times = new_times),
  times = 100
  # check = "equal" # no longer equal!
)
