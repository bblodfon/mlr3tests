library(distr6)

a = VectorDistribution$new(
  list(
    Binomial$new(prob = 0.1, size = 2),
    Binomial$new(prob = 0.6, size = 4)
  ), decorators = "ExoticStatistics"
)
a$survival(c(0,1))

b = VectorDistribution$new(
  list(
    Exponential$new(rate = 2),
    Exponential$new(rate = 3)
  ), decorators = "ExoticStatistics"
)
b$survival(c(0,1))

distr_list = list(a,b)

# cmb = do.call(c, distr_list)
# cmb$survival(c(0,1)) # doesn't work
#
# cmb = c(a, b, decorators = "ExoticStatistics")
# cmb$survival(c(0,1)) # works

# SOS => do.call general command
# do.call(fun, c(args, list(otherargs)))
res = do.call(c, c(distr_list, list(decorators = c("CoreStatistics", "ExoticStatistics"))))
res$survival(c(0,1))

# use Matdist
mats = replicate(3, {
  pdf = runif(200)
  mat = matrix(pdf, 20, 10, FALSE, list(NULL, sort(sample(1:20, 10))))
  mat = t(apply(mat, 1, function(x) x / sum(x)))
  as.Distribution(mat, fun = "pdf", decorators = c("CoreStatistics", "ExoticStatistics"))
})
mats[[1]]$survival(4)

res = do.call(c, mats)
res$survival(4)

# do.call(fun, c(args, list(otherargs)))
res = do.call(c, c(mats, list(decorators = c("CoreStatistics", "ExoticStatistics"))))
res$survival(4)

# use Arrdist
arr = replicate(3, {
  pdf = runif(400)
  arr = array(pdf, c(20, 10, 2), list(NULL, sort(sample(1:20, 10)), NULL))
  arr = aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  as.Distribution(arr, fun = "pdf", decorators = c("CoreStatistics", "ExoticStatistics"))
})
res = do.call(c, arr)
res$survival(3)

res = do.call(c, c(arr, list(decorators = c("CoreStatistics", "ExoticStatistics"))))
res$survival(3)

# Combine Matdist and vector distributions
# vectorize Matdist
vect_matd = as.Distribution(obj = gprm(mats[[1]], 'pdf'), fun = 'pdf', decorators = c("CoreStatistics", "ExoticStatistics"), vector = TRUE)

distr_list = list(a,b,vect_matd)
distr_list

res = do.call(c, c(distr_list, list(decorators = c("CoreStatistics", "ExoticStatistics"))))
res$survival(3)
