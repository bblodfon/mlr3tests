# John's method using beta distribution to control the sampling of subset sizes
# `shape2` => between 0 and 1 more uniform, larger than 1 more skewed
generate_subset_sizes = function(n, size, shape2 = 10) {
  bias = dbeta(seq(1, 0.001, length.out = n), shape1 = 0.5, shape2 = shape2)
  bb = bias/sum(bias) # normalize to probabilities of number selection n:1

  bb[1] = 1 # always include the largest number of features (n)
  if (n <= size) {
    sizes = n:1
  } else {
    sizes = sort(sample(x = n:1, size = size, replace = FALSE, prob = bb),
                 decreasing = TRUE)
  }

  sizes
}

generate_subset_sizes(n = 2000, size = 15)
generate_subset_sizes(n = 2000, size = 15)
# larger shape2, more lower features
generate_subset_sizes(n = 2000, size = 15, shape2 = 20)
generate_subset_sizes(n = 2000, size = 15, shape2 = 20)
# for lower sizes, you can play with shape2 to get different subsets each time:
generate_subset_sizes(n = 20, size = 15, shape2 = 1)
generate_subset_sizes(n = 20, size = 15, shape2 = 1)


# example code
# n:1 numbers (subset_sizes) => sample with beta distribution
# shape1 => alpha (don't change)
# shape2 => beta (configurable, the larger than 1, the more "right-tail" bias)
bias = dbeta(seq(1, 0.001, length.out = n), shape1 = 0.5, shape2 = 15)
bb = bias/sum(bias)
# first include always (?), eg the largest number of features (2000)
bb[1] = 1
bb[n]
# check how probable is to select the 40 lower number of features (1:40):
bb[n:(n-40)]
# get `size` subsets
size = 15
stopifnot(n > size)
set.seed(42)
sort(sample(x = n:1, size = size, replace = FALSE, prob = bb), decreasing = TRUE)
sort(sample(x = n:1, size = size, replace = FALSE, prob = bb), decreasing = TRUE)
sort(sample(x = n:1, size = size, replace = FALSE, prob = bb), decreasing = TRUE)

# check low number features
summary(sapply(1:100, function(i) {
  res = sort(sample(x = n:1, size = 15, replace = FALSE, prob = bb), decreasing = TRUE)
  sum(res < 30)
}))


