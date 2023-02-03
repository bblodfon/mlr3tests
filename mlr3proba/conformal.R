# C-index conformal example

# test C-index
test_cindex = 0.75
# train C-index distr
x = rnorm(n = 10000, mean = 0.7, sd = 0.05)

alpha = 0.05
z = qnorm(1 - alpha/2)

# Step 8: Calculate the lower and upper bounds of the conformal interval
lower_bound = test_cindex - z * global_c_index_sd_test
upper_bound = test_cindex + z * global_c_index_sd_test
