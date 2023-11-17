#' What value of the dcalib statistic M should be used to truncate?
library(tidyverse)

B = 20
M = 20
data = matrix(nrow = B, ncol = M)
for (b in seq(B)) {
  for (m in seq(M)) {
    data[b, m] = stats::pchisq(m, b - 1, lower.tail = FALSE)
  }
}

result_tibble = as_tibble(expand.grid(B = seq(B), M = seq(M)))
result_tibble$Value = as.vector(data)

ggplot(result_tibble, aes(x = B, y = M, fill = Value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "2D Surface Plot", x = "B", y = "M", fill = "p-value")

# p-value > 0.05 is well-calibrated

