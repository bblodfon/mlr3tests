library(mlr3verse)
library(mlr3benchmark)
library(dplyr)

aggr_res = readRDS(file = 'issues/benchmark_aggr.rds')
aggr_res

ba = BenchmarkAggr$new(aggr_res)

autoplot(ba, type = 'mean', meas = 'HarrellC_median')

autoplot(ba, type = 'cd', meas = 'HarrellC_median', minimize = FALSE, style = 2)
# coxph is best?!

# ranks are wrongly calculated?
ranks = ba$rank_data(meas = 'HarrellC_median', minimize = FALSE)

# 1st example
ranks[,"Clinical-miRNA"]
aggr_res %>%
  filter(task_id == 'Clinical-miRNA') %>%
  arrange(desc(HarrellC_median)) # completely different

# 2nd example
ranks[,"Clinical"]
aggr_res %>%
  filter(task_id == 'Clinical') %>%
  arrange(desc(HarrellC_median)) # completely different
