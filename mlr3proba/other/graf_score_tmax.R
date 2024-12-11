library(mlr3proba)
library(ggplot2)

gen = tgen("coxed", T = 1000, type = "none", censor = 0.2, xvars = 6,
           mu = 1, sd = 2, censor.cond = FALSE)
n = 2000
task = gen$generate(n = n)
autoplot(task)

# censoring distr KM fit
truth = task$truth()
times = truth[, 1]
status = truth[, 2]
cens = survival::survfit(survival::Surv(times, 1 - status) ~ 1)
plot(cens)
# plot(task$kaplan(reverse = TRUE)) # same

# get t_max => for given p_max
p_max = 0.8
surv = survival::survfit(truth ~ 1)
indx = which(1 - (surv$n.risk / surv$n) > p_max)
if (length(indx) == 0L) {
  # no indexes found, get last time point
  t_max = tail(surv$time, n = 1L)
} else {
  # first time point that surpasses the specified
  # `p_max` proportion of censoring
  t_max = surv$time[indx[1L]]
}

t_max = max(times) - 1
print(t_max)

# filter observations
times2 = times[times <= t_max]
status2 = status[times <= t_max]
cens2 = survival::survfit(survival::Surv(times2, 1 - status2) ~ 1)
plot(cens2)

# Combine the data for ggplot2
cens_df = data.frame(time = cens$time, surv = cens$surv, group = "cens")
cens2_df = data.frame(time = cens2$time, surv = cens2$surv, group = "cens2")
combined_df = rbind(cens_df, cens2_df)

# Plot G(t)s
ggplot(combined_df, aes(x = time, y = surv, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(title = sprintf("G(t) - Censoring Distr via Kaplan-Meier (n = %s)", n),
       x = "Time", y = "S(t)") +
  theme_minimal() +
  scale_color_manual(
    values = c("blue", "red"),
    #labels = c("All obs", "Remove obs (80% cens)")
    labels = c("All obs", "Remove last time point")
  ) +
  theme(
    legend.position = "top",               # Move legend to the top
    legend.title = element_blank(),        # Remove legend title
    legend.justification = "center"
  )



