library(ggplot2)
library(mlr3proba)
library(mlr3extralearners)
library(cowplot)

plot_crank_vs_time = function(p) {
  times  = p$truth[, 1]
  status = p$truth[, 2]
  indx = which(status == 1) # only events

  # Extract truth and crank
  df = data.frame(
    time = times[indx],
    crank = p$crank[indx]
  )

  # Calculate Spearman correlation
  spearman_corr = cor(df$crank, df$time, method = "spearman")

  ggplot(df, aes(x = crank, y = time)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
    labs(
      x = "Predicted Risk",
      y = "Event time"
    ) +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("Spearman: %.2f", spearman_corr),
             hjust = 1.1, vjust = 1.5, size = 5, fontface = "italic") +
    ylim(c(0, max(times))) +
    theme_minimal()
}

#t = tsk("lung")
t = tsk("gbcs")
part = partition(t)
# only train set (overfitting)
part$test = part$train
l1 = lrn("surv.coxph")
l2 = lrn("surv.ranger")
l3 = lrn("surv.xgboost.cox", nrounds = 1000)
p1 = l1$train(t, part$train)$predict(t, part$test)
p2 = l2$train(t, part$train)$predict(t, part$test)
p3 = l3$train(t, part$train)$predict(t, part$test)
pp1 = plot_crank_vs_time(p1) + labs(title = paste0(l1$id, ", C-index: ", round(p1$score(), 2)))
pp2 = plot_crank_vs_time(p2) + labs(title = paste0(l2$id, ", C-index: ", round(p2$score(), 2)))
pp3 = plot_crank_vs_time(p3) + labs(title = paste0(l3$id, ", C-index: ", round(p3$score(), 2)))
plot_grid(pp1, pp2, pp3)
