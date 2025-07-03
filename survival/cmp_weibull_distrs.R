library(ggplot2)
library(tidyr)

cmp_weibull_distrs = function(shape1, scale1, shape2, scale2, type = "surv") {
  stopifnot(type %in% c("surv", "cdf", "pdf"))

  x_vals = seq(0, 3 * scale1, length.out = 100)  # Define range for x-axis
  if (type == "surv") {
    y_vals1 = pweibull(x_vals, shape = shape1, scale = scale1, lower.tail = FALSE)
    y_vals2 = pweibull(x_vals, shape = shape2, scale = scale2, lower.tail = FALSE)
    y_title = "Survival Probability"
  } else if (type == "cdf") {
    y_vals1 = pweibull(x_vals, shape = shape1, scale = scale1, lower.tail = TRUE)
    y_vals2 = pweibull(x_vals, shape = shape2, scale = scale2, lower.tail = TRUE)
    y_title = "CDF"
  } else if (type == "pdf") {
    y_vals1 = dweibull(x_vals, shape = shape1, scale = scale1)
    y_vals2 = dweibull(x_vals, shape = shape2, scale = scale2)
    y_title = "PDF"
  }

  df = data.frame(x = x_vals, y1 = y_vals1, y2 = y_vals2) |>
    pivot_longer(cols = c("y1" ,"y2"), names_to = "curve", values_to = "y")

  # Plot setup
  ggplot(df, aes(x = x, y = y, color = curve)) +
    geom_line(linewidth = 1.2) +
    labs(
      title = paste0(
        "y1: Weibull(shape = ", round(shape1, 3), ", scale = ", round(scale1, 3), ") vs\n",
        "y2: Weibull(shape = ", round(shape2, 3), ", scale = ", round(scale2, 3), ")"
      ),
      x = "Time", y = y_title, color = "Legend"
    ) +
    theme_minimal()
}

cmp_weibull_distrs(shape1 = 1, scale1 = 1.4, shape2 = 2, scale2 = 2, type = "surv")
cmp_weibull_distrs(shape1 = 1, scale1 = 1.4, shape2 = 2, scale2 = 2, type = "pdf")
