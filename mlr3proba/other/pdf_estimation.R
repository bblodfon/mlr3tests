# check the density estimation/interpolation function from mlr3proba@v0.8.1
library(flexsurv)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(cowplot)

cmp_pdf_est = function(N = c(5, 25, 75), shape = 1, scale = 1, distr = "weibull",
                       x_max = NULL) {
  # choose cdf and density functions depending on distribution
  checkmate::assert_choice(distr, c("weibull", "weibullPH", "gompertz"))
  if (distr == "weibull") {
    cdf = stats::pweibull
    pdf = stats::dweibull
    qua = stats::qweibull
  } else if (distr == "weibullPH") {
    cdf = flexsurv::pweibullPH
    pdf = flexsurv::dweibullPH
    qua = flexsurv::qweibullPH
  } else {
    # assume scale is rate for Gompertz
    cdf = flexsurv::pgompertz
    pdf = flexsurv::dgompertz
    qua = flexsurv::qgompertz
  }

  if (is.null(x_max)) {
    #x_max = qweibull(0.99, shape, scale)
    x_max = 5 * scale
  }
  x = seq(0, x_max, length.out = 1000)

  # True density once
  true_dens = pdf(x, shape, scale)
  df_all = tibble(
    x = x,
    y = true_dens,
    N = "True"
  )

  # Loop over Ns
  for (n in N) {
    # Sample points
    x_sampled = sort(sample(x, size = n))
    surv_sampled = cdf(x_sampled, shape, scale, lower.tail = FALSE)

    dens_est = mlr3proba:::.interp_pdf(
      surv_data = list(surv = surv_sampled, time = x_sampled),
      eval_times = x
    )

    df_tmp = tibble(
      x = x,
      y = dens_est,
      N = paste0("N = ", n)
    )

    df_all = bind_rows(df_all, df_tmp)
  }

  df_all |>
    ggplot(aes(x = x, y = y, color = N)) +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = paste0(distr, " PDF: shape = ", round(shape, 3), ", scale = ", round(scale, 3)),
      #subtitle = paste0("N = ", N),
      x = "Time", y = "Density", color = "Sample Size"
    ) +
    theme_minimal()
}

# weibull
shape = 1
scale = 3
cmp_pdf_est(N = 50, shape, scale)
cmp_pdf_est(N = 100, shape, scale)
cmp_pdf_est(N = c(10, 50, 100), shape, scale)

Ns = c(10, 25, 50, 75, 100, 200)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(25, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(75, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(200, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

# WeibullPH
shape = 1
scale = 2.6
cmp_pdf_est(N = 50, shape, scale, distr = "weibullPH", x_max = 4)

Ns = c(10, 25, 50, 75, 100, 200)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "weibullPH", x_max = 4)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(50, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "weibullPH", x_max = 4)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(100, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "weibullPH", x_max = 4)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(200, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "weibullPH")
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

# Gompertz
shape = 1.6
scale = 0.8
Ns = c(10, 25, 50, 75, 100, 200)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "gompertz", x_max = 2)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(10, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "gompertz", x_max = 3)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(75, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "gompertz", x_max = 3)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)

Ns = rep(200, 6)
plot_list = lapply(Ns, cmp_pdf_est, shape = shape, scale = scale, distr = "gompertz", x_max = 1)
cowplot::plot_grid(plotlist = plot_list, ncol = 2)
