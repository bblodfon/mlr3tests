# plot_interp_weibull = function(
#   sigma = 6,
#   shape = 1.6,
#   n_anchors = 5,
#   end_time = 10,
#   extrapolation_horizon = 2,
#   dense_n = 1200,
#   seed = 202403,
#   methods = c("const_surv", "const_dens", "const_haz"),
#   plot_mode = c("combined", "separate", "both"),
#   save_path = NULL,
#   save_width = 11,
#   save_height = 8,
#   save_dpi = 320,
#   base_size = 12,
#   legend_text_size = 11,
#   facet_text_size = 14,
#   interp_point_size = 0.95,
#   anchor_point_size = 3.4
# ) {
#   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#     stop("Please install 'ggplot2' to use this plotting example.", call. = FALSE)
#   }

#   stopifnot(
#     is.numeric(sigma), length(sigma) == 1L, sigma > 0,
#     is.numeric(shape), length(shape) == 1L, shape > 0,
#     is.numeric(end_time), length(end_time) == 1L, end_time > 0,
#     is.numeric(extrapolation_horizon), length(extrapolation_horizon) == 1L, extrapolation_horizon >= 0,
#     is.numeric(dense_n), length(dense_n) == 1L, dense_n >= 50,
#     is.numeric(n_anchors), length(n_anchors) == 1L, n_anchors >= 3,
#     is.numeric(seed), length(seed) == 1L,
#     is.numeric(save_width), length(save_width) == 1L, save_width > 0,
#     is.numeric(save_height), length(save_height) == 1L, save_height > 0,
#     is.numeric(save_dpi), length(save_dpi) == 1L, save_dpi > 0,
#     is.numeric(base_size), length(base_size) == 1L, base_size > 0,
#     is.numeric(legend_text_size), length(legend_text_size) == 1L, legend_text_size > 0,
#     is.numeric(facet_text_size), length(facet_text_size) == 1L, facet_text_size > 0,
#     is.numeric(interp_point_size), length(interp_point_size) == 1L, interp_point_size > 0,
#     is.numeric(anchor_point_size), length(anchor_point_size) == 1L, anchor_point_size > 0
#   )

#   normalize_methods = function(methods) {
#     allowed = c("const_surv", "const_dens", "const_haz")
#     stopifnot(is.character(methods), length(methods) >= 1L)
#     methods = unique(methods)
#     if (!all(methods %in% allowed)) {
#       stop("`methods` must be chosen from: const_{surv}|{dens}|{haz}", call. = FALSE)
#     }
#     methods
#   }

#   sample_anchor_times = function(n_anchors, end_time, seed) {
#     if (n_anchors <= 2L) {
#       return(c(0, end_time))
#     }

#     set.seed(seed)
#     interior_n = n_anchors - 2L

#     # Define interval (avoid exact boundaries if desired)
#     lower = 0.02 * end_time
#     upper = 0.98 * end_time

#     # LHS in 1D: stratify + jitter
#     breaks = seq(lower, upper, length.out = interior_n + 1L)
#     interior = runif(interior_n, min = breaks[-(interior_n + 1L)], max = breaks[-1L])

#     # simple uniform sampling (leads to clusters of anchors, less ideal for visualization)
#     #pool = seq(lower, upper, length.out = max(5000L, 1000L * interior_n))
#     #interior = sort(sample(pool, size = interior_n, replace = FALSE))
#     c(0, sort(interior), end_time)
#   }

#   make_save_paths = function(save_path, plot_names) {
#     if (is.null(save_path)) {
#       return(setNames(rep(NA_character_, length(plot_names)), plot_names))
#     }

#     if (!grepl("\\.[[:alnum:]]+$", save_path)) {
#       save_path = paste0(save_path, ".png")
#     }

#     ext = sub("^.*(\\.[[:alnum:]]+)$", "\\1", save_path)
#     stem = tools::file_path_sans_ext(save_path)

#     if (length(plot_names) == 1L && identical(plot_names, "combined")) {
#       return(stats::setNames(save_path, plot_names))
#     }

#     stats::setNames(
#       paste0(stem, "_", plot_names, ext),
#       plot_names
#     )
#   }

#   save_plots = function(plot_list, save_path, save_width, save_height, save_dpi) {
#     save_paths = make_save_paths(save_path = save_path, plot_names = names(plot_list))

#     if (is.null(save_path)) {
#       return(save_paths)
#     }

#     for (plot_name in names(plot_list)) {
#       ggplot2::ggsave(
#         filename = save_paths[[plot_name]],
#         plot = plot_list[[plot_name]],
#         width = save_width,
#         height = save_height,
#         dpi = save_dpi,
#         bg = "white"
#       )
#     }

#     save_paths
#   }

#   surv_fun = function(t) exp(- (t / sigma)^shape)
#   cumhaz_fun = function(t) (t / sigma)^shape
#   hazard_fun = function(t) (shape / sigma) * (t / sigma)^(shape - 1)
#   density_fun = function(t) hazard_fun(t) * surv_fun(t)

#   quantity_fun = list(
#     `S(t)` = surv_fun,
#     `H(t)` = cumhaz_fun,
#     `h(t)` = hazard_fun,
#     `f(t)` = density_fun
#   )
#   output_map = c(`S(t)` = "surv", `H(t)` = "cumhaz", `h(t)` = "hazard", `f(t)` = "density")
#   quantity_levels = c("S(t)", "H(t)", "h(t)", "f(t)")
#   method_levels = c("const_surv", "const_dens", "const_haz")
#   method_labels = c(
#     const_surv = "Constant Survival",
#     const_dens = "Constant Density",
#     const_haz = "Constant Hazard"
#   )
#   method_colors = c(
#     "Constant Survival" = "#1b9e77",
#     "Constant Density" = "#7570b3",
#     "Constant Hazard" = "#e7298a"
#   )

#   methods = normalize_methods(methods)
#   plot_mode = match.arg(plot_mode)
#   anchor_times = sample_anchor_times(n_anchors = n_anchors, end_time = end_time, seed = seed)
#   anchor_surv = surv_fun(anchor_times)
#   dense_grid = seq(0, end_time + extrapolation_horizon, length.out = dense_n)
#   eval_times = sort(unique(c(dense_grid, anchor_times)))
#   line_times = seq(0, end_time + extrapolation_horizon, length.out = max(4000L, dense_n))

#   dense_df = do.call(
#     rbind,
#     lapply(methods, function(method) {
#       do.call(
#         rbind,
#         lapply(quantity_levels, function(quantity) {
#           data.frame(
#             time = eval_times,
#             value = survdistr::interp(
#               x = anchor_surv,
#               times = anchor_times,
#               eval_times = eval_times,
#               method = method,
#               output = unname(output_map[[quantity]])
#             ),
#             method = method_labels[[method]],
#             quantity = quantity,
#             stringsAsFactors = FALSE
#           )
#         })
#       )
#     })
#   )

#   anchor_df = do.call(
#     rbind,
#     lapply(quantity_levels, function(quantity) {
#       data.frame(
#         time = anchor_times,
#         value = quantity_fun[[quantity]](anchor_times),
#         quantity = quantity,
#         legend_key = "Anchors",
#         stringsAsFactors = FALSE
#       )
#     })
#   )

#   line_df = do.call(
#     rbind,
#     lapply(quantity_levels, function(quantity) {
#       data.frame(
#         time = line_times,
#         value = quantity_fun[[quantity]](line_times),
#         quantity = quantity,
#         stringsAsFactors = FALSE
#       )
#     })
#   )

#   dense_df$quantity = factor(dense_df$quantity, levels = quantity_levels)
#   anchor_df$quantity = factor(anchor_df$quantity, levels = quantity_levels)
#   line_df$quantity = factor(line_df$quantity, levels = quantity_levels)
#   dense_df$method = factor(dense_df$method, levels = unname(method_labels[method_levels]))
#   anchor_df$legend_key = factor(anchor_df$legend_key, levels = "Anchors")

#   build_plot = function(plot_dense_df, plot_line_df, title_text) {
#     ggplot2::ggplot() +
#       ggplot2::geom_line(
#         data = plot_line_df,
#         ggplot2::aes(x = time, y = value),
#         linewidth = 0.7,
#         color = "#1f1f1f"
#       ) +
#       ggplot2::geom_point(
#         data = plot_dense_df,
#         ggplot2::aes(x = time, y = value, color = method),
#         size = interp_point_size,
#         alpha = 0.72
#       ) +
#       ggplot2::geom_point(
#         data = anchor_df,
#         ggplot2::aes(x = time, y = value, shape = legend_key),
#         color = "#d95f02",
#         fill = NA,
#         stroke = 1,
#         size = anchor_point_size
#       ) +
#       ggplot2::facet_wrap(~ quantity, ncol = 2, scales = "free_y") +
#       ggplot2::scale_color_manual(values = method_colors) +
#       ggplot2::scale_shape_manual(values = c(Anchors = 21)) +
#       ggplot2::ylim(0, NA) +
#       ggplot2::labs(
#         title = title_text,
#         subtitle = sprintf(
#           paste0(
#             "shape = %.2f, sigma = %.2f, %d anchors, last anchor S(%.0f) = %.3f, ",
#             "grid extends to %.1f"
#           ),
#           shape,
#           sigma,
#           n_anchors,
#           end_time,
#           surv_fun(end_time),
#           end_time + extrapolation_horizon
#         ),
#         x = "Time",
#         y = NULL,
#         color = NULL,
#         shape = NULL,
#         caption = paste(
#           "Black line: True Weibull | Colored points: interpolated values | ",
#           "Orange rings: anchors"
#         )
#       ) +
#       ggplot2::theme_bw(base_size = base_size) +
#       ggplot2::theme(
#         legend.position = "bottom",
#         legend.text = ggplot2::element_text(size = legend_text_size),
#         panel.grid.minor = ggplot2::element_blank(),
#         strip.text = ggplot2::element_text(size = facet_text_size, face = "bold"),
#         plot.title.position = "plot"
#       ) +
#       ggplot2::guides(
#         color = ggplot2::guide_legend(order = 1, override.aes = list(size = 4)),
#         shape = ggplot2::guide_legend(
#           order = 2,
#           override.aes = list(color = "#d95f02", fill = NA, size = anchor_point_size, stroke = 1)
#         )
#       )
#   }

#   combined_plot = build_plot(
#     plot_dense_df = dense_df,
#     plot_line_df = line_df,
#     title_text = "Interpolating Weibull-derived survival quantities with survdistr::interp()"
#   )

#   separate_plots = stats::setNames(
#     lapply(methods, function(method) {
#       build_plot(
#         plot_dense_df = dense_df[dense_df$method == method_labels[[method]], , drop = FALSE],
#         plot_line_df = line_df,
#         title_text = paste(
#           "Interpolating Weibull-derived survival quantities with",
#           method_labels[[method]]
#         )
#       )
#     }),
#     methods
#   )

#   plots = switch(
#     plot_mode,
#     combined = list(combined = combined_plot),
#     separate = separate_plots,
#     both = c(list(combined = combined_plot), separate_plots)
#   )

#   saved_paths = save_plots(
#     plot_list = plots,
#     save_path = save_path,
#     save_width = save_width,
#     save_height = save_height,
#     save_dpi = save_dpi
#   )

#   list(
#     plot = if (plot_mode == "combined") combined_plot else plots,
#     plots = plots,
#     saved_paths = saved_paths,
#     plot_mode = plot_mode,
#     anchor_times = anchor_times,
#     anchor_surv = anchor_surv,
#     eval_times = eval_times,
#     dense_data = dense_df,
#     anchor_data = anchor_df,
#     line_data = line_df,
#     parameters = list(
#       sigma = sigma,
#       shape = shape,
#       n_anchors = n_anchors,
#       end_time = end_time,
#       extrapolation_horizon = extrapolation_horizon,
#       dense_n = dense_n,
#       seed = seed,
#       methods = methods,
#       base_size = base_size,
#       legend_text_size = legend_text_size,
#       facet_text_size = facet_text_size,
#       interp_point_size = interp_point_size,
#       anchor_point_size = anchor_point_size,
#       save_path = save_path,
#       save_width = save_width,
#       save_height = save_height,
#       save_dpi = save_dpi
#     )
#   )
# }

plot_interp_weibull = function(
  sigma = 6,
  shape = 1.6,
  n_anchors = 5,
  end_time = 10,
  extrapolation_horizon = 2,
  dense_n = 1200,
  seed = 202403,
  methods = c("const_surv", "const_dens", "const_haz"),
  plot_mode = c("combined", "separate", "both"),
  save_path = NULL,
  save_width = 11,
  save_height = 8,
  save_dpi = 320,
  base_size = 12,
  legend_text_size = 11,
  facet_text_size = 14,
  interp_point_size = 0.95,
  anchor_point_size = 3.4
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install 'ggplot2' to use this plotting example.", call. = FALSE)
  }

  stopifnot(
    is.numeric(sigma), length(sigma) == 1L, sigma > 0,
    is.numeric(shape), length(shape) == 1L, shape > 0,
    is.numeric(end_time), length(end_time) == 1L, end_time > 0,
    is.numeric(extrapolation_horizon), length(extrapolation_horizon) == 1L, extrapolation_horizon >= 0,
    is.numeric(dense_n), length(dense_n) == 1L, dense_n >= 50,
    is.numeric(n_anchors), length(n_anchors) == 1L, n_anchors >= 3,
    is.numeric(seed), length(seed) == 1L,
    is.numeric(save_width), length(save_width) == 1L, save_width > 0,
    is.numeric(save_height), length(save_height) == 1L, save_height > 0,
    is.numeric(save_dpi), length(save_dpi) == 1L, save_dpi > 0,
    is.numeric(base_size), length(base_size) == 1L, base_size > 0,
    is.numeric(legend_text_size), length(legend_text_size) == 1L, legend_text_size > 0,
    is.numeric(facet_text_size), length(facet_text_size) == 1L, facet_text_size > 0,
    is.numeric(interp_point_size), length(interp_point_size) == 1L, interp_point_size > 0,
    is.numeric(anchor_point_size), length(anchor_point_size) == 1L, anchor_point_size > 0
  )

  normalize_methods = function(methods) {
    allowed = c("const_surv", "const_dens", "const_haz")
    stopifnot(is.character(methods), length(methods) >= 1L)
    methods = unique(methods)
    if (!all(methods %in% allowed)) {
      stop("`methods` must be chosen from: const_{surv}|{dens}|{haz}", call. = FALSE)
    }
    methods
  }

  sample_anchor_times = function(n_anchors, end_time, seed) {
    if (n_anchors <= 2L) {
      return(c(0, end_time))
    }

    set.seed(seed)
    interior_n = n_anchors - 2L

    # Define interval (avoid exact boundaries if desired)
    lower = 0.02 * end_time
    upper = 0.98 * end_time

    # LHS in 1D: stratify + jitter
    breaks = seq(lower, upper, length.out = interior_n + 1L)
    interior = runif(interior_n, min = breaks[-(interior_n + 1L)], max = breaks[-1L])

    # simple uniform sampling (leads to clusters of anchors, less ideal for visualization)
    #pool = seq(lower, upper, length.out = max(5000L, 1000L * interior_n))
    #interior = sort(sample(pool, size = interior_n, replace = FALSE))
    c(0, sort(interior), end_time)
  }

  make_save_paths = function(save_path, plot_names) {
    if (is.null(save_path)) {
      return(setNames(rep(NA_character_, length(plot_names)), plot_names))
    }

    if (!grepl("\\.[[:alnum:]]+$", save_path)) {
      save_path = paste0(save_path, ".png")
    }

    ext = sub("^.*(\\.[[:alnum:]]+)$", "\\1", save_path)
    stem = tools::file_path_sans_ext(save_path)

    if (length(plot_names) == 1L && identical(plot_names, "combined")) {
      return(stats::setNames(save_path, plot_names))
    }

    stats::setNames(
      paste0(stem, "_", plot_names, ext),
      plot_names
    )
  }

  save_plots = function(plot_list, save_path, save_width, save_height, save_dpi) {
    save_paths = make_save_paths(save_path = save_path, plot_names = names(plot_list))

    if (is.null(save_path)) {
      return(save_paths)
    }

    for (plot_name in names(plot_list)) {
      ggplot2::ggsave(
        filename = save_paths[[plot_name]],
        plot = plot_list[[plot_name]],
        width = save_width,
        height = save_height,
        dpi = save_dpi,
        bg = "white"
      )
    }

    save_paths
  }

  surv_fun = function(t) exp(- (t / sigma)^shape)
  cumhaz_fun = function(t) (t / sigma)^shape
  hazard_fun = function(t) (shape / sigma) * (t / sigma)^(shape - 1)
  density_fun = function(t) hazard_fun(t) * surv_fun(t)

  quantity_fun = list(
    `S(t)` = surv_fun,
    `H(t)` = cumhaz_fun,
    `h(t)` = hazard_fun,
    `f(t)` = density_fun
  )
  output_map = c(`S(t)` = "surv", `H(t)` = "cumhaz", `h(t)` = "hazard", `f(t)` = "density")
  quantity_levels = c("S(t)", "H(t)", "h(t)", "f(t)")
  method_levels = c("const_surv", "const_dens", "const_haz")
  method_labels = c(
    const_surv = "Constant Survival",
    const_dens = "Constant Density",
    const_haz = "Constant Hazard"
  )
  method_colors = c(
    "Constant Survival" = "#1b9e77",
    "Constant Density" = "#7570b3",
    "Constant Hazard" = "#e7298a"
  )

  methods = normalize_methods(methods)
  plot_mode = match.arg(plot_mode)
  anchor_times = sample_anchor_times(n_anchors = n_anchors, end_time = end_time, seed = seed)
  anchor_surv = surv_fun(anchor_times)
  dense_grid = seq(0, end_time + extrapolation_horizon, length.out = dense_n)
  eval_times = sort(unique(c(dense_grid, anchor_times)))
  line_times = seq(0, end_time + extrapolation_horizon, length.out = max(4000L, dense_n))

  dense_df = do.call(
    rbind,
    lapply(methods, function(method) {
      do.call(
        rbind,
        lapply(quantity_levels, function(quantity) {
          data.frame(
            time = eval_times,
            value = survdistr::interp(
              x = anchor_surv,
              times = anchor_times,
              eval_times = eval_times,
              method = method,
              output = unname(output_map[[quantity]])
            ),
            method = method_labels[[method]],
            quantity = quantity,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )

  anchor_df = data.frame(
    time = anchor_times,
    value = anchor_surv,
    quantity = "S(t)",
    legend_key = "Anchors",
    stringsAsFactors = FALSE
  )

  interp_window_df = data.frame(
    xmin = end_time,
    xmax = end_time + extrapolation_horizon,
    ymin = -Inf,
    ymax = Inf,
    quantity = quantity_levels,
    stringsAsFactors = FALSE
  )

  line_df = do.call(
    rbind,
    lapply(quantity_levels, function(quantity) {
      data.frame(
        time = line_times,
        value = quantity_fun[[quantity]](line_times),
        quantity = quantity,
        stringsAsFactors = FALSE
      )
    })
  )

  dense_df$quantity = factor(dense_df$quantity, levels = quantity_levels)
  anchor_df$quantity = factor(anchor_df$quantity, levels = quantity_levels)
  line_df$quantity = factor(line_df$quantity, levels = quantity_levels)
  interp_window_df$quantity = factor(interp_window_df$quantity, levels = quantity_levels)
  dense_df$method = factor(dense_df$method, levels = unname(method_labels[method_levels]))
  anchor_df$legend_key = factor(anchor_df$legend_key, levels = "Anchors")

  build_plot = function(plot_dense_df, plot_line_df, title_text) {
    ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = interp_window_df,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "grey88",
        alpha = 0.35
      ) +
      ggplot2::geom_line(
        data = plot_line_df,
        ggplot2::aes(x = time, y = value),
        linewidth = 0.7,
        color = "#1f1f1f"
      ) +
      ggplot2::geom_point(
        data = plot_dense_df,
        ggplot2::aes(x = time, y = value, color = method),
        size = interp_point_size,
        alpha = 0.72
      ) +
      ggplot2::geom_point(
        data = anchor_df,
        ggplot2::aes(x = time, y = value, shape = legend_key),
        color = "#d95f02",
        fill = NA,
        stroke = 1,
        size = anchor_point_size
      ) +
      ggplot2::facet_wrap(~ quantity, ncol = 2, scales = "free_y") +
      ggplot2::scale_color_manual(values = method_colors) +
      ggplot2::scale_shape_manual(values = c(Anchors = 21)) +
      ggplot2::ylim(0, NA) +
      ggplot2::labs(
        title = title_text,
        subtitle = sprintf(
          paste0(
            "shape = %.2f, sigma = %.2f, anchors = %d, S(%.0f) = %.3f, ",
            "dense grid extends to %.1f"
          ),
          shape,
          sigma,
          n_anchors,
          end_time,
          surv_fun(end_time),
          end_time + extrapolation_horizon
        ),
        x = "Time",
        y = NULL,
        color = NULL,
        shape = NULL,
        caption = paste(
          "Grey band: extrapolation domain | Black line: True Weibull quantity | ",
          "Colored points: interpolated values | Orange rings: anchors"
        )
      ) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = legend_text_size),
        panel.grid.minor = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = facet_text_size, face = "bold"),
        plot.title.position = "plot"
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(order = 1, override.aes = list(size = 4)),
        shape = ggplot2::guide_legend(
          order = 2,
          override.aes = list(color = "#d95f02", fill = NA, size = anchor_point_size, stroke = 1)
        )
      )
  }

  combined_plot = build_plot(
    plot_dense_df = dense_df,
    plot_line_df = line_df,
    title_text = "Interpolating Weibull-derived survival quantities with survdistr::interp()"
  )

  separate_plots = stats::setNames(
    lapply(methods, function(method) {
      build_plot(
        plot_dense_df = dense_df[dense_df$method == method_labels[[method]], , drop = FALSE],
        plot_line_df = line_df,
        title_text = paste(
          "Interpolating Weibull-derived survival quantities with",
          method_labels[[method]]
        )
      )
    }),
    methods
  )

  plots = switch(
    plot_mode,
    combined = list(combined = combined_plot),
    separate = separate_plots,
    both = c(list(combined = combined_plot), separate_plots)
  )

  saved_paths = save_plots(
    plot_list = plots,
    save_path = save_path,
    save_width = save_width,
    save_height = save_height,
    save_dpi = save_dpi
  )

  list(
    plot = if (plot_mode == "combined") combined_plot else plots,
    plots = plots,
    saved_paths = saved_paths,
    plot_mode = plot_mode,
    anchor_times = anchor_times,
    anchor_surv = anchor_surv,
    eval_times = eval_times,
    dense_data = dense_df,
    anchor_data = anchor_df,
    line_data = line_df,
    parameters = list(
      sigma = sigma,
      shape = shape,
      n_anchors = n_anchors,
      end_time = end_time,
      extrapolation_horizon = extrapolation_horizon,
      dense_n = dense_n,
      seed = seed,
      methods = methods,
      base_size = base_size,
      legend_text_size = legend_text_size,
      facet_text_size = facet_text_size,
      interp_point_size = interp_point_size,
      anchor_point_size = anchor_point_size,
      save_path = save_path,
      save_width = save_width,
      save_height = save_height,
      save_dpi = save_dpi
    )
  )
}

p = plot_interp_weibull(
  plot_mode = "both",
  save_path = "res_anchors5/weibull_interp.png",
  n_anchors = 5,
  dense_n = 100,
  extrapolation_horizon = 1.5,
  base_size = 16,
  legend_text_size = 14,
  seed = 0
)

p$plots$const_surv
p$plots$const_dens
p$plots$const_haz
p$plots$combined
