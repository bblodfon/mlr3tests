library(mlr3proba)

#' Convert returned result list from `codex::sim.survdata` to a
#' `mlr3proba::TaskSurv` task object.
#' @param res result list from `coxed::sim.survdata()`
as_surv_task = function(res, id = "coxed.sim.surv") {
  mlr3proba::as_task_surv(x = res$data, time = "y",
    event = "failed", type = "right", id = id)
}

#' Get a PredictionSurv object to use in `mlr3proba` metrics
#' @param res result list from `coxed::sim.survdata()`
#' @param row_ids vectors of ids for the observations that we need to extract
#' the survival probabilities. If `NULL`, use all observations in `res$ind.survive`
get_pred_surv = function(res, row_ids = NULL) {
  task = as_surv_task(res)
  lp = res$xb[,1]
  surv = res$ind.survive # survival matrix (N x T)
  ntimes = ncol(surv)
  p = mlr3proba::.surv_return(times = seq_len(ntimes), surv = surv)
  if (is.null(row_ids)) {
    row_ids = seq_len(nrow(surv))
  }

  mlr3proba::PredictionSurv$new(
    row_ids = row_ids, truth = task$truth(rows = row_ids),
    distr = p$distr[row_ids, ], crank = p$crank[row_ids], lp = lp[row_ids]
  )
}

#' Get censoring stats (censoring proportion + admin censoring proportion)
#' from an mlr3 `TaskSurv`
cens_stats = function(task) {
  truth = task$truth()
  times = truth[, 1]
  status = truth[, 2]

  cens_prop = sum(status == 0)/length(status)

  cens_times = times[status == 0]
  cens_tmax = max(cens_times) # max censoring time
  admin_cens_prop = sum(cens_times == cens_tmax)/length(cens_times)

  return(list(cens_prop = cens_prop, admin_cens_prop = admin_cens_prop))
}

#' @param surv survival matrix (N x T) from `res$ind.survive`
#' @param ids which observations to plot
#' @param type Type of output survival lines => `step` uses `geom_step`, `line`
#' uses `geom_line`
plot_surv = function(surv, ids = NULL, type = "step") {
  checkmate::assert_choice(type, choices = c("step", "line"))
  colnames(surv) = 1:ncol(surv) # times
  rownames(surv) = 1:nrow(surv) # obs ids

  if (is.null(ids)) {
    ids = seq_len(nrow(surv)) # keep all observations
  }

  # NxT => TxN
  surv = t(surv)

  # subset to specific observations
  surv = surv[, ids, drop = FALSE]

  # long format: (obs-id x time x surv-prob)
  surv_tbl =
    surv |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "time") |>
    dplyr::mutate(time = as.numeric(time)) |>
    tidyr::pivot_longer(cols = !time, values_to = "surv", names_to = "id") |>
    dplyr::relocate(id)

  p = surv_tbl %>%
    ggplot(aes(x = time, y = surv))

  if (type == "step") {
    p = p + geom_step(aes(color = id))
  } else {
    p = p + geom_line(aes(color = id))
  }

  p = p +
    xlab('Time') +
    ylab('Survival Probability') +
    theme_bw(base_size = 14)
  p
}
