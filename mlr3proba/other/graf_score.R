#' @param score matrix of IBS(i,t)
#' @param truth task$truth()/ Surv() obj of test set
#' @param unique_times unique time points of test set, same as `ncol(score)`
#' @param cens (matrix of 2 columns: 1st time points, 2nd survival probaiblity) => KM fit on cens. distr. of the train set
#' @param proper (TRUE vs FALSE)
#' @param eps (0.001 default)
r_weight_survival_score = function(score, truth, unique_times, cens, proper, eps) {
  times = truth[, 1]
  status = truth[, 2]

  cens_times = cens[, 1]
  cens_surv = cens[, 2]

  nr = nrow(score)
  nc = ncol(score)
  k = 0

  mat = matrix(0, nrow = nr, ncol = nc)

  for (i in 1:nr) {
    k = 0
    # if censored and proper then zero-out and remove
    if (proper && status[i] == 0) {
      mat[i, ] = rep(0, nc)
      next
    }
    #browser()

    for (j in 1:nc) {
      # if alive and not proper then IPC weights are current time
      if (!proper && times[i] > unique_times[j]) {
        for (l in 1:length(cens_times)) {
          if (unique_times[j] >= cens_times[l] && (l == length(cens_times) || unique_times[j] < cens_times[l + 1])) {
            mat[i, j] = score[i, j] / cens_surv[l]
            break
          }
        }
        # if dead (or alive and proper) weight by event time
        # if censored remove
      } else {
        if (status[i] == 0) {
          mat[i, j] = 0
          next
        }

        if (k == 0) {
          for (l in 1:length(cens_times)) {
            # weight 1 if death occurs before first censoring time
            if ((times[i] < cens_times[l]) && l == 1) {
              k = 1
              break
            } else if (times[i] >= cens_times[l] && (l == length(cens_times) || times[i] < cens_times[l + 1])) {
              if (cens_surv[l] > 0) {
                k = cens_surv[l]
              } else if (cens_surv[l-1] > 0) {
                # in this case the last time point was 0 (due to fixing an
                # improper KM censoring distribution), so we might as well
                # try the pre-last time point
                k = cens_surv[l-1]
              } else {
                browser() # check when this happens!
                k = eps
              }

	      # old code with inflation
              # k = cens_surv[l]
              # # k == 0 only if last obsv censored, therefore mat is set to 0 anyway
              # if (k == 0) {
              #   k = eps
              # }
              break
            }
          }
        }

        # weight by IPCW
        mat[i, j] = score[i, j] / k
      }
    }
  }

  return(mat)
}

