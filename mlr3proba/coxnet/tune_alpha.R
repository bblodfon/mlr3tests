#' @description
#' A function that can be used for tuning alphas for a cv_glmnet() model.
#' The function outputs a list with a tibble of the C-indexes or deviances for
#' the tested alphas ($tune_alpha_tbl) and the best alpha after tuning (e.g. the
#' one with the highest C-index ($best_alpha)).

#' @param alphas sequence of alphas to tune
#' @param x train data matrix
#' @param y Survival object (time and event) for train data matrix
#' @param foldid vector of length nrow(x) identyfying the fold group for each sample
#' @param pf Penealty factor, which penalty factor to use for each variable (column in x). 0 = do not penalize, 1 = can be penalized.
#' @param type.measure discrimination metric used for tuning lambda. "C" for C- index, "deviance"
#' @param method How to selected the best alpha. `best` selects the alpha with the
#' best score (depends on the `type.measure`, can be the alpha with the highest
#' mean C-index, or the one with the lowest mean Partial Likelihood Deviance).
#' `1se` selects the higher alpha that results in a mean score which is within
#' 1 standard-error from the best alpha (thus favoring closer to Lasso-based models).
#'
#' @returns a list with a table of the tested alphas and measures (C-indexes or
#' deviance), a graph of alphas vs the chosen metric and the final selected alpha
#' based on the method

tune_alpha <- function(alphas = seq(0, 1, 0.1), x, y, foldid, type.measure = "C", pf=rep(1, ncol(x)), method = "best") {
  atbl = future.apply::future_lapply(1:length(alphas), function(i) {
    alpha = alphas[i]
    coxnet <- cv.glmnet(x = x,
      y = y, family = "cox", foldid = foldid, standardize = FALSE, penalty.factor = pf,
      alpha = alpha, type.measure = type.measure)

    lambda_min_index <- coxnet$index["min",]
    nfolds = length(unique(foldid))

    tibble(
      alpha = alpha,
      low  = coxnet$cvlo[lambda_min_index],
      mean = coxnet$cvm[lambda_min_index],
      up   = coxnet$cvup[lambda_min_index],
      sem  = coxnet$cvsd[lambda_min_index]/sqrt(nfolds), # standard error of the mean
      model = list(coxnet)
    )
  }, future.seed = TRUE) %>% dplyr::bind_rows()

  # select best alpha based on mean CV score
  alpha_seq <- atbl %>%
    arrange(desc(mean)) %>%
    pull(alpha)
  alpha_cv = ifelse(type.measure == 'C', alpha_seq[1], alpha_seq[length(alphas)])

  if (method == "best") {
    # alpha with best mean CV score (C-index or deviance)
    sel_alpha = alpha_cv
  } else {
    # alpha within 1se of the best mean CV score
    cvm = atbl %>% filter(alpha == alpha_cv) %>% pull(mean)
    sem = atbl %>% filter(alpha == alpha_cv) %>% pull(sem)

    if (type.measure == 'C') { # C-index, higher is better
      sel_alpha = atbl %>%
        filter(mean <= cvm, mean >= cvm - sem) %>% # C-index - SEM
        slice_tail() %>% # choose higher alpha
        pull(alpha)
    } else { # deviance, lower is better
      sel_alpha = atbl %>%
        filter(mean >= cvm, mean <= cvm + sem) %>% # PLDeviance + SEM
        slice_tail() %>% # choose higher alpha
        pull(alpha)
    }
  }

  # get trained model for the selected alpha
  best_model = atbl %>%
    filter(alpha == sel_alpha) %>%
    pull(model) %>%
    `[[`(1) # unlist it

  plot <- ggplot(atbl, aes(x=alpha)) +
    geom_ribbon(aes(ymin = low, ymax = up), fill="grey80") +
    geom_ribbon(aes(ymin = mean - sem, ymax = mean + sem), fill="grey30") +
    geom_line(aes(y=mean)) +
    # add selected alpha to the plot
    geom_vline(xintercept = sel_alpha, col = 'red', linetype = 'dashed') +
    ylab(ifelse(type.measure == "C", "C-index", "Partial Likelihood Deviance"))

  list(tune_alpha_tbl = atbl, best_alpha = sel_alpha,
       best_model = best_model, plot = plot)
}
