#' C_Vec_WeightedDiscreteCdf(x, data, cdf, lower, logp) 
#' @param x times to extend distribution
#' @param data times of the currect cdf distr
#' @param cdf matrix, rows => times (data) and columns => patients
#' @param lower TRUE => returns cdf, FALSE => return survival (1 - cdf)

# see also => https://github.com/mlr-org/mlr3proba/pull/337#discussion_r1390432520
