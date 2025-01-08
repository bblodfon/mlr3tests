library(mlr3proba)
library(mlr3extralearners)
library(ggplot2)

measures = list(
  msr("surv.cindex", id = "harrell_c", label = "Harrell's C"),
  msr("surv.cindex", id = "uno_c", weight_meth = "G2", label = "Uno's C"),
  msr("surv.rcll", id = "rcll"),
  msr("surv.logloss", id = "logloss"),
  msr("surv.intlogloss", id = "intlogloss"),
  msr("surv.calib_alpha", id = "caliba"),
  msr("surv.dcalib", id = "dcalib", truncate = 10),
  msr("surv.calib_index", id = "ICI"),
  msr("surv.graf", id = "graf_proper", proper = TRUE, ERV = FALSE, label = "Graf Score (Proper)"),
  msr("surv.graf", id = "graf_improper", proper = FALSE, ERV = FALSE, label = "Graf Score (Improper)")
)
names(measures) = mlr3misc::ids(measures)

# Example bmr
bmr = benchmark(benchmark_grid(
  tasks = tsks(c("rats", "gbcs", "grace")),
  learners = lrns(c("surv.ranger", "surv.coxph")),
  resamplings = rsmp("cv", folds = 3)
), store_backends = TRUE)

bmr

# benchmark (or bmr$aggregate(...))
bm = suppressWarnings(
  bench::mark(
    harrell_c = bmr$score(measures = measures[["harrell_c"]]),
    uno_c = bmr$score(measures = measures[["uno_c"]]),
    rcll = bmr$score(measures = measures[["rcll"]]),
    logloss = bmr$score(measures = measures[["logloss"]]),
    intlogloss = bmr$score(measures = measures[["intlogloss"]]),
    caliba = bmr$score(measures = measures[["caliba"]]),
    dcalib = bmr$score(measures = measures[["dcalib"]]),
    ICI = bmr$score(measures = measures[["ICI"]]), # uses `polspline`
    graf_proper = bmr$score(measures = measures[["graf_proper"]]),
    graf_improper = bmr$score(measures = measures[["graf_improper"]]),
    check = FALSE,
    iterations = 20
  )
)

saveRDS(bm, file = "mlr3proba/other/bm.rds")

# results table
bm

# plot(bm, type = "violin")

# plot by execution time (ordered)
bm |> dplyr::mutate(
  expression = forcats::fct_reorder(as.character(expression), median, .desc = TRUE)
  ) |>
  autoplot("violin")
