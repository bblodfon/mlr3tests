library(survivalmodels)
set_seed(1234)
library(mlr3)
library(mlr3proba)

## get the `whas` task from mlr3proba
whas <- tsk("whas")

## create our own task from the rats dataset
rats_data <- survival::rats

## convert characters to factors
rats_data$sex <- factor(rats_data$sex, levels = c("f", "m"))
rats <- TaskSurv$new("rats", rats_data, time = "time", event = "status")

## combine in list
tasks <- list(whas, rats)

library(paradox)

search_space <- ps(
  ## p_dbl for numeric valued parameters
  dropout = p_dbl(lower = 0, upper = 1),
  weight_decay = p_dbl(lower = 0, upper = 0.5),
  learning_rate = p_dbl(lower = 0, upper = 1),

  ## p_int for integer valued parameters
  nodes = p_int(lower = 1, upper = 32),
  k = p_int(lower = 1, upper = 4)
)

search_space$extra_trafo <- function(x, param_set) {
  x$num_nodes = rep(x$nodes, x$k)
  x$nodes = x$k = NULL
  return(x)
}

library(mlr3tuning)

create_autotuner <- function(learner) {
  AutoTuner$new(
    learner = learner,
    search_space = search_space,
    resampling = rsmp("holdout"),
    measure = msr("surv.cindex"),
    terminator = trm("evals", n_evals = 2),
    tuner = tnr("random_search")
  )
}

## learners are stored in mlr3extralearners
library(mlr3extralearners)

## load learners
learners <- lrns(
  paste0("surv.", c("coxtime", "deephit", "deepsurv", "loghaz", "pchazard")),
  frac = 0.3, early_stopping = TRUE, epochs = 10, optimizer = "adam"
)

# apply our function
learners <- lapply(learners, create_autotuner)


library(mlr3pipelines)

create_pipeops <- function(learner) {
  po("encode") %>>% po("scale") %>>% po("learner", learner)
}

## apply our function
learners <- lapply(learners, create_pipeops)
learners[[1]]
#> Graph with 3 PipeOps:
#>                  ID         State           sccssors prdcssors
#>              <char>        <char>             <char>    <char>
#>              encode <<UNTRAINED>>              scale
#>               scale <<UNTRAINED>> surv.coxtime.tuned    encode
#>  surv.coxtime.tuned <<UNTRAINED>>                        scale

devtools::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.2.1 (2022-06-23)
#>  os       Ubuntu 20.04.6 LTS
#>  system   x86_64, linux-gnu
#>  ui       X11
#>  language (EN)
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       Europe/Oslo
#>  date     2024-03-13
#>  pandoc   3.1.1 @ /usr/lib/rstudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
#>
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package           * version    date (UTC) lib source
#>  backports           1.4.1      2021-12-13 [1] CRAN (R 4.2.1)
#>  bbotk               0.8.0      2024-02-29 [1] CRAN (R 4.2.1)
#>  cachem              1.0.8      2023-05-01 [1] CRAN (R 4.2.1)
#>  callr               3.7.3      2022-11-02 [1] CRAN (R 4.2.1)
#>  checkmate           2.3.1      2023-12-04 [1] CRAN (R 4.2.1)
#>  cli                 3.6.1      2023-03-23 [1] CRAN (R 4.2.1)
#>  codetools           0.2-19     2023-02-01 [1] CRAN (R 4.2.1)
#>  colorspace          2.1-0      2023-01-23 [1] CRAN (R 4.2.1)
#>  crayon              1.5.2      2022-09-29 [1] CRAN (R 4.2.1)
#>  data.table          1.15.0     2024-01-30 [1] CRAN (R 4.2.1)
#>  devtools          * 2.4.5      2022-10-11 [1] CRAN (R 4.2.1)
#>  dictionar6          0.1.3      2021-09-13 [1] CRAN (R 4.2.1)
#>  digest              0.6.33     2023-07-07 [1] CRAN (R 4.2.1)
#>  distr6              1.8.4      2023-11-23 [1] Github (xoopR/distr6@1854b22)
#>  dplyr               1.1.2      2023-04-20 [1] CRAN (R 4.2.1)
#>  ellipsis            0.3.2      2021-04-29 [1] CRAN (R 4.2.1)
#>  evaluate            0.23       2023-11-01 [1] CRAN (R 4.2.1)
#>  fansi               1.0.5      2023-10-08 [1] CRAN (R 4.2.1)
#>  fastmap             1.1.1      2023-02-24 [1] CRAN (R 4.2.1)
#>  fs                  1.6.3      2023-07-20 [1] CRAN (R 4.2.1)
#>  future              1.33.0     2023-07-01 [1] CRAN (R 4.2.1)
#>  generics            0.1.3      2022-07-05 [1] CRAN (R 4.2.1)
#>  ggplot2             3.4.4      2023-10-12 [1] CRAN (R 4.2.1)
#>  globals             0.16.2     2022-11-21 [1] CRAN (R 4.2.1)
#>  glue                1.6.2      2022-02-24 [1] CRAN (R 4.2.1)
#>  gtable              0.3.4      2023-08-21 [1] CRAN (R 4.2.1)
#>  here                1.0.1      2020-12-13 [1] CRAN (R 4.2.1)
#>  htmltools           0.5.6      2023-08-10 [1] CRAN (R 4.2.1)
#>  htmlwidgets         1.6.2      2023-03-17 [1] CRAN (R 4.2.1)
#>  httpuv              1.6.11     2023-05-11 [1] CRAN (R 4.2.1)
#>  jsonlite            1.8.7      2023-06-29 [1] CRAN (R 4.2.1)
#>  knitr               1.43       2023-05-25 [1] CRAN (R 4.2.1)
#>  later               1.3.1      2023-05-02 [1] CRAN (R 4.2.1)
#>  lattice             0.21-8     2023-04-05 [1] CRAN (R 4.2.1)
#>  lgr                 0.4.4      2022-09-05 [1] CRAN (R 4.2.1)
#>  lifecycle           1.0.3      2022-10-07 [1] CRAN (R 4.2.1)
#>  listenv             0.9.0      2022-12-16 [1] CRAN (R 4.2.1)
#>  magrittr            2.0.3      2022-03-30 [1] CRAN (R 4.2.1)
#>  Matrix              1.6-1      2023-08-14 [1] CRAN (R 4.2.1)
#>  memoise             2.0.1      2021-11-26 [1] CRAN (R 4.2.1)
#>  mime                0.12       2021-09-28 [1] CRAN (R 4.2.1)
#>  miniUI              0.1.1.1    2018-05-18 [1] CRAN (R 4.2.1)
#>  mlr3              * 0.18.0     2024-03-05 [1] CRAN (R 4.2.1)
#>  mlr3extralearners * 0.7.1-9000 2024-03-08 [1] Github (mlr-org/mlr3extralearners@5baa86a)
#>  mlr3misc            0.14.0     2024-02-15 [1] Github (mlr-org/mlr3misc@c0673db)
#>  mlr3pipelines     * 0.5.0-9000 2024-03-08 [1] Github (mlr-org/mlr3pipelines@c52d7e1)
#>  mlr3proba         * 0.6.0      2024-02-21 [1] Github (mlr-org/mlr3proba@ed6c351)
#>  mlr3tuning        * 0.20.0     2024-03-05 [1] CRAN (R 4.2.1)
#>  mlr3viz             0.8.0      2024-03-05 [1] CRAN (R 4.2.1)
#>  munsell             0.5.0      2018-06-12 [1] CRAN (R 4.2.1)
#>  ooplah              0.2.0      2022-01-21 [1] CRAN (R 4.2.1)
#>  palmerpenguins      0.1.1      2022-08-15 [1] CRAN (R 4.2.1)
#>  paradox           * 1.0.0      2024-02-28 [1] Github (mlr-org/paradox@5a353d9)
#>  parallelly          1.36.0     2023-05-26 [1] CRAN (R 4.2.1)
#>  param6              0.2.4      2022-10-31 [1] Github (xoopR/param6@0fa3577)
#>  pillar              1.9.0      2023-03-22 [1] CRAN (R 4.2.1)
#>  pkgbuild            1.4.2      2023-06-26 [1] CRAN (R 4.2.1)
#>  pkgconfig           2.0.3      2019-09-22 [1] CRAN (R 4.2.1)
#>  pkgload             1.3.2.1    2023-07-08 [1] CRAN (R 4.2.1)
#>  png                 0.1-8      2022-11-29 [1] CRAN (R 4.2.1)
#>  prettyunits         1.1.1      2020-01-24 [1] CRAN (R 4.2.1)
#>  processx            3.8.2      2023-06-30 [1] CRAN (R 4.2.1)
#>  profvis             0.3.8      2023-05-02 [1] CRAN (R 4.2.1)
#>  promises            1.2.1      2023-08-10 [1] CRAN (R 4.2.1)
#>  ps                  1.7.5      2023-04-18 [1] CRAN (R 4.2.1)
#>  purrr               1.0.2      2023-08-10 [1] CRAN (R 4.2.1)
#>  R.cache             0.16.0     2022-07-21 [1] CRAN (R 4.2.1)
#>  R.methodsS3         1.8.2      2022-06-13 [1] CRAN (R 4.2.1)
#>  R.oo                1.25.0     2022-06-12 [1] CRAN (R 4.2.1)
#>  R.utils             2.12.2     2022-11-11 [1] CRAN (R 4.2.1)
#>  R6                  2.5.1      2021-08-19 [1] CRAN (R 4.2.1)
#>  rappdirs            0.3.3      2021-01-31 [1] CRAN (R 4.2.1)
#>  Rcpp                1.0.11     2023-07-06 [1] CRAN (R 4.2.1)
#>  remotes             2.4.2.1    2023-07-18 [1] CRAN (R 4.2.1)
#>  reprex              2.0.2      2022-08-17 [1] CRAN (R 4.2.1)
#>  reticulate          1.35.0     2024-01-31 [1] CRAN (R 4.2.1)
#>  RhpcBLASctl         0.23-42    2023-02-11 [1] CRAN (R 4.2.1)
#>  rlang               1.1.1      2023-04-28 [1] CRAN (R 4.2.1)
#>  rmarkdown           2.24       2023-08-14 [1] CRAN (R 4.2.1)
#>  rprojroot           2.0.3      2022-04-02 [1] CRAN (R 4.2.1)
#>  rstudioapi          0.15.0     2023-07-07 [1] CRAN (R 4.2.1)
#>  scales              1.2.1      2022-08-20 [1] CRAN (R 4.2.1)
#>  sessioninfo         1.2.2      2021-12-06 [1] CRAN (R 4.2.1)
#>  set6                0.2.6      2023-09-01 [1] Github (xoopR/set6@a901255)
#>  shiny               1.7.5      2023-08-12 [1] CRAN (R 4.2.1)
#>  stringi             1.7.12     2023-01-11 [1] CRAN (R 4.2.1)
#>  stringr             1.5.0      2022-12-02 [1] CRAN (R 4.2.1)
#>  styler              1.10.2     2023-08-29 [1] CRAN (R 4.2.1)
#>  survival            3.5-7      2023-08-14 [1] CRAN (R 4.2.1)
#>  survivalmodels    * 0.1.19     2024-03-11 [1] Github (RaphaelS1/survivalmodels@f418791)
#>  tibble              3.2.1      2023-03-20 [1] CRAN (R 4.2.1)
#>  tidyselect          1.2.0      2022-10-10 [1] CRAN (R 4.2.1)
#>  urlchecker          1.0.1      2021-11-30 [1] CRAN (R 4.2.1)
#>  usethis           * 2.2.2      2023-07-06 [1] CRAN (R 4.2.1)
#>  utf8                1.2.4      2023-10-22 [1] CRAN (R 4.2.1)
#>  uuid                1.1-1      2023-08-17 [1] CRAN (R 4.2.1)
#>  vctrs               0.6.3      2023-06-14 [1] CRAN (R 4.2.1)
#>  withr               2.5.2      2023-10-30 [1] CRAN (R 4.2.1)
#>  xfun                0.40       2023-08-09 [1] CRAN (R 4.2.1)
#>  xtable              1.8-4      2019-04-21 [1] CRAN (R 4.2.1)
#>  yaml                2.3.7      2023-01-23 [1] CRAN (R 4.2.1)
#>
#>  [1] /opt/R/4.2.1/lib/R/library
#>
#> ─ Python configuration ───────────────────────────────────────────────────────
#>  python:         /usr/bin/python3
#>  libpython:      /usr/lib/python3.8/config-3.8-x86_64-linux-gnu/libpython3.8.so
#>  pythonhome:     //usr://usr
#>  version:        3.8.10 (default, Nov 22 2023, 10:22:35)  [GCC 9.4.0]
#>  numpy:          /usr/lib/python3/dist-packages/numpy
#>  numpy_version:  1.17.4
#>  numpy:          /usr/lib/python3/dist-packages/numpy
#>
#>  NOTE: Python version was forced by RETICULATE_PYTHON_FALLBACK
#>
#> ──────────────────────────────────────────────────────────────────────────────
