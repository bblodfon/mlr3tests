# THESE ARE ALL FOR right-censored competing risk data
library(mlr3cmprsk)

# RFSRC ----
library(randomForestSRC)

# Competing Risk Datasets
data(follic, package = "randomForestSRC")
data(hd, package = "randomForestSRC")
data(pbc, package = "survival")

## WIHS analysis
## Competing risk data set involving AIDS in women
data(wihs, package = "randomForestSRC")
?wihs

#' `wihs$status`
#' 0=censoring, 1=HAART initiation, 2=AIDS/Death before HAART
nrow(wihs) # 1164
length(unique(wihs$time)) # 103 => unique time points
table(wihs$status)

## mlr3proba ----
task = TaskCompRisks$new(id = "wihs", backend = wihs, time = "time", event = "status")
task$formula()
task$truth()
task$cens_prop()
table(task$event())/task$nrow

task$unique_events()
task$cmp_events
task$set_col_roles(cols = "status", add_to = "stratum")
part = partition(task, ratio = 0.8)
table(task$event(rows = part$test))/length(part$test) # stratified partition yay

model = randomForestSRC::rfsrc(
  formula = task$formula(),
  data = task$data(rows = part$train),
  splitrule = "logrankCR",
  nsplit = 3,
  ntree = 100
)
model

# task$truth(part$test)
#' `$cif` => 3rd dimension used for the event type, each time point in
#' `time.interest` making up the second dimension (columns), and the case
#' (individual) being the first dimension (rows)
p = predict(model, newdata = task$data(rows = part$test))
dim(p$cif)
dim(p$chf) # CSCHF => cause-specific cumulative hazard function
p$predicted
length(p$time.interest)
# convert to list of matrices
cif = p$cif
dimnames(cif)[[2]] = p$time.interest
CIF = lapply(1:dim(cif)[3], function(i) cif[,,i])
names(CIF) = task$cmp_events # all competing risks
# make prediction object
pred = PredictionCompRisks$new(
  task = task$clone()$filter(rows = part$test),
  cif = CIF
)
pred

# Censoring must be coded as a NON-NEGATIVE INTEGER, where 0 indicates right-censoring, and non-zero values indicate different event types.
# While 0,1,2,..,J is standard, and recommended, events can be coded non-sequentially, although 0 must always be used for censoring.
wihs.obj = rfsrc(Surv(time, status) ~ ., data = wihs,
                  splitrule = "logrankCR", nsplit = 3, ntree = 100)
wihs.obj2 = rfsrc(Surv(time, status) ~ ., data = wihs,
                   splitrule = "logrank", nsplit = 3, ntree = 100) # needs the cause?
# logrankscore => all events as 1, censoring as 0

wihs.obj
wihs.obj2

p1 = predict(wihs.obj)
dim(p1$cif)
p2 = predict(wihs.obj2, newdata = wihs[1:5, ])
dim(p2$cif)
class(p2$cif) # 3d array
str(p2$cif)
dimnames(p2$cif)
dimnames(p2$cif)[[2]] = p2$time.interest
# that's how you put the colnames in every matrix of the array

# 1) cause-specific cumulative hazard function (CSCHF) for each event type
# 2) cumulative incidence function (CIF) for each event type
# 3) continuous probability curves (CPC) for each event (Pepe and Mori, 1993) =>
# Pepe MS Mori M, Kaplan-Meier, marginal or conditional probability curves in summarizing competing risks failure time data?, Stat. Med., 1993, vol. 12 (pg. 737-751)
dim(wihs.obj$cif) # array (n_samples x unique_death_times x causes)
dim(wihs.obj2$cif)
length(wihs.obj$time.interest) # unique death times (no?)
wihs |> as_tibble() |> filter(status == 2) |> pull(time) |> unique() |> length()

### CIF array
# Each individual is the first dimension (rows)
# Each time point in `time.interest` making up the second dimension (columns)
# The third dimension is used for the event type

plot.competing.risk(wihs.obj)
cif = wihs.obj$cif.oob
Time = wihs.obj$time.interest
idu = wihs$idu # history of injection drug use
cif.haart = cbind(apply(cif[,,1][idu == 0,], 2, mean),
                   apply(cif[,,1][idu == 1,], 2, mean))
cif.aids  = cbind(apply(cif[,,2][idu == 0,], 2, mean),
                   apply(cif[,,2][idu == 1,], 2, mean))
matplot(Time, cbind(cif.haart, cif.aids), type = "l",
        lty = c(1,2,1,2), col = c(4, 4, 2, 2), lwd = 3,
        ylab = "Cumulative Incidence")
legend("topleft",
       legend = c("HAART (Non-IDU)", "HAART (IDU)", "AIDS (Non-IDU)", "AIDS (IDU)"),
       lty = c(1,2,1,2), col = c(4, 4, 2, 2), lwd = 3, cex = 1.5)

## illustrates the various splitting rules
## illustrates event specific and non-event specific variable selection
if (library("survival", logical.return = TRUE)) {

  ## use the pbc data from the survival package
  ## events are transplant (1) and death (2)
  data(pbc, package = "survival")
  pbc$id = NULL

  ## modified Gray's weighted log-rank splitting
  ## (equivalent to cause=c(1,1) and splitrule="logrankCR")
  pbc.cr = rfsrc(Surv(time, status) ~ ., pbc)

  ## log-rank cause-1 specific splitting and targeted VIMP for cause 1
  pbc.log1 = rfsrc(Surv(time, status) ~ ., pbc,
                    splitrule = "logrankCR", cause = c(1,0), importance = TRUE)

  ## log-rank cause-2 specific splitting and targeted VIMP for cause 2
  pbc.log2 = rfsrc(Surv(time, status) ~ ., pbc,
                    splitrule = "logrankCR", cause = c(0,1), importance = TRUE)

  ## extract VIMP from the log-rank forests: event-specific
  ## extract minimal depth from the Gray log-rank forest: non-event specific
  var.perf = data.frame(md = max.subtree(pbc.cr)$order[, 1],
                         vimp1 = 100 * pbc.log1$importance[ ,1],
                         vimp2 = 100 * pbc.log2$importance[ ,2])
  print(var.perf[order(var.perf$md), ], digits = 2)
}

# CoxBoost ----
train_set = 1:1100
test_set  = 1101:nrow(wihs)
fit = CoxBoost::CoxBoost(
  time = wihs$time[train_set],
  status = wihs$status[train_set],
  x = as.matrix(wihs[train_set, c("ageatfda", "idu", "black", "cd4nadir")]),
  stepno = 100,
  penalty = 100,
  cmprsk = "csh" # cause-specific hazard (NEED TO BE SET!!!)
  #cmprsk = "sh" # specific hazard / main hazard? (default)
  #cmprsk = "ccsh" # combined cause-specific hazard
)
#' In the code, `cmprsk` controls the type of competing risks analysis:
#' `"csh"` stands for cause-specific hazards. Each cause is modeled separately, treating other causes as censored.
#' `"ccsh"` stands for combined cause-specific hazards. In this mode, the code combines information across strata or causes when selecting variables, using a joint criterion (see the use of -2*sum(log(1-pchisq(arg,df=1)))).
#'
#' The main difference is in how variable selection and boosting steps are performed:
#' With `"csh"`, each cause is handled independently.
#' With `"ccsh"`, the selection is based on a combined likelihood/statistic across all causes/strata, leading to joint variable selection.

fit$n
fit$causes

length(unique(fit$time))

tt = sort(unique(fit$time))
length(tt) # 98 => unique training time points (all competing events + censored)
p0 = predict(fit, type = "CIF")
p = predict(fit, type = "CIF", times = tt)
dim(p) # only one matrix prediction (for main event) for "sh"
class(p) # list if "csh" or "ccsh"
dim(p$`1`) # CIF matrix for cause 1
dim(p$`2`) # CIF matrix for cause 2

pall = p$`1` + p$`2` # sum CIFs from 2 causes
all(pall >= 0, pall <= 1) # rest is transition from 0 to 0 state (censoring)
