# THESE ARE ALL FOR right-censored competing risk data

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
length(unique(wihs$time)) # 103

# mlr3proba task ???
wihs$status
wihs$status = factor(wihs$status)
task_wihs = TaskCompRisks$new(id = "wihs", backend = wihs, x = wihs, time = "time", event = "status")
task_wihs$cens_type
task_wihs$truth()

# Censoring must be coded as a NON-NEGATIVE INTEGER, where 0 indicates right-censoring, and non-zero values indicate different event types.
# While 0,1,2,..,J is standard, and recommended, events can be coded non-sequentially, although 0 must always be used for censoring.
wihs.obj = rfsrc(Surv(time, status) ~ ., data = wihs,
                  splitrule = "logrankCR", nsplit = 3, ntree = 100)
wihs.obj2 = rfsrc(Surv(time, status) ~ ., data = wihs,
                   splitrule = "logrank", nsplit = 3, ntree = 100)
wihs.obj
wihs.obj2

p1 = predict(wihs.obj)
dim(p1$cif)
p2 = predict(wihs.obj, newdata = wihs[1:5, ])
dim(p2$cif)
class(p2$cif) # 3d array
str(p2$cif)
dimnames(p2$cif)
dimnames(p2$cif)[[2]] = p2$time.interest # that's how you put the colnames in every matrix of the array

# 1) cause-specific cumulative hazard function (CSCHF) for each event type
# 2) cumulative incidence function (CIF) for each event type
# 3) continuous probability curves (CPC) for each event (Pepe and Mori, 1993) =>
# Pepe MS Mori M, Kaplan-Meier, marginal or conditional probability curves in summarizing competing risks failure time data?, Stat. Med., 1993, vol. 12 (pg. 737-751)
dim(wihs.obj$cif) # array (n_samples x unique_death_times x causes)
dim(wihs.obj2$cif)
length(wihs.obj$time.interest) # unique death times (no?)
wihs |> as_tibble() |> filter(status == 2) |> pull(time) |> unique() |> length()

### CIF array
# Each time point in `time.interest` making up the second dimension (columns)
# Each individual is the first dimension (rows)
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
  cmprsk = "csh" # cause-specific hazard
  #cmprsk = "sh" # specific hazard / main hazard? (default)
  #cmprsk = "ccsh" # cumulative cause-specific hazard?
)
fit$n
fit$causes # "`1`

length(unique(fit$time))

tt = sort(unique(fit$time))
length(tt) # 98
p = predict(fit, type = "CIF", times = sort(unique(fit$time)))
dim(p) # only one matrix prediction (for main event) for "sh"
class(p) # list if "csh"
dim(p$`1`) # CIF matrix for cause 1
dim(p$`2`) # CIF matrix for cause 2

pall = p$`1` + p$`2` # sum CIFs from 2 causes
all(pall >= 0, pall <= 1) # rest is transition from 0 to 0 state (censoring)

# construct array container
aj_container3 = array(data = c(p$`1`, p$`2`),
                      dim = c(nrow(p$`1`), ncol(p$`1`), 2),
                      dimnames = list(NULL, tt, NULL))
aj_container3

p2 = predict(fit, type = "CIF", newdata = as.matrix(wihs[test_set, c("ageatfda", "idu", "black", "cd4nadir")]))
class(p2)


# Simulate cmprisk data ----
# ONE OBSERVATION PER ROW
set.seed(123)
n = 100  # Number of individuals

# Simulated survival times
time = rexp(n, rate = 0.1)  # Exponential distribution for event times
length(time) # 100 time points unique
# Simulated event types (1 = Risk 1, 2 = Risk 2, 0 = Censored)
event = sample(c(0, 1, 2), size = n, replace = TRUE, prob = c(0.4, 0.3, 0.3))

# Fine-Gray model ----
# WE WILL NOT SUPPORT THIS IN MLR3PROBA AS IT DOESNT RETURN CIFs

# Convert events for cmprsk
# Note: Event 1 = Risk 1, Event 2 = Risk 2
cov = matrix(runif(100), nrow=100) # 100 obs, 1 variable
colnames(cov) = c("x1")
head(cov)

fg_model = cmprsk::crr(ftime = time, fstatus = event, cov1 = cov,
  failcode = 1, cencode = 0, variance = TRUE)
fg_model

# Risk 1 as the event of interest
fg_model$uftime

fg_model$bfitj # jumps in the Breslow-type estimate of the underlying sub-distribution cumulative hazard

# Summary of the model
summary(fg_model)

# predict train set
p = predict(fg_model, cov1 = cov)
class(p)
dim(p)
p

cov_test = matrix(runif(9), nrow=3)
cov_test
p_test = predict(fg_model, cov1 = cov_test)
p_test # no idea what that is!

# cuminc ----
# I don't think this is the AJ estimator...
inc = cmprsk::cuminc(ftime = time, fstatus = event, cencode = 0)
class(inc)
# they split the dataset to different causes??? so maybe that's why
# different event times are used for each CIF??? (NO, NOT TRUE!)

class(inc$`1 1`) # results for risk 1
length(inc$`1 1`$time) # 60 time points
length(inc$`1 1`$est) # same as above

inc$`1 2` # results for risk 2
length(inc$`1 2`$time) # 74 time points
length(inc$`1 2`$est) # same as above, but different time points from risk 1

## TIME POINTS ARE SPLITTED BETWEEN THE CAUSES...
aj_est = mlr3misc::map(inc, `[[`, "est")
aj_times = mlr3misc::map(inc, `[[`, "time")

aj1 = matrix(aj_est[[1]], nrow = 1)
colnames(aj1) = aj_times[[1]]

aj2 = matrix(aj_est[[2]], nrow = 1)
colnames(aj2) = aj_times[[2]]

aj_container = list(aj1, aj2)
abind::abind(aj1, aj2, along = 3) # doesn't work due to diff time points

# how to do prediction in a new set? (as KM in mlr3proba, just put the
# train data predictions to as many test sets as you have)
# expand samples ??? ("everyone is the same curve")

# survfit ----
## what is the following, AJ estimator ??? YES!!!
obj = Surv(time, as.factor(event))
head(obj)
attributes(obj)

obj2 = Surv(time, event, type = "mstate")
head(obj2)
attributes(obj2)

res = survfit(obj ~ 1)
res2 = survfit(obj2 ~ 1)

# same results
all(res$pstate == res2$pstate)

length(res$time) # ALL 100 TIME POINTS ARE THERE, GOOD
dim(res$pstate) # transition probabilities from 0 to (s0),1,2 states, so All-cause-Surv and CIFs
survfit_res = res$pstate
head(survfit_res)
survfit_res_unique = unique(res$pstate)
dup_lgl = duplicated(survfit_res) # to be removed
sum(dup_lgl) # 35
dim(survfit_res_unique) # 65 time points

colnames(survfit_res_unique) = c("0", "1", "2")

# check which time points are left
new_times = res$time[!dup_lgl]
length(new_times)
all.equal(sort(new_times), sort(time[which(event != 0)])) # ONLY THE EVENTS!!!

# probs of states sum up to 1
expect_equal(rowSums(res$pstate), rep(1, 100))

res$states
res$cumhaz # how this is computed?
res$transitions # looks like cmprisks
res$n.risk # looks like cmprisks

# reshape to array
mat = res$pstate
aj_container2 = array(mat, dim = c(1, nrow(mat), ncol(mat)),
                      dimnames = list(NULL, res$time, res$states))
aj_container2 = array(mat, dim = c(1, nrow(mat), ncol(mat)),
                      dimnames = list(NULL, NULL, res$states))
str(aj_container2)

# etm ----
library(etm)

# transform data to (id, from, to, time)
data = cbind.data.frame(
  id = 1:length(time), # obs id
  from = rep(0, length(time)), # from state
  to = event, # to state
  time = time, # time of event or censoring
  entry = rep(0, length(time)) # entry time
)
head(data)
data$trans = paste(data$from, data$to, sep = "->")

# make event a factor with 0 => "cens"
data$to[data$to == 0] = "cens"
data$to = factor(data$to)

tra = matrix(ncol = 3, nrow = 3, FALSE)
tra[1, 2:3] = TRUE
tra

res3 = etm::etm(data = data, state.names = c("0", "1", "2"),
                tra = tra, cens.name = "cens", s = 0)

res3
res3$trans
res3$est # [from-state x to-state x times]
res3$time # 65 time points => event times (from the doc)
sum(event != 0) # 65
all.equal(sort(res3$time), sort(time[which(event != 0)]))

res3$state.names
sumres3 = summary(res3)
mat33 = cbind(sumres3$`0 0`[,"P"], sumres3$`0 1`[,"P"], sumres3$`0 2`[,"P"])
colnames(mat33) = res3$state.names

# same results, nicer interface (for Competing Risks only)
res4 = etm::etmCIF(formula = survival::Surv(entry, time, to != 0) ~ 1,
                   data = data, etype = to, failcode = 1)
res44 = res4[[1]]
res44$time
expect_equal(res44$est, res3$est) # YES, EQUAL
rr = summary(res4)
rr[[1]] # list with CIF tables (time, probs, var, lower, upper, n.risk, n.event)
plot(res44, tr.choice = c("0 1", "0 2"), col = c("red", "blue", "green"))

# make it as the survfit format: [times x to-states]
mat2 = matrix(0, nrow = length(res44$time), ncol = length(res44$state.names))
colnames(mat2) = res44$state.names
for (i in 1:nrow(mat)) {
  mat2[i, ] = res44$est[1, , i]
}
mat2

### ETM AND SURVFIT HAVE THE SAME RESULTS
# compare with survfit_res => YES
expect_equal(survfit_res_unique, mat)
# compare etm vs etmCIF
expect_equal(mat2, mat)
# compare etm vs summary(etm)
expect_equal(mat2, mat33)

# Johannes used this functions for data transfo:
etm:::ci.transfo()

### DOC says: Transitions into a same state, mathematically superfluous, are not allowed. If transitions into the same state are detected in the data, the function will stop

# mstate (CRs) ----
library(mstate)
# See: vignette("Tutorial", package="mstate")
# Competing Risks using mstate package, get AJ estimator
data(aidssi) # => nice competing risks dataset!!!
si <- aidssi
head(si)
?aidssi
table(si$status)

# transition matrix
tmat <- trans.comprisk(K = 2, names = c("event-free", "AIDS", "SI"))
tmat

si$stat1 <- as.numeric(si$status == 1)
si$stat2 <- as.numeric(si$status == 2)
# msprep creates the "complete" data format (with all missing transitions)
silong <- msprep(time = c(NA, "time", "time"), status = c(NA, "stat1", "stat2"),
                 data = si, keep = "ccr5", trans = tmat)
head(silong)
events(silong)

# BUT WE DON'T NEED IT TO GET THE CIFS!
#' `Cuminc` is now simply a WRAPPER AROUND `survfit` of the survival package
#' with `type="mstate"`
?Cuminc
ci = Cuminc(time = si$time, status = si$status)
ci = Cuminc(time = "time", status = "status", data = si) # same

ci$time
ci$CI.1
ci$CI.2
ci$Surv

# mstate (GENERAL) ----
# SEE => https://bblodfon.github.io/surv-courses/mstate.html
library(mstate)
library(tidyverse)

bonemarrow = read.table("https://www.med.uio.no/imb/english/research/centres/ocbe/courses/imb9335/bone-marrow.txt",header=T) %>% as_tibble()
bonemarrow %>% select(T2, DF, TP, P)

tmat = transMat(x = list(c(2,3),c(3),c()),
                names = c("transplanted","recovered","relapsed.dead"))
tmat

# from wide-format => (time, status) different columns per event of interest (status in {0,1})
bonemarrow.long = msprep(
  time = c(NA,"TP","T2"), # 3, no time column for first state, recovery, death times (0 or 1)
  status = c(NA,"P","DF"), # 3, no time column for first state,recovery status, death status (0 or 1)
  data = bonemarrow,
  trans = tmat) # 3 x 3
head(bonemarrow.long) # gives complete format, i.e. all possible transitions from => to for observation i

### EXPLANATION
#' `from`	=> the starting state
#' `to` => the receiving state
#' `trans`	=> the transition number
#' `Tstart` => the starting time of the transition
#' `Tstop`	=> the stopping time of the transition
#' `status` => status variable, with 1 indicating an event (transition), 0 a censoring

trans.comprisk(2) # nice

# numbers of transitions, both in terms of frequencies and percentages:
events(bonemarrow.long)

# Cumhazard per transition
#' We use cox estimation without covariates because this is essentially `Nelson-Aalen`!!!!!)
cox.bm = coxph(Surv(Tstart,Tstop,status) ~ strata(trans),
               data = bonemarrow.long,
               method = "breslow")
#' `msfit` has `newdata` arg => patient-specific transition hazards
haz.bm = msfit(cox.bm, trans = tmat) # cumhaz
haz.bm$trans
haz.bm$Haz
plot(haz.bm, xlab="Days post-transplant", xlim=c(0,1000), ylim=c(0,3.5), use.ggplot = TRUE)

# AJ estimates
#' `probtrans` => patient-specific transition probabilities
#' `predt = 0` => This gives transition probabilities from time `s = 0` to time `t`
prob.bm = probtrans(haz.bm, predt = 0, variance = FALSE)
str(prob.bm)
#' `prob.bm[[i]]` predictions from state `i`
prob.bm[[1]] # most informative maybe, 1 => j state trans. probs
head(prob.bm[[1]]) # transition probabilities from state “transplanted” (state 1)
head(prob.bm[[2]]) # from state 2
head(prob.bm[[3]]) # from state 3

summary(prob.bm, from = 1, times = c(2, 6, 7), variance = FALSE) # constant interpolation
summary(prob.bm, from = 3, variance = FALSE)

# PLOTTING
# stacked probabilities
plot(prob.bm, type = "filled", use.ggplot = TRUE)
# Probability of being in each state (more informative imho)
plot(prob.bm, type = "single", xlim = c(0,1000), col = 1:3, use.ggplot = TRUE)
