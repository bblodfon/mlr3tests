# http://www.sthda.com/english/wiki/survival-analysis-basics
# Visualization of KM curves and log-rank statistics
library(mlr3verse)
library(mlr3proba)
library(survival)
library(survminer)
library(tidyverse)

# Task lung ----
task = tsk('lung')
#task$missings()

pre = po('encode', method = 'treatment') %>>%
  po('imputelearner', lrn('regr.rpart'))
task = pre$train(task)[[1]]
task$missings()

# survfit ----
## create survival table - all below formulas are the same
fit  = survival::survfit(formula = task$formula(1), data = task$data())
fit2 = survminer::surv_fit(formula = task$formula(1), data = task$data()) # best!!!
fit3 = task$kaplan() # has some problems, see below!
eval(fit3$call$data) # the problem! doesn't evaluate the data call

## summary table for KM plot
summary(fit)
## nicer
sum_res = survminer::surv_summary(fit2) %>% as_tibble()
sum_res

## Plot KM curves
# simple
plot(fit3)

# fancy
survminer::ggsurvplot(fit)
survminer::ggsurvplot(fit2)
survminer::ggsurvplot(fit3, data = task$data()) # doesn't work with fit3!

# even fancier
set1_cols = RColorBrewer::brewer.pal(n = 3, name = 'Set1')
survminer::ggsurvplot(fit, # doesn't work with fit3
  # fun = 'cumhaz',
  # 'event' = 1-S(t),
  # 'cumhaz' = 1 - log(S(t)) => number of events that would be expected for each individual by time t
  pval = TRUE, #pval = "The hot p-value is: 0.031",
  pval.method	= TRUE,
  conf.int = TRUE,
  #conf.int.style = "ribbon", # default
  #conf.int.alpha = 0.3, # default
  break.time.by = 100,
  #ncensor.plot = TRUE,
  risk.table = 'percentage', # Add risk table
  #risk.table.col = "strata", # Change risk table color by groups
  #linetype = "strata", # Change line type by groups
  surv.median.line = "hv", # Specify median survival (v: vertical, h:horizontal)
  ggtheme = theme_bw(), # Change ggplot2 theme
  palette = set1_cols,
  xlim = c(0,600)) # shorten the considered time frame to NOT have huge
# confidence intervals due to censoring and thus gain more certainty in
# the plotted curves

## Autoplot ----
?autoplot.TaskSurv
autoplot(task)
autoplot(task, rhs = 'sex')
autoplot(task, type = 'duo')

# Log-rank test / Compare 2 KM curves ----
fit = task$kaplan(strata = 'sex')
plot(fit)
ggsurvplot(fit, data = task$data()) # doesn't work

# No need to use this, it has NO p-value output, just the chi-squared statistic
?survival::survdiff
surv_diff = survival::survdiff(Surv(time, status) ~ sex, data = lung)
surv_diff
# same:
surv_diff2 = survival::survdiff(formula = task$formula('sex'),
  data = task$data())
surv_diff2

fit = survfit(formula = task$formula('sex'), data = task$data())
fit
ggsurvplot(fit, pval = TRUE, pval.method = TRUE)

?survminer::surv_pvalue
surv_pvalue(fit)
