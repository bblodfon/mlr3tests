# Interval censoring data example
set.seed(42)
time = 1:20
time2 = 2:21
event = sample(x = c(0,1,2,3), size = 20, replace = T)
event

# 0=right censored, 1=event at time, 2=left censored, 3=interval censored
res = survival::Surv(
  time = time,
  time2 = time2,
  event = event, # 4 is not allowed, fails
  type = "interval"
)
res
# + => right-censored, no symbol => event, - => left-censored, [start, end) => interval
res[which(event == 2)] # left-censored
res[which(event == 1)] # events

# transform to representation for interval censored data
data = cbind.data.frame(time, time2, event)

# left-censoring => [-Inf, time]
data[which(event == 2), "time2"] = data[which(event == 2), "time"]
data[which(event == 2), "time"] = -Inf
# events => [time, time]
data[which(event == 1), "time2"] = data[which(event == 1), "time"]
# convert the right-censoring to [time, Inf] values
data[which(event == 0), "time2"] = Inf

res2 = survival::Surv(
  time = data$time,
  time2 = data$time2,
  #event = event, # 'event' is not allowed, fails (so you just define the 2 time columns)
  type = "interval2"
)
res2
attributes(res2)
dimnames(res2)[[2]]

# Multi-state data + interval censoring
res3 = survival::Surv(
  time = time,
  time2 = time2,
  event = event, # causes
  type = "mstate"
)
res3
attributes(res3)
dimnames(res3)[[2]]

# THIS IS competing risks
res4 = survival::Surv(time = time, event = event, type = "mstate")
res4
attributes(res4)

# also cmprisks
res5 = survival::Surv(time = time, event = as.factor(event))
res5
attributes(res4)

# doesn't work
survival::Surv(time = time, event = event)
