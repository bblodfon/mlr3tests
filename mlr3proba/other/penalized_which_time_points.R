library(penalized)
#> Loading required package: survival
#> Welcome to penalized. For extended examples, see vignette("penalized").

data("nki70")

times = nki70[,"time"]
status = nki70[,"event"]

indx = status == 1 # event lgl vec

# get unique event times
events = times[status == 1] # only events
events = sort(unique(events))
max(events) # 14.0123203

# add censored subject
id = 26
times[id]
status[id] # 0 => censored
indx[id] = TRUE

# A single lasso fit predicting survival
pen = penalized(Surv(time, event), penalized = nki70[indx,8:77],
                unpenalized = ~ER+Age+Diam+N+Grade, data = nki70[indx,], lambda1 = 10)
#> # nonzero coefficients: 76# nonzero coefficients: 6          # nonzero coefficients: 6
res = penalized::predict(pen, penalized = nki70[indx,8:77])
# check where the time points are stored for prediction
all(res@time == pen@nuisance$baseline@time)

# add 0 time
c(0, events)
# seems to be 1 extra non-event time point included?
res@time

testthat::expect_equal(c(0, events), res@time)


task = tsk("gbcs")
set.seed(42)
part = partition(task, ratio = 0.5)

# keep different time points sets to check later
train_times = task$unique_times(part$train)
train_event_times = task$unique_event_times(part$train)

test_times = task$times(part$test)
test_status = task$status(part$test)
test_event_times = sort(unique(test_times[test_status == 1]))
test_times = sort(unique(test_times))

all_times = task$unique_times()
all_event_times = task$unique_event_times()

l = lrn("surv.penalized")
l$train(task, part$train)
p = l$predict(task, part$test)
times = as.numeric(colnames(p$data$distr))
testthat::expect_equal(times, train_event_times) # has added the largest time point if censored
# 2659 => which time point is it?
train_times = task$times(rows = part$train)
train_status = task$status(rows = part$train)
# find max censored time
cens_times = train_times[train_status == 0]
max(cens_times) # 2659
