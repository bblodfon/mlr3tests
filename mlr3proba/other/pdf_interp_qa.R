# RCLL minor extrapolation issue (ie on mlr3proba@3281cb and forward) 
# solution => you need to calculate the time points that S(t) = 0|1. Beyond that, f(t) = 0

# in the right extrapolation region, the eval_times influence the value of pdf!
# see pdf(4) below:
.interp_surv(surv_data = list(surv = c(0.1, 0.05), time = c(1,2)),
             eval_times = c(1, 2, 3, 50))
# [1] 0.10 0.05 0.00 0.00
.interp_pdf(surv_data = list(surv = c(0.1, 0.05), time = c(1,2)),
            eval_times = c(1, 2, 50)) # without the point where S(t) = 0
# [1] 0.050 0.050 0.001041667
.interp_pdf(surv_data = list(surv = c(0.1, 0.05), time = c(1,2)),
            eval_times = c(1, 50)) # without the point where S(t) = 0
# [1] 0.050000000 0.001041667
.interp_pdf(surv_data = list(surv = c(0.1, 0.05), time = c(1,2)),
            eval_times = c(1, 2, 2.9, 50))  # without the point where S(t) = 0
# [1] 0.0500000000 0.0500000000 0.0500000000 0.0001061571
.interp_pdf(surv_data = list(surv = c(0.1, 0.05), time = c(1,2)),
            eval_times = c(1, 2, 3, 50))

# same for left extrapolation
.interp_pdf(surv_data = list(surv = c(1, 0.8, 0.2), time = c(1,2,3)),
            eval_times = c(1,2,3)) # left-align first interval (pdf(1) != 0)?
.interp_pdf(surv_data = list(surv = c(1, 0.8, 0.2), time = c(1,2,3)),
            eval_times = c(0.9,1,2,3)) # this is more correct?

.interp_pdf(surv_data = list(surv = c(1, 0.8, 0.2), time = c(1,2,3)),
            eval_times = c(1, 1.1))
.interp_pdf(surv_data = list(surv = c(1, 0.8, 0.2), time = c(1,2,3)),
            eval_times = c(0.7, 1, 1.1))

.interp_pdf(surv_data = list(surv = c(0.99, 0.8, 0.2), time = c(1,2,3)),
            eval_times = c(1, 1.1))
.interp_pdf(surv_data = list(surv = c(0.99, 0.8, 0.2), time = c(1,2,3)),
            eval_times = c(0.9, 1, 1.1))
