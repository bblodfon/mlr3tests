#'###############
# CoxNet glmnet # (package example)
#'###############
library(glmnet)
library(survival)

# Data ----
data(CoxExample)
x = CoxExample$x
colnames(x) = paste0('x', 1:30) # 30 covariates
x[1:5, 1:5]
dim(x) # (1000,30)
y = CoxExample$y
y[1:5, ] # status 1 => dead (---SOS---), time is not days I think!!!
y_mat = y

# max time ~52
max(y_mat[,"time"])

# but better to survival::Surv() objects!
# some things don't work unless it's in this format
y = survival::Surv(time = y[,1], event = y[,2])

# Fit ----
# standardize = TRUE (default)
# dfmax = nvars + 1 => limit the max number of variables in the model

fit = glmnet(x, y, family = "cox", lambda = 0.001)
# plot(fit)
# 49 lambdas tried (out of max 100 `nlambda`)
length(fit$lambda)
# Extract coefficients for lambda == s in a tidy manner
coef_tbl = coef(fit, s = 0.07752803) %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = 'coef_name') %>%
  dplyr::rename(value = `1`)
plot(coef_tbl$value, ylab = 'Coefficient Value')

# Predict Survival ----
# Standard glmnet() function "tries" many lambdas but does no CV: you can select whichever you want
lambda = 0.07752803
# linear predictors: b*x(new)
linear_predictors = predict(fit, newx = x[1:5,], s = lambda, type = "link") # DEFAULT
# risk: exp(b*x(new))
risks = predict(fit, newx = x[1:5,], s = lambda, type = "response")
# SAME AS ABOVE with coef() => THE COEFFFICIENTS
pred_coefs = predict(fit, newx = x[1:5,], s = lambda, type = "coefficients") %>% as.matrix()
all(coef_tbl$value == pred_coefs[,1])

# using the mean of the covariates, DOESN'T MAKE SENSE
res = survival::survfit(fit, s = lambda, x = x, y = y)
plot(res)
# Next doesn't work :(
survminer::ggsurvplot(res, data = x, conf.int = F, ggtheme = ggplot2::theme_minimal())

# Get survival curves (same time points as training data)
fit_res = survival::survfit(fit, s = lambda, x = x, y = y, newx = x[1:5, ])
survminer::ggsurvplot(fit_res, data = x)

# Predict Survival (specific times) ----
times = c(1, 4, 10, 15, 20, 50, 55, 70, 100)
obj = summary(fit_res, times = times, extend = TRUE)
dplyr::bind_cols(time = obj$time, obj$surv)

# Prediction performance ----
# https://cran.r-project.org/web/packages/survival/vignettes/concordance.pdf (read 1st page)
glmnet::Cindex(linear_predictors, y[1:5])
# better (get se as well)
f = -linear_predictors # f = -risks (order matters and risks = exp(linear_predictors))
c_index = survival::concordance(y[1:5] ~ f) # or: (y ~ f, reverse = TRUE)

c_index$concordance
sqrt(c_index$var) #std

#' NOTE: For a Cox model the predicted survival y is longer if the risk score Xβ
#' is lower, so we have to flip the definitions of concordant and discordant
#' With survival output (on a specific time point) we don't need to change sign:
concordance(y[1:5] ~ obj$surv[2,])

# Cross Validation ----
set.seed(42)
# type.measure="deviance" uses partial-likelihood for the Cox model
# type.measure="C" uses the C-index
cvfit = cv.glmnet(x, y, family = "cox", type.measure = "C", nfolds = 10)

plot(cvfit)
cvfit$lambda.1se
log(cvfit$lambda.min)
log(cvfit$lambda.1se) # better if I recall (more coefficients zeroed)

# extract coef
coef_tbl = coef(cvfit, s = "lambda.1se") %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = 'coef_name') %>%
  dplyr::rename(value = `1`)
# get number of non-zero coefs
cvfit$nzero[cvfit$index["1se",]]

fit_res = survival::survfit(cvfit, s = "lambda.1se", x = x, y = y, newx = x[1:5, ])
survminer::ggsurvplot(fit_res, data = x)

obj = summary(fit_res, times = times, extend = TRUE)
res = dplyr::bind_cols(time = obj$time, obj$surv)
res
concordance(y[1:5] ~ obj$surv[2,]) # at a specific timepoint? NO!!!! Better to use linear predictors as they don't change and C-index is time-indepedent!!!
