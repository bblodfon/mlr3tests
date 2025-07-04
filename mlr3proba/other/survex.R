# use `survex` with `mlr3proba`
# see also articles on https://modeloriented.github.io/survex/
library(mlr3proba)
library(mlr3extralearners)
library(mlr3pipelines)
library(survex)
library(survival)
library(ranger)

# mlr3proba setup
task = as_task_surv(veteran, time = "time", event = "status")
# doesn't take tsk("veteran") because time is an `integer`!
learner = lrn("surv.ranger")
learner$train(task)
explainer = explain(learner,
                    # can create explainer for different data, eg test dataset!
                    data = task$data(cols = task$feature_names),
                    y = task$truth(),
                    label = "Ranger")

# GLOBAL explanation ----
## Performance Evaluation ----
loss_rcll = loss_adapt_mlr3proba(msr("surv.rcll"))
loss_intlogloss = loss_adapt_mlr3proba(msr("surv.intlogloss"))
custom_metrics = c(
  "Integrated Log Likelihood" = loss_intlogloss,
  "Right-Censored Log Likelihood" = loss_rcll
)

perf = model_performance(explainer)
perf1 = model_performance(explainer, metrics = custom_metrics)
# ?survex:::plot.model_performance_survival()
plot(perf1, metrics_type = "scalar") # only this works?
plot(perf) # BS(t) and AUC(t)

## PDP/ALE plots ----
mp = model_profile(explainer, type = "partial")
mpa = model_profile(explainer, type = "accumulated") # N => obs used for calculation
head(mp$result) # lots of data

plot(mp, variables = "celltype") # PDP
plot(mpa, variables = "celltype") # ALE
# contours is interesting, though I think binning cont. variables might be better
plot(mpa, variables = "age", numerical_plot_type = "contours")

## Permutation Feature Importance ----
## if type = "ratio" the results are in the form loss/loss_full_model
## since I mess up the X ~ Y association with the feature permutation
## I expect loss > loss_full_model
parts = explainer |> model_parts(type = "ratio") # karno more important for earlier times!
plot(parts)

## SurvSHAP(t) ----
newdata = task$data(rows = 1:20, cols = task$feature_names)
# newdata can be the whole dataset here
mshap = model_survshap(explainer, new_observation = newdata)
plot(mshap) # geom = "importance", => importance of variables over time and aggregated
plot(mshap, geom = "beeswarm") # distribution of SurvSHAP(t) values for variables and observations (dots are obs)

# LOCAL explanation ----
## Ceteris Paribus plot ----
## like a PDP plot for one observation!!!

# veteran[1,]
# trt celltype time status karno diagtime age prior
# 1 squamous   72      1    60        7  69     0
explainer |>
  # Ceteris Paribus Profiles for a specific observation with the
  # possibility to take the time dimension into account.
  predict_profile(new_observation = task$data(rows = 1, cols = task$feature_names)) |>
  plot(numerical_plot_type = "contours",
       variables = c("karno", "celltype"), # dashed and red line are the values of the obs!
       facet_ncol = 1, subtitle = NULL)

## SurvSHAP(t) ----
# Positive SurvSHAP(t) values indicate that a given variable has increased the
# survival function by that much, while negative values indicate a decrease
newdata = task$data(rows = 1, cols = task$feature_names)
shap_res = predict_parts(explainer, new_observation = newdata) # type = "survshap" => default
plot(shap_res)
# ?survex:::plot.surv_shap()

# SurvLIME is faster as it uses surrogate Cox model, but less good in general according to SurvSHAP paper
lime_res = predict_parts(explainer, new_observation = newdata, type = "survlime")
plot(lime_res, type = "local_importance")
# if right plot shows divergence, left plot is not to be trusted :)

plot(lime_res, type = "coefficients")
