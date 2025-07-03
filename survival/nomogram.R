# Example: Cox model nomogram
library(rms)

surv
data(lung, package = "survival")
dd = datadist(survival::lung)
options(datadist = 'dd')

fit = rms::cph(Surv(time, status) ~ age + sex + ph.ecog, data = lung,
                x=TRUE, y=TRUE, surv=TRUE)

# Nomogram: predict 1-year survival
surv1 = Survival(fit)
nom = nomogram(fit, fun = function(x) surv1(365, x), lp=FALSE, funlabel="1-Year Survival Probability")

plot(nom)

# 1,3,5-year survival
nom = nomogram(fit,
  fun = list(function(x) surv1(1*365, x),
             function(x) surv1(2*365, x)),
  funlabel = c(
    "1-Year Survival Probability",
    "2-Year Survival Probability"),
  lp = TRUE
)
nom
plot(nom)

regplot::regplot(fit,
                 observation = survival::lung[2, ],
                 failtime = c(165, 365, 700), title = "",
                 prfail = FALSE, points = TRUE, showP = FALSE, subticks = TRUE
)


# regplot
# Example: Logistic regression nomogram
library(regplot)
library(survival)

fit = coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)

regplot(fit, observation = lung[1,], # type = "survival",
        failtime = 365, prfail = FALSE, droplines = TRUE, points = TRUE,
        showP = TRUE, subticks = FALSE, title = "")

# rmlnomogram => ML nomogram via IML?
# (confusing!) https://htmlpreview.github.io/?https://github.com/herdiantrisufriyana/rmlnomogram/blob/master/doc/ml_nomogram_exemplar.html
library(rmlnomogram)
data(nomogram_features)
data(nomogram_outputs)
create_nomogram(nomogram_features, nomogram_outputs)

data(nomogram_shaps)
create_nomogram(nomogram_features, nomogram_outputs, nomogram_shaps, prob = TRUE)

data(nomogram_features2)
data(nomogram_outputs2)
create_nomogram(nomogram_features2, nomogram_outputs2, prob = TRUE)
