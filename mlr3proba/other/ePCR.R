#' Code from: https://cran.r-project.org/web/packages/ePCR/vignettes/ePCR_guide.pdf
library(ePCR)
library(survival)
library(skimr)

# Data ----
data(TYKSSIMU)

# test search cohort
head(xTEXTSIMU)
dim(xTEXTSIMU) # 101 variables (clinical, demographic, lab values, medical history,
# lesion sites, and previous treatments)
skim(xTEXTSIMU)
head(yTEXTSIMU) # survival

# medication curated cohort
head(xMEDISIMU)
dim(xMEDISIMU) # 101 variables (same as above)
skim(xMEDISIMU)
class(yMEDISIMU$surv) # Surv object

# all variables are the same
all(colnames(xTEXTSIMU) == colnames(xMEDISIMU))

# PSP object => Penalized Single Predictors (per cohort object)
# PEP object => Penalized Ensemble Predictors
?`PSP-class`
?`PEP-class`

# Training/Tuning  (PSP) ----

testset = 1:30
# Medication cohort fit

#' Note: `new` construction => trains!!!
psp_medi = new("PSP",
    # Input data matrix x (example data loaded previously)
    # Leaving out patients into a separate test set using negative indices
    x = xMEDISIMU[-testset,],
    # Response vector, 'surv'-object
    y = yMEDISIMU[-testset, "surv"],
    # Seeds for reproducibility
    seeds = c(1,2),
    # If user wishes to run the CV binning multiple times,
    # this is possible by averaging over them for smoother CV heatmap.
    cvrepeat = 2,
    # Using the concordance-index as prediction accuracy in CV
    score = score.cindex,
    # Alpha sequence
    alphaseq = seq(from=0, to=1, length.out=5),
    # Using glmnet's default nlambda of 100
    nlambda = 10,
    # Running the nominal 10-fold cross-validation
    folds = 5,
    # x.expand slot is a function that would allow interaction terms
    # For the sake of the simplicity we will consider identity function
    x.expand = function(x) { as.matrix(x) }
)
psp_medi

# Text search cohort fit
psp_text = new("PSP",
  x = xTEXTSIMU[-testset, ],
  y = yTEXTSIMU[-testset,"surv"],
  seeds = c(3,4),
  cvrepeat = 2,
  score = score.cindex,
  alphaseq = seq(from=0, to=1, length.out=5),
  nlambda = 10,
  folds = 5,
  x.expand = function(x) { as.matrix(x) }
)
psp_text

# Properties ----
psp_medi@optimum
psp_text@optimum

# Methods ----
slotNames(psp_medi)
?`PSP-methods`

?PSP.CSP
plot(psp_medi,
  # Showing only every 10th row and column name (propagated to heatcv-function)
  by.rownames=10, by.colnames=10,
  # Adjust main title and tilt the bias of the color key legend (see ?heatcv)
  main="C-index CV for psp_medi", bias=0.2
)

# PEP ----
pep_tyks = new("PEP",
  # The main input is the list of PSP objects
  PSPs = list(psp_medi, psp_text)
)

# These PSPs were constructed using the example code above.
pep_tyks

pep_tyks@PSPs[[1]]@features
pep_tyks@PSPs[[2]]@features

# Predictions
xtest = rbind(xMEDISIMU[testset,], xTEXTSIMU[testset,])
ytest = rbind(yMEDISIMU[testset,], yTEXTSIMU[testset,])
# Perform survival prediction based on the PEP-ensemble we've created
xpred = predict(pep_tyks, newx=as.matrix(xtest), type="ensemble") # fails
# Construct a survival object using the Surv-package
ytrue = Surv(time = ytest[,"surv"][,"time"], event = ytest[,"surv"][,"status"])
# Test c-index between our constructed ensemble prediction and true response
tyksscore = score.cindex(pred = xpred, real = ytrue)
print(paste("TYKS example c-index:", round(tyksscore, 4)))
# [1] "TYKS example c-index: 0.7463"
