library(mlr3verse)
library(stabm)

# General info ----
listStabilityMeasures()

#' "Adjusted" measures need to have `sim.mat` between features (see Bommert 2020)
#' NO NEED TO USE THESE IF FEATURES ARE FILTERED FOR CORRELATION (keep only < 0.9 Pearson correlation)
listStabilityMeasures() %>% filter(Adjusted == TRUE)
listStabilityMeasures() %>% filter(Adjusted == FALSE)

#' "Corrected" measures have incorporated correction for chance
#' otherwise this package allows you to correct some of these (means more computation though)
listStabilityMeasures() %>% filter(Corrected == TRUE)
listStabilityMeasures() %>% filter(Corrected == FALSE)

# Stability Measures ----
## Yu (2012) ----
#' NEEDs `sim.mat`
listStabilityMeasures() %>% filter(Name == 'stabilityYu')
feats = list(1:3, 1:4, 1:5) # feature list => list of vectors
feats
feats[[1]]
mat = 0.92 ^ abs(outer(1:10, 1:10, "-")) # symmetrical, absolute values (no negatives)
mat
?stabilityYu
stabilityYu(features = feats, correction.for.chance = 'estimate',
  sim.mat = mat, N = 1000)
#' with correction.for.chance = `none` you get crazy stuff

## Zunick (2008) ----
#' NEEDs `sim.mat`
listStabilityMeasures() %>% filter(Name == 'stabilityZucknick')
?stabilityZucknick
stabilityZucknick(features = feats, sim.mat = mat, threshold = 0.9,
  correction.for.chance = 'none')
no.sim.mat = diag(7) # identity
stabilityZucknick(features = feats, sim.mat = no.sim.mat)
#' add correction for chance! => IMPORTANT IF YOU USE THIS MEASURE, see Bommert (2020)
stabilityZucknick(features = feats, sim.mat = mat, threshold = 0.9,
  correction.for.chance = 'estimate', N = 100) # slower

## IntersectionCount (Bommert 2020) ----
#' USE THIS FOR DATASETS WITH CORRELATED FEATURES!!!
listStabilityMeasures() %>% filter(Name == 'stabilityIntersectionCount')
set.seed(1)
stabilityIntersectionCount(features = feats, sim.mat = mat, N = 1000)

## Jaccard (1901) ----
listStabilityMeasures() %>% filter(Name == 'stabilityJaccard')
?stabilityJaccard
feats = list(1:3, 1:4, c(1:3, 5:7))
feats
stabilityJaccard(features = feats) # correction.for.chance = 'none, p is not needed
#' `p` => TOTAL NUMBER OF FEATURES!!!
stabilityJaccard(features = feats, p = 10, N = 1000, correction.for.chance = 'estimate')

## Hamming ----
listStabilityMeasures() %>% filter(Name == 'stabilityHamming')
?stabilityHamming
stabilityHamming(features = feats, p = 10, correction.for.chance = 'estimate', N = 1000)
stabilityHamming(features = feats, p = 10) # default correction.for.chance = `none`, high values in general?

## Nogueira (2018) ----
listStabilityMeasures() %>% filter(Name == 'stabilityNogueira')
?stabilityNogueira
stabilityNogueira(features = feats, p = 7) #' `p` is always needed
stabilityNogueira(features = feats, p = 10) # very fast and corrected for chance!!!
stabilityNogueira(features = feats, p = 10000) # wow, very strict!

# Feature Heatmap ----
plotFeatures(feats)

# Example: Feature Selection ----
#' https://bommert.github.io/stabm/articles/stabm.html#example-feature-selection-1
#' In this example, we will analyze the stability of the feature selection of regression trees on the `BostonHousing2` data set from the `mlbench` package

library(rpart) # for classification trees
data("BostonHousing2", package = "mlbench")

# remove feature that is a version of the target variable
dataset = subset(BostonHousing2, select = -cmedv)
head(dataset)

#' subsample `dataset` to `ratio` of observations, fit regression tree, get
fit_tree = function(target = "medv", data = dataset, ratio = 0.67, cp = 0.01) {
  n = nrow(data)
  i = sample(n, n * ratio) # replace = FALSE, so just subsample
  formula = as.formula(paste(target,  "~ ."))
  model = rpart::rpart(formula = formula, data = data, subset = i,
    control = rpart.control(maxsurrogate = 0, cp = cp))
  names(model$variable.importance)
}

set.seed(1)
fit_tree()

set.seed(1)
selected_features = replicate(30, fit_tree(), simplify = FALSE)
selected_features

# How many features were selected in all sets?
Reduce(intersect, selected_features)

# Sorted selection frequency across all sets:
sort(table(unlist(selected_features)), decreasing = TRUE)

# heatmap
plotFeatures(selected_features)

stabilityJaccard(selected_features)
stabilityJaccard(selected_features, p = ncol(dataset) - 1, correction.for.chance = 'estimate', N = 100)

#' more stable FS with different `cp`
set.seed(1)
selected_features2 = replicate(30, fit_tree(cp = 0.02), simplify = FALSE)
stabilityJaccard(selected_features2)
stabilityJaccard(selected_features2, p = ncol(dataset) - 1, correction.for.chance = 'estimate', N = 100)

plotFeatures(selected_features2)

# Example: highly correlated features ----
dataset2 = subset(BostonHousing2, select = -town)
dataset2$chas = as.numeric(dataset2$chas)
head(dataset2)

set.seed(1)
selected_features3 = replicate(30, fit_tree(target = "rm", data = dataset2, cp = 0.075),
  simplify = FALSE)

plotFeatures(selected_features3)

# no common features amongst all sets
Reduce(intersect, selected_features3)

# similarity matrix (Pearson corr)
sim.mat = abs(cor(subset(dataset2, select = -rm)))
sim.mat
sel.feats = unique(unlist(selected_features3))
sel.feats # subset to (unique) selected features

# medv and cmedv are almost perfectly correlated
sim.mat[sel.feats, sel.feats]

# the choice of medv or cmedv should be seen as equal => adjusted measures take care of that!
plotFeatures(selected_features3, sim.mat = sim.mat)

stabilityIntersectionCount(selected_features3, sim.mat = sim.mat, N = 10)

# with Identity matrix as similarity matrix (all features are different)
no.sim.mat = diag(nrow(sim.mat))
colnames(no.sim.mat) = row.names(no.sim.mat) = colnames(sim.mat)
head(no.sim.mat)
stabilityIntersectionCount(selected_features3, sim.mat = no.sim.mat)
