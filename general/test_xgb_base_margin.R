# most of these have been addressed in mlr3learners xgboost tests
# see also discussion here => https://github.com/dmlc/xgboost/issues/11872
library(xgboost)
library(testthat)

# Load data, split train/test
data(agaricus.train, package = "xgboost")
X = agaricus.train$data
y = agaricus.train$label

set.seed(42)
train_ids = sample(x = 1:nrow(X), size = 4000)
test_ids = setdiff(1:nrow(X), train_ids)

xgb_data_train = xgboost::xgb.DMatrix(data = X[train_ids, ], label = y[train_ids])
xgb_data_test  = xgboost::xgb.DMatrix(data = X[test_ids, ], label = y[test_ids])

# m1 model with no base_margin (default base_score = 0.5)
m1 = xgboost::xgb.train(
  data = xgb_data_train,
  params = list(base_score = 0.5, objective = "binary:logistic"),
  nrounds = 5)
xgboost::setinfo(xgb_data_train, "base_margin", rep(0, length(train_ids)))
m3 = xgboost::xgb.train(
  data = xgb_data_train,
  # base_score doesn't matter for training, but yes for prediction (estimated base score is used if no base_margin)
  params = list(objective = "binary:logistic"),
  nrounds = 5)
testthat::expect_equal(xgb.dump(m1), xgb.dump(m3)) # same models

p1 = predict(m1, newdata = xgb_data_test)
p3 = predict(m3, newdata = xgb_data_test)
head(p1-p3)

# use the simple matrix interface for prediction
test_data = mlr3extralearners:::as_numeric_matrix(X[test_ids, ])
p1 = predict(m1, newdata = test_data)
p3 = predict(m3, newdata = test_data)
head(p1-p3)

# can we force base_margin = 0 for both models?
bm = rep(0, length(test_ids))
p11 = predict(m1, newdata = test_data, base_margin = bm)
p33 = predict(m3, newdata = test_data, base_margin = bm)
expect_equal(p11, p33) # OKAY!!!

bm1 = rep(0.5, length(test_ids))
p111 = predict(m1, newdata = test_data, base_margin = bm1)

# force base_margin = 0 for the test set via xgb.DMatrix, is it used?
xgboost::setinfo(xgb_data_test, "base_margin", rep(0, length(test_ids)))
xgb_data_test
p1 = predict(m1, newdata = xgb_data_test)
p3 = predict(m3, newdata = xgb_data_test)
head(p1-p3) # still something is different here! ISSUE => xgb.DMatrix + base_margin is not understood properly by the predict function, normal matrix + base_margin works

# different base_margins from the same model, but same predictions ???
xgboost::setinfo(xgb_data_test, "base_margin", rep(0.5, length(test_ids)))
p11 = predict(m1, newdata = xgb_data_test)
xgboost::setinfo(xgb_data_test, "base_margin", rep(1.5, length(test_ids)))
p111 = predict(m1, newdata = xgb_data_test)
expect_equal(p1, p11) ## ISSUE
expect_equal(p11, p111) ## ISSUE
