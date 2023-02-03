# https://github.com/mlr-org/mlr3/issues/896
library(mlr3)

spam = tsk('spam')
all(spam$truth() == spam$truth(rows = 1:spam$nrow)) # ok

spam$filter(rows = sample(1:spam$nrow, 42))
spam
all(spam$row_ids == spam$row_roles$use) # row_ids == row_roles$use
all(spam$truth() == spam$truth(rows = 1:spam$nrow))
# still okay, as `rows` refers to the whole dataset! (so it literally uses 1 to 42nd row)
# here `truth()` takes from the `spam$row_ids`
spam$row_ids
