# debugging learner

task = tsk("mtcars")
# Create train and test set
# check task's features
task$feature_names
# partition features to 2 blocks
blocks = list(bl1 = 1:3, bl2 = 4:10)
# define learner
learner = lrn("regr.blockforest", blocks = blocks,
              importance = "permutation", nsets = 10,
              num.trees = 50, num.trees.pre = 10, splitrule = "variance")
learner$encapsulate("mirai", fallback = lrn("regr.featureless"))


# Train the learner on the training ids
learner$train(task)
learner$encapsulate("none")
debugonce(learner$.__enclos_env__$private$.predict) # to seee what is going wrong as the model is now "broken"
learner$predict(task)
learner$model



# Add marshaling to a learner:

- add public fields:

#' @description
    #' Marshal the learner's model.
    #' @param ... (any)\cr
    #'   Additional arguments passed to [`mlr3::marshal_model()`][mlr3::marshaling()].
    marshal = function(...) {
      learner_marshal(.learner = self, ...)
    },

    #' @description
    #' Unmarshal the learner's model.
    #' @param ... (any)\cr
    #'   Additional arguments passed to [`mlr3::unmarshal_model()`][mlr3::marshaling()].
    unmarshal = function(...) {
      learner_unmarshal(.learner = self, ...)
    }

- add active field: 

  active = list(
    #' @field marshaled (`logical(1)`)\cr
    #' Whether the learner has been marshaled.
    marshaled = function() {
      learner_marshaled(self)
    }
  ),

- add S3 methods beneath the learner:

#' @export
marshal_model.regr_blockforest_model = function(model, inplace = FALSE, ...) {
  tmp = tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  # Save the model to a temporary file and read it back as raw
  saveRDS(model, tmp)
  raw_model = readBin(tmp, what = "raw", n = file.info(tmp)$size)

  structure(list(
    marshaled = raw_model,
    packages = c("mlr3extralearners", "blockForest")
  ), class = c("regr_blockforest_model_marshaled", "marshaled"))
}

#' @export
unmarshal_model.regr_blockforest_model_marshaled = function(model, ...) {
  tmp = tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  # Write raw bytes to a temp file and read back the model
  writeBin(model$marshaled, tmp)
  restored_model = readRDS(tmp)

   # Sanity check: make sure the object has the right class
  if (!inherits(restored_model, "regr_blockforest_model")) {
    stopf("Unmarshaled object is not of class 'regr_blockforest_model' (got: %s)",
          paste(class(restored_model), collapse = ", "))
  }

  #browser()
  # structure(list(
  #   model = restored_model$model
  # ), class = "regr_blockforest_model")
  restored_model
}



