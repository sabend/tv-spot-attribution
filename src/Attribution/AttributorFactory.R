library("R6")

source("src/Attribution/Attributor.R")
source("src/BaselineModel/BaselineModel.R")

AttributorFactory <- R6Class("AttributorFactory",
  # PRIVATE
  private = list(),

  # PUBLIC
  public = list(
    # FUN: create
    create = function(model_name, baseline_model, model_params) {
      stopifnot(is.character(model_name), length(model_name) == 1)
      stopifnot(is.list(model_params))
      stopifnot("BaselineModel" %in% class(baseline_model))

      if (model_name == "Attributor")
        return(Attributor$new(baseline_model, model_params))

      warning(paste0("Missing implementation for attributor '", model_name, "'"))

      return(NULL)
    }
  )
)