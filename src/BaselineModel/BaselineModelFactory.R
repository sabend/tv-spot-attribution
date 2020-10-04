library("R6")

source("src/BaselineModel/ConstantModel.R")
source("src/BaselineModel/LowessModel.R")
source("src/BaselineModel/WaveletModel.R")
source("src/BaselineModel/FourierModel.R")

BaselineModelFactory <- R6Class("BaselineModelFactory",
  # PRIVATE
  private = list(),

  # PUBLIC
  public = list(
    # FUN: create
    create = function(model_name, model_params, traffic, ad_locations) {
      stopifnot(is.character(model_name), length(model_name) == 1)
      stopifnot(is.list(model_params))
      stopifnot(is.numeric(traffic), length(traffic) > 1)
      stopifnot(is.integer(ad_locations), length(ad_locations) > 1)
      stopifnot(length(traffic) == length(ad_locations))
      
      if (model_name == "ConstantModel")
        return(ConstantModel$new(model_params, traffic, ad_locations))
      if (model_name == "LowessModel")
        return(LowessModel$new(model_params, traffic, ad_locations))
      if (model_name == "WaveletModel")
        return(WaveletModel$new(model_params, traffic, ad_locations))
      if (model_name == "FourierModel")
        return(FourierModel$new(model_params, traffic, ad_locations))

      warning(paste0("Missing implementation for baseline model '", model_name, "'"))

      return(NULL)
    }
  )
)