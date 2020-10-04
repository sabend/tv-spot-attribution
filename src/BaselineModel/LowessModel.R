source("src/BaselineModel/BaselineModel.R")

LowessModel <- R6Class("LowessModel",
  # INHERIT
  inherit = BaselineModel,

  # PRIVATE
  private = list(
    .baseline = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize @overwrite
    initialize = function(params, traffic, ad_locations) {
      super$initialize(params, traffic, ad_locations)

      stopifnot(all(LowessModel$param_names() %in% names(params)))

      private$.baseline <- lowess(
        x    = traffic,
        f    = as.numeric(params$FRACTION),
        iter = as.integer(params$ITERATIONS)
      )$y
    },

    # FUN: calculate @overwrite
    calculate = function(ad_start_index, baseline_window) {
      super$calculate(ad_start_index, baseline_window)

      n <- length(private$.traffic)
      end_index <- ad_start_index + baseline_window - 1
      end_index <- ifelse(end_index > n, n, end_index)

      return(private$.baseline[ad_start_index:end_index])
    }
  )
)

# STATIC
LowessModel$.param_names = c("FRACTION", "ITERATIONS")
LowessModel$param_names = function() {
  return(LowessModel$.param_names)
}