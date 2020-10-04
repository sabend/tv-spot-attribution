library("R6")

BaselineModel <- R6Class("BaselineModel",
  # PRIVATE
  private = list(
    .params = NULL,
    .traffic = NULL,
    .ad_locations = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function(params, traffic, ad_locations) {
      stopifnot(is.list(params))
      stopifnot(is.numeric(traffic), length(traffic) > 1)
      stopifnot(is.integer(ad_locations), length(ad_locations) > 1)
      stopifnot(length(traffic) == length(ad_locations))

      private$.params <- params
      private$.traffic <- traffic
      private$.ad_locations <- ad_locations
    },

    # FUN: calculate
    calculate = function(ad_start_index, baseline_window) {
      stopifnot(is.integer(ad_start_index))
      stopifnot(ad_start_index > 0)
      stopifnot(ad_start_index < length(private$.ad_locations))
      stopifnot(is.integer(baseline_window))
      stopifnot(baseline_window > 0)
      stopifnot(baseline_window < length(private$.ad_locations))
    }
  )
)

# STATIC
BaselineModel$param_names = function() {
  stop("missing child class implementation for static 'param_names()'")
}