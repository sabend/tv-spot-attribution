library("R6")

source("src/Data/AttributionOutput.R")

Attributor <- R6Class("Attributor",
  # PRIVATE
  private = list(
    .baseline_model = NULL,
    .params = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function(baseline_model, params) {
      stopifnot("BaselineModel" %in% class(baseline_model))
      stopifnot(is.list(params))
      stopifnot(all(Attributor$param_names() %in% names(params)))

      private$.baseline_model = baseline_model
      private$.params = params
    },

    # FUN: attribute
    attribute = function(ad_index, traffic, ad_locations, store_baseline) {
      max_response_window <- as.integer(private$.params$MAX_RESPONSE_WINDOW)

      baseline <- private$.baseline_model$calculate(as.integer(ad_index), max_response_window)

      ad_window_end <- ifelse(ad_index + max_response_window - 1 > length(traffic), length(traffic), ad_index + max_response_window - 1)
      rel_traffic <- traffic[ad_index:ad_window_end]

      attr <- pmax(rel_traffic - baseline, 0)

      res <- AttributionOutput$new()
      res$attribution <- sum(attr)

      if (store_baseline)
        res$baseline <- baseline

      return(res)
    }
  )
)

# STATIC
Attributor$.param_names = c("MAX_RESPONSE_WINDOW")
Attributor$param_names = function() {
  return(Attributor$.param_names)
}