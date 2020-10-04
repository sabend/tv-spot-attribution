source("src/BaselineModel/BaselineModel.R")

ConstantModel <- R6Class("ConstantModel",
  # INHERIT
  inherit = BaselineModel,

  # PRIVATE
  private = list(),

  # PUBLIC
  public = list(
    # FUN: initialize @overwrite
    initialize = function(params, traffic, ad_locations) {
      super$initialize(params, traffic, ad_locations)

      stopifnot(all(ConstantModel$param_names() %in% names(params)))
    },

    # FUN: calculate @overwrite
    calculate = function(ad_start_index, baseline_window) {
      super$calculate(ad_start_index, baseline_window)

      lb_window <- as.integer(private$.params$LOOKBACK_WINDOW)
      spot_chains <- as.logical(private$.params$HANDLE_SPOT_CHAINS)
      traffic <- private$.traffic

      # calculate average of traffic preceeding ad start
      if (spot_chains) {
        ad_ids <- which(private$.ad_locations != 0L)

        leading_ad_ids <- ad_ids[which(ad_ids <= ad_start_index)]
        
        if (length(leading_ad_ids) > 1) {
          delta <- as.integer(private$.params$SPOT_CHAIN_MAX_DELTA)
          rev_ad_deltas <- rev(diff(leading_ad_ids))

          chain_start_index <- Position(function(index) index >= delta, rev_ad_deltas)
          chain_start_index <- ifelse(is.na(chain_start_index), 1, chain_start_index)

          ad_start_index <- rev(leading_ad_ids)[chain_start_index]
        }
      }

      avg_window_start <- ifelse(ad_start_index > lb_window, ad_start_index - lb_window, 1)
      baseline_level <- mean(traffic[avg_window_start:(ad_start_index - 1)])

      # extrapolate contant baseline
      baseline <- rep(baseline_level, baseline_window)

      return(baseline)
    }
  )
)

# STATIC
ConstantModel$.param_names = c("LOOKBACK_WINDOW", "HANDLE_SPOT_CHAINS", "SPOT_CHAIN_MAX_DELTA")
ConstantModel$param_names = function() {
  return(ConstantModel$.param_names)
}