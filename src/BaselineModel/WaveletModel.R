source("src/BaselineModel/BaselineModel.R")

library("WaveletComp")

WaveletModel <- R6Class("WaveletModel",
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

      stopifnot(all(WaveletModel$param_names() %in% names(params)))
      
      max_period <- as.integer(params$MAX_PERIOD)
      cut_period <- as.integer(params$CUT_PERIOD)
      
      stopifnot(max_period > 8L)
      stopifnot(cut_period > 8L, cut_period < max_period)

      signal <- log(pmax(traffic, 1))
      mu     <- mean(signal)
      sd     <- sd(signal)
      signal <- (signal - mu) / sd

      wt <- analyze.wavelet(
        my.data     = data.frame(signal),
        my.series   = "signal",
        loess.span  = 0,
        dt          = 1,
        dj          = 1 / 20,
        lowerPeriod = 8,
        upperPeriod = max_period,
        make.pval   = TRUE,
        n.sim       = 100,
        verbose     = FALSE
      )

      smoothed_signal <- reconstruct(
        wt,
        sel.period = wt$Period[wt$Period >= cut_period],
        plot.waves = FALSE,
        lwd        = c(1,2),
        verbose    = FALSE
      )$series$signal.r

      private$.baseline <- exp(mu + sd * smoothed_signal)
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
WaveletModel$.param_names = c("MAX_PERIOD", "CUT_PERIOD")
WaveletModel$param_names = function() {
  return(WaveletModel$.param_names)
}