library("R6")

AttributionOutput <- R6Class("AttributionOutput",
  # PRIVATE
  private = list(
    .attribution = NULL,
    .baseline = NULL
  ),

  # ACTIVE
  active = list(
    # FIELD: attribution
    attribution = function(value) {
      if (missing(value)) {
        return(private$.attribution)
      } else {
        stopifnot(is.numeric(value), length(value) == 1)
        private$.attribution <- value
      }
    },

    # FIELD: baseline
    baseline = function(value) {
      if (missing(value)) {
        return(private$.baseline)
      } else {
        stopifnot(is.numeric(value))
        private$.baseline <- value
      }
    }
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function() {}
  )
)