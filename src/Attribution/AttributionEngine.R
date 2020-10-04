library("R6")

source("src/Attribution/Attributor.R")
source("src/Data/AttributionOutput.R")
source("src/Data/DateTime.R")
source("src/Data/DataHandler.R")

AttributionEngine <- R6Class("AttributionEngine",
  # PRIVATE
  private = list(
    .attributor = NULL,
    .data_handler = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function(attributor, data_handler) {
      stopifnot("Attributor" %in% class(attributor))
      stopifnot("DataHandler" %in% class(data_handler))

      private$.attributor = attributor
      private$.data_handler = data_handler
    },

    # FUN: run
    run = function(start_datetime, end_datetime) {
      stopifnot("DateTime" %in% class(start_datetime))
      stopifnot("DateTime" %in% class(end_datetime))
      stopifnot(start_datetime$compare_smaller_equal(end_datetime))
      stopifnot(private$.data_handler$has_field("ad_locations"))
      stopifnot(private$.data_handler$has_field("traffic"))

      ad_locations <- private$.data_handler$get_range("ad_locations", start_datetime, end_datetime)
      traffic <- private$.data_handler$get_range("traffic", start_datetime, end_datetime)

      ad_ids <- which(ad_locations != 0L)
      attr_result <- AttributionResult$new(as.character(ad_ids))

      for (i in ad_ids) {
        #if (ad_locations[i] != 0L) next

        ad_res <- private$.attributor$attribute(i, traffic, ad_locations, TRUE)
        attr_result$insert(as.character(i), ad_res)
      }

      return(attr_result)
    }
  )
)