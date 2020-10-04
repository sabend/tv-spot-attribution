library("R6")

source("src/Data/AttributionResult.R")
source("src/Data/DateTime.R")
source("src/Data/DataHandler.R")

ResultHandler <- R6Class("ResultHandler",
  # PRIVATE
  private = list(
    .attribution_result = NULL,
    .data_handler = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function(attribution_result, data_handler) {
      stopifnot("AttributionResult" %in% class(attribution_result))
      stopifnot("DataHandler" %in% class(data_handler))
      stopifnot(data_handler$has_field("cost"))

      private$.attribution_result <- attribution_result
      private$.data_handler <- data_handler
    },

    # FUN: cpv
    cpv = function(start_datetime, end_datetime) {
      tab <- self$details(start_datetime, end_datetime)
      return(sum(tab$costs) / sum(tab$visits))
    },

    # FUN: cpv_table
    cpv_table = function(start_datetime, end_datetime) {
      return(self$details(start_datetime, end_datetime)[c("id", "cpv")])
    },

    # FUN: details
    details = function(start_datetime, end_datetime) {
      stopifnot("DateTime" %in% class(start_datetime))
      stopifnot("DateTime" %in% class(end_datetime))
      stopifnot(start_datetime$compare_smaller_equal(end_datetime))
      stopifnot(private$.data_handler$has_field("ad_locations"))

      date_format <- DateTime$date_format()
      time_format <- DateTime$time_format()

      attr_res <- private$.attribution_result$get_result()
      ad_indices <- which(as.integer(private$.data_handler$get_range("ad_locations", start_datetime, end_datetime)) != 0L)
      costs <- private$.data_handler$get_range("cost", start_datetime, end_datetime)[ad_indices]
      date <- as.Date(private$.data_handler$get_range("date", start_datetime, end_datetime), date_format)
      time <- private$.data_handler$get_range("time", start_datetime, end_datetime)
      date_time <- paste(format(date, date_format), time, sep=" ")

      visits <- attr_res$value

      tab <- data.frame(
        id     = date_time[ad_indices],
        visits = as.integer(round(visits, 0)),
        costs  = costs,
        cpv    = round(costs / visits, 2),
        stringsAsFactors = FALSE
      )

      return(tab)
    },

    # FUN: summary
    summary = function(start_datetime, end_datetime) {
      stopifnot("DateTime" %in% class(start_datetime))
      stopifnot("DateTime" %in% class(end_datetime))
      stopifnot(start_datetime$compare_smaller_equal(end_datetime))
      stopifnot(private$.data_handler$has_field("ad_locations"))

      attr_res <- private$.attribution_result$get_result()
      traffic <- private$.data_handler$get_range("traffic", start_datetime, end_datetime)
      ad_indices <- which(as.integer(private$.data_handler$get_range("ad_locations", start_datetime, end_datetime)) != 0L)
      costs <- private$.data_handler$get_range("cost", start_datetime, end_datetime)[ad_indices]

      keys <- c(
        "number ads",
        "visits",
        "attributed visits",
        "attributed fraction",
        "total costs",
        "average CPV"
      )

      total_traffic <- sum(traffic)
      total_attribution <- sum(attr_res$value)
      total_costs <- sum(costs)

      values <- c(
        as.integer(nrow(attr_res)),
        as.integer(total_traffic),
        round(total_attribution, 2),
        round(total_attribution / total_traffic, 2),
        round(total_costs, 2),
        round(total_costs / total_attribution, 2)
      )

      tab <- data.frame(keys, values, stringsAsFactors = FALSE)
      colnames(tab) <- NULL

      return(tab)
    }
  )
)