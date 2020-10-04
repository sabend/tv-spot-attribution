library("R6")
library("tools")

source("src/Data/DateTime.R")

DataHandler <- R6Class("DataHandler",
  # PRIVATE
  private = list(
    .data = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function(file_path) {
      stopifnot(is.character(file_path), length(file_path) == 1)
      stopifnot(file_ext(file_path) == "csv")
      data <- read.csv(file_path, stringsAsFactors = FALSE)
      stopifnot("date" %in% colnames(data))
      stopifnot("time" %in% colnames(data))

      private$.data <- data
    },

    # FUN: fields
    fields = function() {
      return(colnames(private$.data))
    },

    # FUN: get
    get = function(name) {
      stopifnot(name %in% colnames(private$.data))
      return(private$.data[[name]])
    },

    # FUN: get_range
    get_range = function(name, from, to) {
      stopifnot(name %in% colnames(private$.data))
      stopifnot("DateTime" %in% class(from))
      stopifnot("DateTime" %in% class(to))
      stopifnot(from$compare_smaller(to))

      date <- as.character(private$.data[["date"]])
      time <- as.character(private$.data[["time"]])

      n <- length(date)
      first_datetime <- DateTime$new(date[1], time[1])
      last_datetime <- DateTime$new(date[n], time[n])

      stopifnot(from$compare_greater_equal(first_datetime))
      stopifnot(to$compare_smaller_equal(last_datetime))

      date <- as.Date(date, DateTime$date_format())
      time <- times(time)

      full_index_set <- seq_along(date)

      indices <- which(from$date() <= date)
      indices <- intersect(indices, which(to$date() >= date))
      indices <- intersect(indices, seq(indices[min(which(from$time() <= time[indices]))], n))
      indices <- intersect(indices, seq(1, indices[max(which(to$time() >= time[indices]))]))

      res <- private$.data[[name]][indices]

      return(res)
    },

    # FUN: has_field
    has_field = function(name) {
      return(all(name %in% colnames(private$.data)))
    },

    # FUN: min_datetime
    min_datetime = function() {
      return(DateTime$new(private$.data$date[1], private$.data$time[1]))
    },

    # FUN: max_datetime
    max_datetime = function() {
      n <- nrow(private$.data)
      return(DateTime$new(private$.data$date[n], private$.data$time[n]))
    }
  )
)