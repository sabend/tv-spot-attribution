library("R6")
library("chron")

DateTime <- R6Class("DateTime",
  # PRIVATE
  private = list(
    .date = NULL,
    .time = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function(date, time) {
      stopifnot(is.character(date), length(date) == 1)
      stopifnot(is.character(time), length(time) == 1)

      try_date <- as.Date(date, DateTime$.date_format)
      stopifnot(!is.na(try_date))

      if (length(gregexpr(":", time)[[1]]) == 1)
        time <- paste(time, "00", sep=":")

      stopifnot(length(gregexpr(":", time)[[1]]) == 2)
      try_time <- times(time)
      stopifnot("times" %in% class(try_time))

      private$.date = try_date
      private$.time = try_time
    },

    # FUN: date
    date = function() {
      return(private$.date)
    },

    # FUN: time
    time = function() {
      return(private$.time)
    },

    # FUN: to_posixct
    to_posixct = function() {
      date_str <- format(private$.date, DateTime$.date_format)
      time_str <- format(private$.time)

      datetime_str <- paste(date_str, time_str, sep=" ")
      format_str <- paste(DateTime$.date_format, DateTime$.time_format, sep=" ")

      dt <- as.POSIXct(strptime(datetime_str, format_str))

      return(dt)
    },

    # FUN: compare_smaller
    compare_smaller = function(other) {
      stopifnot("DateTime" %in% class(other))
      return((private$.date < other$date()) || (private$.date == other$date() && private$.time < other$time()))
    },

    # FUN: compare_smaller_equal
    compare_smaller_equal = function(other) {
      stopifnot("DateTime" %in% class(other))
      return((private$.date < other$date()) || (private$.date == other$date() && private$.time <= other$time()))
    },

    # FUN: compare_greater
    compare_greater = function(other) {
      stopifnot("DateTime" %in% class(other))
      return((private$.date > other$date()) || (private$.date == other$date() && private$.time > other$time()))
    },

    # FUN: compare_greater_equal
    compare_greater_equal = function(other) {
      stopifnot("DateTime" %in% class(other))
      return((private$.date > other$date()) || (private$.date == other$date() && private$.time >= other$time()))
    },

    # FUN: print
    print = function(...) {
      cat(paste0("DateTime(", private$.date, ",", private$.time, ")\n"))
      invisible(self)
    }
  )
)

# STATIC
DateTime$.date_format = "%d.%m.%Y"
DateTime$.time_format = "%H:%M:%S"

DateTime$date_format = function() {
  return(DateTime$.date_format)
}

DateTime$time_format = function() {
  return(DateTime$.time_format)
}

DateTime$datetime_format = function() {
  return(paste(DateTime$date_format(), DateTime$time_format(), sep=" "))
}


datetime_from_posixct <- function(datetime) {
  date <- format(as.Date(datetime), "%d.%m.%Y")
  time <- strftime(datetime, "%H:%M:%S")

  return(DateTime$new(date, time))
}