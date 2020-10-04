library("R6")
library("htmlwidgets")
library("plotly")

source("src/Data/AttributionResult.R")
source("src/Data/DateTime.R")
source("src/Data/DataHandler.R")

ChartingEngine <- R6Class("ChartingEngine",
  # PRIVATE
  private = list(
    .attribution_result = NULL,
    .data_handler = NULL,
    .colours = NULL
  ),

  # PUBLIC
  public = list(
    # FUN: initialize
    initialize = function(attribution_result, data_handler, colours) {
      stopifnot("AttributionResult" %in% class(attribution_result))
      stopifnot("DataHandler" %in% class(data_handler))
      stopifnot(is.list(colours))
      stopifnot("AD" %in% names(colours))
      stopifnot("AD_MARKER" %in% names(colours))
      stopifnot("BASELINE" %in% names(colours))
      stopifnot("TRAFFIC" %in% names(colours))
      stopifnot(data_handler$has_field("ad_locations"))
      stopifnot(data_handler$has_field("traffic"))

      private$.attribution_result <- attribution_result
      private$.data_handler <- data_handler
      private$.colours <- colours
    },

    # FUN: calculate
    traffic_and_baseline = function(start_datetime, end_datetime) {
      stopifnot("DateTime" %in% class(start_datetime))
      stopifnot("DateTime" %in% class(end_datetime))
      stopifnot(start_datetime$compare_smaller_equal(end_datetime))

      date_format <- DateTime$date_format()
      time_format <- DateTime$time_format()

      date <- as.Date(private$.data_handler$get_range("date", start_datetime, end_datetime), date_format)
      time <- private$.data_handler$get_range("time", start_datetime, end_datetime)
      date_time <- as.POSIXct(paste(format(date, date_format), time, sep=" "), format = paste(date_format, time_format, sep=""))

      traffic <- private$.data_handler$get_range("traffic", start_datetime, end_datetime)
      max_traffic <- max(traffic)

      ad_indices <- which(as.integer(private$.data_handler$get_range("ad_locations", start_datetime, end_datetime)) != 0L)
      baselines <- private$.attribution_result$get_baseline()

      ad_lines <- list()
      for (i in ad_indices) {
        x_ad <- date_time[i]
        line = list(
          x0        = x_ad,
          x1        = x_ad,
          y0        = 0,
          y1        = max_traffic,
          layer     = "below",
          line      = list(color = private$.colours$AD),
          text      = "AD",
          hoverinfo = "text"
        )
        ad_lines <- c(ad_lines, list(line))
      }

      p <- plot_ly() %>%
        add_trace(
          x         = date_time,
          y         = traffic,
          type      = "scatter",
          mode      = "lines",
          name      = "TRAFFIC",
          line      = list(color = private$.colours$TRAFFIC,
                           shape = "spline")
        ) %>%
        layout(
          shapes = ad_lines#,
          # xaxis  = list(showspikes     = TRUE,
          #               spikethickness = 2),
          # yaxis  = list(showspikes     = TRUE,
          #               spikethickness = 2)
        )

      show_legend <- TRUE

      for (i in ad_indices) {
        baseline <- baselines[[as.character(i)]]
        x_ad <- date_time[i]

        p <- add_trace(
          p,
          x          = date_time[i - 1 + seq_along(baseline)],
          y          = baseline,
          type       = "scatter",
          mode       = "lines",
          name       = "BASELINE",
          line       = list(color = private$.colours$BASELINE),
          showlegend = show_legend
        )

        p <- add_markers(
          p,
          x          = x_ad,
          y          = baseline[1],
          type       = "scatter",
          mode       = "markers",
          name       = "AD",
          marker     = list(color  = private$.colours$AD_MARKER,
                            size   = 10,
                            symbol = "circle"),
          showlegend = show_legend
        )

        show_legend = FALSE
      }

      js_disable_legend_click <- "var p = document.getElementsByClassName('plotly')[0];
                                  p('plotly_legendclick', function(d, i) { return false; });"
      js_disable_legend_double_click <- "var p2 = document.getElementsByClassName('plotly')[0];
                                         p2('plotly_legenddoubleclick', function(d, i) { return false; });"
      p <- htmlwidgets::prependContent(p, htmlwidgets::onStaticRenderComplete(js_disable_legend_click), data=list(""))
      p <- htmlwidgets::prependContent(p, htmlwidgets::onStaticRenderComplete(js_disable_legend_double_click), data=list(""))

      return(p)
    }
  )
)