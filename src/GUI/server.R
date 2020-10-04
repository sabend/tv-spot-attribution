source("src/Attribution/AttributionEngine.R")
source("src/Attribution/Attributor.R")
source("src/Attribution/AttributorFactory.R")
source("src/BaselineModel/BaselineModelFactory.R")
source("src/Data/ResultHandler.R")
source("src/Plotting/ChartingEngine.R")
source("src/GUI/helpers.R")

# allow a maximum upload size of 35MB
options(shiny.maxRequestSize = 35*1024^2)

# static loads
colours <- read_config_to_list("config/colours.csv")
required_fields <- read_required_fields("config/required_fields.txt")

# server
server <- function(input, output, session) {
  # load data from csv file
  data_handler <- reactive({
    if (is.null(input$data_path_input))
      return(NULL)

    dh <- DataHandler$new(input$data_path_input$datapath)
    stopifnot(dh$has_field(required_fields))

    return(dh)
  })

  # observer for datetime slider
  obs_datetime_slider <- observe({
    dh <- data_handler()

    if (!is.null(dh)) {
      min_datetime <- dh$min_datetime()$to_posixct()
      max_datetime <- dh$max_datetime()$to_posixct()

      updateSliderInput(
        session    = session,
        inputId    = "datetime_range_slider",
        min        = min_datetime,
        max        = max_datetime,
        value      = c(min_datetime, max_datetime)
      )
    }
  })

  # baseline params
  baseline_params <- reactive({
    return(parse_params(input$baseline_model_params))
  })

  # observer for baseline params
  obs_baseline_params <- observe({
    param_values <- paste0(eval(parse(text = paste0(input$baseline_model_type, "$param_names()"))), " = ")
    updateTextAreaInput(session, "baseline_model_params", value = paste(param_values, collapse = "\n"))
  })

  # attribution params
  attribution_params <- reactive({
    return(parse_params(input$attribution_model_params))
  })

  # observer for attributor params
  obs_attributor_params <- observe({
    param_values <- paste0(eval(parse(text = paste0(input$attribution_model_type, "$param_names()"))), " = ")
    updateTextAreaInput(session, "attribution_model_params", value = param_values)
  })

  # plot traffic and baseline for selected period
  traffic_and_baseline <- renderPlot({
    a <- attribution_result#attribute()
    dh <- data_handler()

    if (is.null(a) || is.null(dh))
      return(NULL)

    ce <- ChartingEngine$new(a, dh, colours)

    start_datetime <- datetime_from_posixct(input$datetime_range_slider[1])
    end_datetime <- datetime_from_posixct(input$datetime_range_slider[2])

    return(ggplotly(ce$traffic_and_baseline(start_datetime, end_datetime)))
  })

  # attribution result details table
  attribution_details_table <- renderDataTable({
    a <- attribution_result#attribute()
    dh <- data_handler()

    if (is.null(a) || is.null(dh))
      return(NULL)

    result_handler <- ResultHandler$new(a, dh)

    start_datetime <- datetime_from_posixct(input$datetime_range_slider[1])
    end_datetime <- datetime_from_posixct(input$datetime_range_slider[2])

    cpv_avg <- result_handler$cpv(start_datetime, end_datetime)

    details_table <- result_handler$details(start_datetime, end_datetime)
    details_table = rbind(details_table, c("summary", sum(details_table$visits), sum(details_table$costs), cpv_avg))

    return(details_table)
  })

  # run attribution on demand
  observeEvent(input$run_attribution,
  {
    # gather relevant objects
    dh <- data_handler()
    bp <- baseline_params()
    ap <- attribution_params()

    if (is.null(dh) || is.null(bp) || is.null(ap))
      return(NULL)

    # check params
    bp_check <- check_params(bp)
    ap_check <- check_params(ap)

    if (!bp_check) {
      session$sendCustomMessage(
        type    = "error",
        message = "Misspecified baseline parameters - attribution task cancelled"
      )
      return(NULL)
    }

    if (!ap_check) {
      session$sendCustomMessage(
        type    = "error",
        message = "Misspecified attribution parameters - attribution task cancelled"
      )
      return(NULL)
    }

    start_datetime <- datetime_from_posixct(input$datetime_range_slider[1])
    end_datetime <- datetime_from_posixct(input$datetime_range_slider[2])

    if (sum(dh$get_range("ad_locations", start_datetime, end_datetime)) == 0) {
      session$sendCustomMessage(
        type    = "error",
        message = "There exist no ads in the selected period - attribution task cancelled"
      )
      return(NULL)
    }

    # attribution start timestamp
    attribution_start <- Sys.time()

    # build model
    bm <- BaselineModelFactory$new()$create(
      model_name   = input$baseline_model_type,
      model_params = bp,
      traffic      = dh$get_range("traffic", start_datetime, end_datetime),
      ad_locations = dh$get_range("ad_locations", start_datetime, end_datetime)
    )

    # build attributor
    a <- AttributorFactory$new()$create(
      model_name     = input$attribution_model_type,
      baseline_model = bm,
      model_params   = ap
    )

    # run attribution
    attribution_engine <- AttributionEngine$new(a, dh)
    attribution_result <- attribution_engine$run(start_datetime, end_datetime)

    # attribution start timestamp
    attribution_end <- Sys.time()
    attribution_duration <- round(as.numeric(attribution_end - attribution_start, units = "secs"), 2)

    # create plot
    ce <- ChartingEngine$new(attribution_result, dh, colours)
    result_plot <- ce$traffic_and_baseline(start_datetime, end_datetime)

    # create table
    result_handler <- ResultHandler$new(attribution_result, dh)

    cpv_avg <- result_handler$cpv(start_datetime, end_datetime)
    result_table <- result_handler$details(start_datetime, end_datetime)
    summary_table <- result_handler$summary(start_datetime, end_datetime)
    summary_table <- rbind(summary_table, c("run time (s)", attribution_duration))

    # pass results
    output$traffic_and_baseline <- renderPlotly(result_plot)
    output$attribution_details_table <- renderTable(result_table)
    output$attribution_summary_table <- renderTable(summary_table)
  })
}