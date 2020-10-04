library(shiny)

# global params
init_datetime <- Sys.Date()

# static loads
baseline_model_choices <- read_config_to_vector("config/baseline_models.csv")
attributor_choices <- read_config_to_vector("config/attributors.csv")

# ui
ui <- fluidPage(
  titlePanel("Spot Attribution Sandbox"),

  singleton(
    tags$head(tags$script(src = "message-handler.js"))
  ),

  sidebarLayout(

    # SIDEBAR
    sidebarPanel(
      # text input for path to csv
      fileInput(
        inputId = "data_path_input",
        label   = h4("Path to data file"),
        accept  = c(".csv")
      ),

      # datetime range slider
      sliderInput(
        inputId = "datetime_range_slider",
        label   = h4(""),
        min     = init_datetime,
        max     = init_datetime,
        value   = c(
          init_datetime,
          init_datetime
        ),
        timeFormat = "%Y-%m-%d",
        timezone   = "Etc/GMT+1"
      ),

      # drop-down to select baseline model
      selectInput(
        inputId = "baseline_model_type",
        label   = h4("Baseline Model Type"),
        choices = baseline_model_choices
      ),

      # input area for baseline model parameters
      textAreaInput(
        inputId = "baseline_model_params",
        label   = h4("Baseline Model Parameters"),
        value   = "",
        resize  = "vertical",
        rows    = 3
      ),

      # drop-down to select attribution model
      selectInput(
        inputId = "attribution_model_type",
        label   = h4("Attribution Model Type"),
        choices = attributor_choices
      ),

      # input area for baseline model parameters
      textAreaInput(
        inputId = "attribution_model_params",
        label   = h4("Attribution Model Parameters"),
        value   = "",
        resize  = "vertical",
        rows    = 3
      ),
      
      # run attribution button
      actionButton(
        inputId = "run_attribution",
        label   = h4("Run Attribution")
      ),

      # CSS tags
      tags$head(
        tags$style(HTML("#run_attribution{ color: #FF0033 }"))
      )
    ),

    # MAIN PANEL
    mainPanel(
      # plot traffic and baseline for selected period
      plotlyOutput(outputId = "traffic_and_baseline"),

      br(),

      fluidRow(
        # attribution result details table
        column(
          6,
          h4("Attribution Details"),
          tableOutput(outputId = "attribution_details_table")
        ),
        column(
          6,
          h4("Attribution Summary"),
          tableOutput(outputId = "attribution_summary_table")
        )
      )
    )

  )
)