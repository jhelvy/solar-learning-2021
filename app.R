# Load libraries, functions, and dir paths
source(here::here("code", "0setup.R"))
library(shinythemes)

# Load formatted data
data <- readRDS(dir$data_formatted)

# Load estimated LR models
lr <- readRDS(dir$lr_models)

# Load data frames for historical data
df_us <- data$hist_us
df_china <- data$hist_china
df_germany <- data$hist_germany

# Merge together true historical cost per kW
cost_historical_true <- rbind(
  data$hist_us %>% mutate(country = "U.S."),
  data$hist_china %>% mutate(country = "China"),
  data$hist_germany %>% mutate(country = "Germany")
)

# Load data frames of params from estimated models
params_us <- lr$params_us
params_china <- lr$params_china
params_germany <- lr$params_germany

# Load projection data frames for each country and scenario
df_nat_trends_us <- data$proj_nat_trends_us
df_sus_dev_us <- data$proj_sus_dev_us
df_nat_trends_china <- data$proj_nat_trends_china
df_sus_dev_china <- data$proj_sus_dev_china
df_nat_trends_germany <- data$proj_nat_trends_germany
df_sus_dev_germany <- data$proj_sus_dev_germany

# Set exchange rates
er_us <- 1
er_china <- data$exchangeRatesRMB
er_germany <- data$exchangeRatesEUR

# Compute GLOBAL cost scenarios by country
cost_global_us <- predict_cost(
  params = params_us,
  df = df_us,
  lambda = 0,
  exchange_rate = er_us
)

cost_global_china <- predict_cost(
  params = params_china,
  df = df_china,
  lambda = 0,
  exchange_rate = er_china
)

cost_global_germany <- predict_cost(
  params = params_germany,
  df = df_germany,
  lambda = 0,
  exchange_rate = er_germany
)

# ui ----

ui <- navbarPage(
  title = "",
  theme = shinytheme("united"),
  tabPanel(
    title = "Historical",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "lambda_start",
          label = "lambda (start)",
          min = 0,
          max = 1,
          value = 0
        ),
        sliderInput(
          inputId = "lambda_end",
          label = "lambda (end)",
          min = 0,
          max = 1,
          value = 1
        ),
        sliderInput(
          inputId = "delay",
          label = "Years delay",
          min = 1,
          max = 10,
          value = 10
        ),
        radioButtons(
          inputId = "log_scale",
          label = "Log scale on Y axis?",
          choices = c("Yes", "No"),
          selected = "No"
        )
      ),
      mainPanel(
        plotOutput("cost_historical"),
        plotOutput("savings_historical")
      )
    )
  ),
  tabPanel(
    title = "Projections"
  ),
  tabPanel(
    HTML('About</a></li><li><a href="https://github.com/jhelvy/solar-learning-2021" target="_blank"><i class="fa fa-github fa-fw"></i>'),
    icon = icon(name = "question-circle", lib = "font-awesome"),
    mainPanel(
      width = 6,
      includeMarkdown("README.md")
    )
  )
)

# server ----

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Get variables based on inputs

  log_scale_switch <- reactive({
    if (input$log_scale == "Yes") {
      return(TRUE)
    }
    return(FALSE)
  })

  lambda_nat_hist <- reactive({
    us <- make_lambda_national(
      input$lambda_start, input$lambda_end, input$delay, df_us
    )
    china <- make_lambda_national(
      input$lambda_start, input$lambda_end, input$delay, df_china
    )
    germany <- make_lambda_national(
      input$lambda_start, input$lambda_end, input$delay, df_germany
    )
    return(list(us = us, china = china, germany = germany))
  })

  lambda_nat_proj <- reactive({
    us <- make_lambda_national(
      input$lambda_start, input$lambda_end, input$delay, df_nat_trends_us
    )
    china <- make_lambda_national(
      input$lambda_start, input$lambda_end, input$delay, df_nat_trends_china
    )
    germany <- make_lambda_national(
      input$lambda_start, input$lambda_end, input$delay,
      df_nat_trends_germany
    )
    return(list(us = us, china = china, germany = germany))
  })

  # Compute outcomes

  # Compute NATIONAL cost scenarios by country
  get_costs_hist <- reactive({
    lambda <- lambda_nat_hist()

    cost_national_us <- predict_cost(
      params = params_us,
      df = df_us,
      lambda = lambda$us,
      exchange_rate = er_us
    )

    cost_national_china <- predict_cost(
      params = params_china,
      df = df_china,
      lambda = lambda$china,
      exchange_rate = er_china
    )

    cost_national_germany <- predict_cost(
      params = params_germany,
      df = df_germany,
      lambda = lambda$germany,
      exchange_rate = er_germany
    )

    cost <- combine(
      global_us        = cost_global_us,
      national_us      = cost_national_us,
      global_china     = cost_global_china,
      national_china   = cost_national_china,
      global_germany   = cost_global_germany,
      national_germany = cost_national_germany
    )

    return(cost)
  })

  get_savings_hist <- reactive({
    lambda <- lambda_nat_hist()

    cost_diff_us <- compute_cost_diff(
      params = params_us,
      df = df_us,
      lambda_nat = lambda$us,
      exchange_rate = er_us
    )

    cost_diff_china <- compute_cost_diff(
      params = params_china,
      df = df_china,
      lambda_nat = lambda$china,
      exchange_rate = er_china
    )

    cost_diff_germany <- compute_cost_diff(
      params = params_germany,
      df = df_germany,
      lambda_nat = lambda$germany,
      exchange_rate = er_germany
    )

    cost_diffs <- combine_cost_diffs(
      us = cost_diff_us,
      china = cost_diff_china,
      germany = cost_diff_germany,
      year_min = year_savings_min,
      year_max = year_savings_max
    )

    savings <- compute_savings(cost_diffs, cost_historical_true)

    return(savings)
  })

  # Outputs

  output$cost_historical <- renderPlot(
    make_historical_plot(get_costs_hist(), log_scale_switch()),
    width = 650, height = 400
  )

  output$savings_historical <- renderPlot(
    make_ann_savings_plot(get_savings_hist()),
    width = 650, height = 400
  )
}

# Run the application
shinyApp(ui = ui, server = server)
