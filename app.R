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
er_china_proj <- data$exchangeRatesRMB %>%
    filter(year == year_proj_min) %>%
    pull(average_of_rate)
er_germany_proj <- data$exchangeRatesEUR %>%
    filter(year == year_proj_min) %>%
    pull(average_of_rate)

# Compute GLOBAL historical cost scenarios by country

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

# Compute GLOBAL projection scenarios by country & scenario ----

proj_nat_trends_global_us <- predict_cost(
    params = params_us,
    df     = df_nat_trends_us,
    lambda = 0,
    exchange_rate = er_us)

proj_sus_dev_global_us <- predict_cost(
    params = params_us,
    df     = df_sus_dev_us,
    lambda = 0,
    exchange_rate = er_us)

proj_nat_trends_global_china <- predict_cost(
    params = params_china,
    df     = df_nat_trends_china,
    lambda = 0,
    exchange_rate = er_china_proj)

proj_sus_dev_global_china <- predict_cost(
    params = params_china,
    df     = df_sus_dev_china,
    lambda = 0,
    exchange_rate = er_china_proj)

proj_nat_trends_global_germany <- predict_cost(
    params = params_germany,
    df     = df_nat_trends_germany,
    lambda = 0,
    exchange_rate = er_germany_proj)

proj_sus_dev_global_germany <- predict_cost(
    params = params_germany,
    df     = df_sus_dev_germany,
    lambda = 0,
    exchange_rate = er_germany_proj)

# ui ----

ui <- navbarPage(
  title = "",
  theme = shinytheme("united"),
  tabPanel(
    title = "About",
    icon = icon(name = "question-circle", lib = "font-awesome", verify_fa = FALSE)
  ),
  tabPanel(
    title = "Historical",
    icon = icon(name = "rotate-left", lib = "font-awesome", verify_fa = FALSE),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        sliderInput(
          inputId = "lambda_start_hist",
          label = "lambda (start)",
          min = 0,
          max = 1,
          value = 0
        ),
        sliderInput(
          inputId = "lambda_end_hist",
          label = "lambda (end)",
          min = 0,
          max = 1,
          value = 1
        ),
        sliderInput(
          inputId = "delay_hist",
          label = "Years delay",
          min = 1,
          max = 10,
          value = 10
        ),
        radioButtons(
          inputId = "log_scale_hist",
          label = "Log scale on Y axis?",
          choices = c("Yes", "No"),
          selected = "No"
        )
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = "Cost Curve", 
            plotOutput("cost_hist")
          ),
          tabPanel(
            title = "Savings", 
            plotOutput("savings_hist")
          )
        )
      )
    )
  ),
  tabPanel(
    HTML('Projections</a></li><li><a href="https://github.com/jhelvy/solar-learning-2021" target="_blank"><i class="fa fa-github fa-fw"></i>'),
    icon = icon(name = "rotate-right", lib = "font-awesome", verify_fa = FALSE),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        sliderInput(
          inputId = "lambda_start_proj",
          label = "lambda (start)",
          min = 0,
          max = 1,
          value = 0
        ),
        sliderInput(
          inputId = "lambda_end_proj",
          label = "lambda (end)",
          min = 0,
          max = 1,
          value = 1
        ),
        sliderInput(
          inputId = "delay_proj",
          label = "Years delay",
          min = 1,
          max = 10,
          value = 10
        ),
        radioButtons(
          inputId = "log_scale_proj",
          label = "Log scale on Y axis?",
          choices = c("Yes", "No"),
          selected = "No"
        )
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = "Cost Curve", 
            plotOutput("cost_proj")
          ),
          tabPanel(
            title = "Savings", 
            plotOutput("savings_proj")
          )
        )
      )
    )
  )
)

# server ----

server <- function(input, output) {

  # Get variables based on inputs ----

  log_scale_hist <- reactive({
    if (input$log_scale_hist == "Yes") {
      return(TRUE)
    }
    return(FALSE)
  })

  log_scale_proj <- reactive({
    if (input$log_scale_proj == "Yes") {
      return(TRUE)
    }
    return(FALSE)
  })
  
  lambda_nat_hist <- reactive({
    us <- make_lambda_national(
      input$lambda_start_hist, input$lambda_end_hist, input$delay_hist, 
      df_us
    )
    china <- make_lambda_national(
      input$lambda_start_hist, input$lambda_end_hist, input$delay_hist, 
      df_china
    )
    germany <- make_lambda_national(
      input$lambda_start_hist, input$lambda_end_hist, input$delay_hist, 
      df_germany
    )
    return(list(us = us, china = china, germany = germany))
  })

  lambda_nat_proj <- reactive({
    us <- make_lambda_national(
      input$lambda_start_proj, input$lambda_end_proj, input$delay_proj,
      df_nat_trends_us
    )
    china <- make_lambda_national(
      input$lambda_start_proj, input$lambda_end_proj, input$delay_proj,
      df_nat_trends_china
    )
    germany <- make_lambda_national(
      input$lambda_start_proj, input$lambda_end_proj, input$delay_proj,
      df_nat_trends_germany
    )
    return(list(us = us, china = china, germany = germany))
  })

  # Compute NATIONAL historical cost scenarios by country ----
  
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
  
  # Compute historical savings ----

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

  # Compute NATIONAL projection scenarios for national trends scenario ----
  
  get_nat_trends_proj <- reactive({
    
    lambda <- lambda_nat_proj()
    
    proj_nat_trends_national_us <- predict_cost(
        params = params_us,
        df     = df_nat_trends_us,
        lambda = lambda$us,
        exchange_rate = er_us)

    proj_nat_trends_national_china <- predict_cost(
        params = params_china,
        df     = df_nat_trends_china,
        lambda = lambda$china,
        exchange_rate = er_china_proj)
    
    proj_nat_trends_national_germany <- predict_cost(
        params = params_germany,
        df     = df_nat_trends_germany,
        lambda = lambda$germany,
        exchange_rate = er_germany_proj)
    
    nat_trends <- combine(
      global_us = proj_nat_trends_global_us,
      national_us = proj_nat_trends_national_us,
      global_china = proj_nat_trends_global_china,
      national_china = proj_nat_trends_national_china,
      global_germany = proj_nat_trends_global_germany,
      national_germany = proj_nat_trends_national_germany) %>%
      mutate(scenario = "nat_trends")
    
    return(nat_trends)

  })
  
  # Compute NATIONAL projection scenarios for sustainable dev scenario ----
  
  get_sus_dev_proj <- reactive({
    
    lambda <- lambda_nat_proj()

    proj_sus_dev_national_us <- predict_cost(
        params = params_us,
        df     = df_sus_dev_us,
        lambda = lambda$us,
        exchange_rate = er_us)

    proj_sus_dev_national_china <- predict_cost(
        params = params_china,
        df     = df_sus_dev_china,
        lambda = lambda$china,
        exchange_rate = er_china_proj)
    
    proj_sus_dev_national_germany <- predict_cost(
        params = params_germany,
        df     = df_sus_dev_germany,
        lambda = lambda$germany,
        exchange_rate = er_germany_proj)
    
    sus_dev <- combine(
        global_us = proj_sus_dev_global_us,
        national_us = proj_sus_dev_national_us,
        global_china = proj_sus_dev_global_china,
        national_china = proj_sus_dev_national_china,
        global_germany = proj_sus_dev_global_germany,
        national_germany = proj_sus_dev_national_germany) %>%
        mutate(scenario = "sus_dev")
    
    return(sus_dev)

  })
  
  # Outputs ----

  output$cost_hist <- renderPlot(
    make_historical_plot(get_costs_hist(), log_scale_hist()),
    width = 700, height = 300
  )

  output$savings_hist <- renderPlot(
    make_ann_savings_plot(get_savings_hist()),
    width = 700, height = 300
  )
  
  output$cost_proj <- renderPlot(
    make_projection_plot(
      get_nat_trends_proj(), get_sus_dev_proj(), log_scale_proj()
    ),
    width = 700, height = 500
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
