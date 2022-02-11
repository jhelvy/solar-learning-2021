# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))
library(shiny)

# Load formatted data
data <- readRDS(dir$data_formatted)
df_list <- list(
    "U.S." = data$hist_us,
    "China" = data$hist_china,
    "Germany" = data$hist_germany
)
df_proj_nt_list <- list(
    "U.S." = data$proj_nat_trends_us,
    "China" = data$proj_nat_trends_china,
    "Germany" = data$proj_nat_trends_germany
)
df_proj_sd_list <- list(
    "U.S." = data$proj_sus_dev_us,
    "China" = data$proj_sus_dev_china,
    "Germany" = data$proj_sus_dev_germany
)

# Run historical models
model_us <- run_model(df_list[["U.S."]], lambda = 0)
model_china <- run_model(df_list[["China"]], lambda = 0)
model_germany <- run_model(df_list[["Germany"]], lambda = 0)

model_list <- list(
    "U.S." = model_us,
    "China" = model_china, 
    "Germany" = model_germany
)
lr_list <- list(
    "U.S." = percent(1 - 2^coef(model_us)["log_q"]),
    "China" = percent(1 - 2^coef(model_china)["log_q"]), 
    "Germany" = percent(1 - 2^coef(model_germany)["log_q"])
)

# # Example calculations
# country <- "U.S."
# model <- model_list[[country]]
# df <- df_list[[country]]
# df_proj <- df_proj_nt_list[[country]]
# lambda_start <- 0
# lambda_end <- 1
# delay <- 10
# temp <- seq(lambda_start, lambda_end, length.out = delay + 1)
# lambda_hist <- c(temp, rep(lambda_end, nrow(df) - length(temp)))
# lambda_proj <- c(temp, rep(lambda_end, nrow(df_proj) - length(temp)))
# predict_cost(model, df, lambda = 0)
# predict_cost(model, df, lambda_hist)
# project_cost(model, df_proj, lambda <- 0)
# project_cost(model, df_proj, lambda_proj)

# ui ----

# Define UI for application that draws a histogram 
ui <- fluidPage(

    # Application title
    titlePanel("Learning Models"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "country",
                label = "Select country", 
                choices = c("U.S.", "China", "Germany"),
                selected = "U.S."
            ),
            sliderInput(
                inputId = "lambda_start",
                label = "lambda (start)",
                min = 0,
                max = 1,
                value = 0),
            sliderInput(
                inputId = "lambda_end",
                label = "lambda (end)",
                min = 0,
                max = 1,
                value = 1),
            sliderInput(
                inputId = "delay",
                label = "Years delay",
                min = 1,
                max = 10,
                value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Learning Rate:"),
            textOutput('lr'),
            h3("Historical cost"),
            plotOutput("historical"),
            h3("Future cost"),
            plotOutput("projection")
        )
    )
)

# server ----

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Get variables based on inputs

    lambda_nat <- reactive({
        temp <- seq(
            input$lambda_start, 
            input$lambda_end, 
            length.out = input$delay + 1
        )
        temp <- c(
            temp, 
            rep(input$lambda_end, nrow(df()) - length(temp))
        )
        return(temp)
    })
    
    lambda_nat_proj <- reactive({
        temp <- seq(
            input$lambda_start, 
            input$lambda_end, 
            length.out = input$delay + 1
        )
        temp <- c(
            temp, 
            rep(input$lambda_end, nrow(df_proj_nt()) - length(temp))
        )
        return(temp)
    })
    
    df <- reactive({
        return(df_list[[input$country]])
    })
    
    df_proj_nt <- reactive({
        return(df_proj_nt_list[[input$country]])
    })
    
    df_proj_sd <- reactive({
        return(df_proj_sd_list[[input$country]])
    })

    model <- reactive({
        return(model_list[[input$country]])
    })

    # Compute outcomes
    cost_global <- reactive({ 
        return(predict_cost(model(), df(), lambda = 0))
    })
    
    cost_national <- reactive({ 
        return(predict_cost(model(), df(), lambda_nat()))
    })
    
    proj_global_nt <- reactive({ 
        return(project_cost(model(), df_proj_nt(), lambda = 0)) 
    })
    
    proj_national_nt <- reactive({ 
        return(project_cost(model(), df_proj_nt(), lambda_nat_proj())) 
    })

    proj_global_sd <- reactive({ 
        return(project_cost(model(), df_proj_sd(), lambda = 0)) 
    })
    
    proj_national_sd <- reactive({ 
        return(project_cost(model(), df_proj_sd(), lambda_nat_proj())) 
    })
    
    # Output
    
    output$lr <- renderText(lr_list[[input$country]])
    
    output$historical <- renderPlot(
        make_historical_plot(cost_national(), cost_global()),
        width = 650, height = 400
    )
    
    output$projection <- renderPlot(
        make_projection_plot(
            proj_national_nt(), proj_global_nt(), 
            proj_national_sd(), proj_global_sd()
        ),
        width = 800, height = 350
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
