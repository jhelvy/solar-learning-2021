# Load libraries, functions, and dir paths
source(here::here('code', '0setup.R'))
library(shiny)

# load custom functions
run_model <- function(df, lambda) {
    # Run the linear model for a given lambda
    q0 <- df$cumCapKw_world[1]
    temp <- df %>%
        mutate(
            q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
            log_q = log(q),
            log_c = log(costPerKw),
            log_p = log(price_si)
        )
    return(lm(formula = log_c ~ log_q + log_p, data = temp))
}

predict_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    q0 <- df$cumCapKw_world[1]
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>%
        mutate(
            q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
            log_q = log(q),
            log_c = log(costPerKw),
            log_p = log(price_si)
        )
    params <- as.data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    names(params) <- c("alpha", "beta", "gamma")
    for (i in 1:nobs) {
        sim <- params$alpha + params$beta * temp$log_q[i] + params$gamma * temp$log_p[i]
        y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
    }
    y_sim <- as.data.frame(exp(y_sim))
    names(y_sim) <- c("mean", "lower", "upper")
    y_sim <- cbind(
        y_sim,
        cumCapKw = temp$q,
        costPerKw = df$costPerKw, 
        year = df$year
    )
    return(y_sim)
}

project_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    q0 <- df$cumCapKw_world[1]
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>%
        mutate(
            q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
            log_q = log(q),
            log_p = log(price_si)
        )
    params <- as.data.frame(MASS::mvrnorm(10^4, coef(model), vcov(model)))
    names(params) <- c("alpha", "beta", "gamma")
    for (i in 1:nobs) {
        sim <- params$alpha + params$beta * temp$log_q[i] + params$gamma * temp$log_p[i]
        y_sim[i,] <- c(mean(sim), quantile(sim, probs = c(0.05, 0.95)))
    }
    y_sim <- as.data.frame(exp(y_sim))
    names(y_sim) <- c("mean", "lower", "upper")
    y_sim <- cbind(y_sim, year = df$year, cumCapKw = temp$q)
    return(y_sim)
}

make_historical_plot <- function(cost_national, cost_global) {
    plot <- rbind(
        cost_global %>% mutate(scenario = "global"),
        cost_national %>% mutate(scenario = "national")) %>%
        mutate(scenario = fct_relevel(scenario, c("national", "global"))) %>% 
        ggplot() +
        geom_line(
            mapping = aes(x = year, y = mean, color = scenario),
        ) +
        geom_ribbon(
            mapping = aes(
                x = year, ymin = lower, ymax = upper, fill = scenario),
            alpha = 0.2
        ) +
        geom_point(aes(x = year, y = costPerKw)) +
        scale_x_continuous(breaks = cost_global$year) +
        scale_y_log10() +
        theme_bw() +
        labs(
            title = "Estimated Module Cost Under Global vs. National Markets",
            x = "log(Cumulative Global Installed Capacity, kW)",
            y = "log(Cost per kW)"
        )
    return(plot)
}

make_projection_plot <- function(
    proj_national_nt, proj_global_nt, proj_national_sd, proj_global_sd
) {
    plot <- rbind(
        proj_global_nt %>% 
            mutate(learning = "global", scenario = "National Trends"),
        proj_national_nt %>% 
            mutate(learning = "national", scenario = "National Trends"),
        proj_global_sd %>% 
            mutate(learning = "global", scenario = "Sustainable Development"),
        proj_national_sd %>% 
            mutate(learning = "national", scenario = "Sustainable Development")
        ) %>%
        mutate(
            learning = fct_relevel(learning, c("national", "global"))
        ) %>% 
        ggplot() +
        geom_line(
            mapping = aes(x = year, y = mean, color = learning),
        ) +
        geom_ribbon(
            mapping = aes(
                x = year, ymin = lower, ymax = upper, fill = learning),
            alpha = 0.2
        ) +
        scale_x_continuous(breaks = proj_global_nt$year) +
        facet_wrap(vars(scenario), nrow = 1) +
        theme_bw() +
        labs(
            title = "Estimated Module Cost Under Global vs. National Markets",
            x = "log(Cumulative Global Installed Capacity, kW)",
            y = "log(Cost per kW, $USD)"
        )
    return(plot)
}

# calculations ----

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
