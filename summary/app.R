
# load custom functions
run_model <- function(df) {
    # For every value of lambda, run the linear model
    lambdas <- seq(0, 1, by = 0.005)
    models <- list()
    errs <- list()
    for (i in 1:length(lambdas)) {
        temp <- df %>%
            mutate(
                log_q = log(cumCapKw_world - (lambdas[i]*cumCapKw_other)),
                log_c = log(costPerKw),
                log_p = log(price_si)
            )
        model <- lm(formula = log_c ~ log_q + log_p, data = temp)
        models[[i]] <- model
        errs[[i]] <- sum(model$residuals^2)
    }
    # Get the best model based on the lowest error
    err <- unlist(errs)
    # plot(lambdas, err)
    index_best <- which(err == min(err))
    model <- models[[index_best]]

    # Best lambda
    lambda <- lambdas[index_best]
    return(list(model = model, lambda = lambda))
}

predict_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>%
        mutate(
            log_q = log(cumCapKw_world - (lambda*cumCapKw_other)),
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
        cumCapKw_world = df$cumCapKw_world,
        costPerKw = df$costPerKw)
    return(y_sim)
}

project_cost <- function(model, df, lambda) {
    nobs <- nrow(df)
    y_sim <- matrix(0, ncol = 3, nrow = nobs)
    temp <- df %>%
        mutate(
            log_q = log(cumCapKw_world - (lambda*cumCapKw_other)),
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
    y_sim <- cbind(y_sim, cumCapKw_world = df$cumCapKw_world)
    return(y_sim)
}

make_historical_plot <- function(cost_national, cost_global) {
    plot <- rbind(
        cost_global %>% mutate(scenario = "global"),
        cost_national %>% mutate(scenario = "national")) %>%
        ggplot() +
        geom_line(
            mapping = aes(x = cumCapKw_world, y = mean, color = scenario),
        ) +
        geom_ribbon(
            mapping = aes(
                x = cumCapKw_world, ymin = lower, ymax = upper, fill = scenario),
            alpha = 0.2
        ) +
        geom_point(aes(x = cumCapKw_world, y = costPerKw)) +
        scale_x_log10() +
        scale_y_log10() +
        theme_bw() +
        labs(
            x = "log(Cumulative Global Installed Capacity, kW)",
            y = "log(Cost per kW)"
        )
    return(plot)
}

make_projection_plot <- function(proj_national, proj_global) {
    plot <- rbind(
        proj_global %>% mutate(scenario = "global"),
        proj_national %>% mutate(scenario = "national")) %>%
        ggplot() +
        geom_line(
            mapping = aes(x = cumCapKw_world, y = mean, color = scenario),
        ) +
        geom_ribbon(
            mapping = aes(
                x = cumCapKw_world, ymin = lower, ymax = upper, fill = scenario),
            alpha = 0.2
        ) +
        theme_bw() +
        labs(
            x = "log(Cumulative Global Installed Capacity, kW)",
            y = "log(Cost per kW, $USD)"
        )
    return(plot)
}

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
results_us <- run_model(df_us)
results_china <- run_model(df_china)
results_germany <- run_model(df_germany)

model_list <- list(
    "U.S." = results_us$model,
    "China" = results_china$model, 
    "Germany" = results_germany$model 
)
lambda_list <- list(
    "U.S." = results_us$lambda,
    "China" = results_china$lambda, 
    "Germany" = results_germany$lambda
)
lr_list <- list(
    "U.S." = percent(1 - 2^coef(model_us)["log_q"]),
    "China" = percent(1 - 2^coef(model_china)["log_q"]), 
    "Germany" = percent(1 - 2^coef(model_germany)["log_q"])
)


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
                inputId = "delay",
                label = "Years delay",
                min = 1,
                max = 10,
                value = 6),
            sliderInput(
                inputId = "lambda_start",
                label = "lambda (start)",
                min = 0,
                max = 1,
                value = 0.1),
            sliderInput(
                inputId = "lambda_end",
                label = "lambda (end)",
                min = 0,
                max = 1,
                value = 0.9), 
            selectInput(
                inputId = "scenario",
                label = "Projection scenario", 
                choices = c("National Trends", "Sustainable Development"),
                selected = "National Trends"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("historical"),
           plotOutput("projection")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Get variables based on inputs
    df <- reactive({
        return(df_list[[input$country]])
    })
    
    df_proj <- reactive({
        if (input$scenario == "National Trends") {
            return(df_proj_nt_list[[input$country]])
        }
        return(df_proj_sd_list[[input$country]])
    })
    
    model <- reactive({
        return(model_list[[input$country]])
    })
    
    lr <- reactive({
        return(lr_list[[input$country]])
    })
    
    lambda <- reactive({ 
        return(input$lambda_start)
    })
    
    lambda_end <- reactive({ 
        return(input$lambda_end)
    })
    
    delay <- reactive({ 
        return(input$delay)
    })
    
    lambda_nat <- reactive({
        temp <- seq(input$lambda_start, input$lambda_end, length.out = delay() + 1)
        temp <- c(temp, rep(input$lambda_end, nrow(df()) - length(temp)))
        return(temp)
    })

    lambda_nat_proj <- reactive({
        temp <- seq(input$lambda_start, input$lambda_end, length.out = delay() + 1)
        temp <- c(temp, rep(input$lambda_end, nrow(df_proj()) - length(temp)))
        return(temp)
    })
    
    # Compute outcomes
    cost_global <- reactive({ 
        return(predict_cost(model(), df(), lambda()))
    })
    
    cost_national <- reactive({ 
        return(predict_cost(model(), df(), lambda_nat()))
    })
    
    proj_global <- reactive({ 
        return(project_cost(model(), df_proj(), lambda())) 
    })
    
    proj_national <- reactive({ 
        return(project_cost(model(), df_proj(), lambda_nat_proj())) 
    })
    
    output$historical <- renderPlot(
        make_historical_plot(cost_national(), cost_global()),
        width = 600, height = 400
    )
    
    output$projection <- renderPlot(
        make_projection_plot(proj_national(), proj_global()),
        width = 600, height = 400
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
