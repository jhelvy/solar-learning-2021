
# load custom functions
run_model <- function(df, lambda) {
    # Run the linear model for a given lambda
    temp <- df %>%
        mutate(
            log_q = log(cumCapKw_world - (lambda*cumCapKw_other)),
            log_c = log(costPerKw),
            log_p = log(price_si)
        )
    model <- lm(formula = log_c ~ log_q + log_p, data = temp)
    err <- sum(model$residuals^2)
    return(list(model = model, err = err))
}

run_models <- function(df) {
    
    # For every value of lambda, run the linear model
    lambdas <- seq(0, 1, by = 0.005)
    models <- list()
    errs <- list()
    for (i in 1:length(lambdas)) {
        result <- run_model(df, lambdas[i])
        models[[i]] <- result$model
        errs[[i]] <- result$err
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
        costPerKw = df$costPerKw, 
        year = df$year)
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
    y_sim <- cbind(y_sim, cumCapKw_world = df$cumCapKw_world, year = df$year)
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
results_us <- run_models(df_list[["U.S."]])
results_china <- run_models(df_list[["China"]])
results_germany <- run_models(df_list[["Germany"]])

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
    "U.S." = percent(1 - 2^coef(model_list[["U.S."]])["log_q"]),
    "China" = percent(1 - 2^coef(model_list[["China"]])["log_q"]), 
    "Germany" = percent(1 - 2^coef(model_list[["Germany"]])["log_q"])
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
                inputId = "lambda_start",
                label = "lambda (start)",
                min = 0,
                max = 1,
                value = 0.32),
            h4("National Markets Scenario Controls:"),
            sliderInput(
                inputId = "lambda_end",
                label = "lambda (end)",
                min = 0,
                max = 1,
                value = 0.9),
            sliderInput(
                inputId = "delay",
                label = "Years delay",
                min = 1,
                max = 10,
                value = 6)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Best Model Results:"),
            textOutput('lr_best'),
            textOutput('lambda_best'),
            h2("Current Model Results:"),
            textOutput('lr'),
            textOutput('lambda'),
            h2("Historical cost"),
            plotOutput("historical"),
            h2("Future cost"),
            plotOutput("projection")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Get variables based on inputs
    lambda <- reactive({ 
        return(input$lambda_start)
    })
    
    lambda_end <- reactive({ 
        return(input$lambda_end)
    })

    lambda_nat <- reactive({
        temp <- seq(input$lambda_start, input$lambda_end, length.out = delay() + 1)
        temp <- c(temp, rep(input$lambda_end, nrow(df()) - length(temp)))
        return(temp)
    })
    
    lambda_nat_proj <- reactive({
        temp <- seq(input$lambda_start, input$lambda_end, length.out = delay() + 1)
        temp <- c(temp, rep(input$lambda_end, nrow(df_proj_nt()) - length(temp)))
        return(temp)
    })
    
    delay <- reactive({ 
        return(input$delay)
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
        result <- run_model(df(), lambda())
        return(result$model)
    })

    lr <- reactive({
        return(percent(1 - 2^coef(model())["log_q"]))
    })
    
    lr_best <- reactive({
        return(lr_list[[input$country]])
    })

    lambda_best <- reactive({ 
        return(lambda_list[[input$country]])
    })

    # Compute outcomes
    cost_global <- reactive({ 
        return(predict_cost(model(), df(), lambda()))
    })
    
    cost_national <- reactive({ 
        return(predict_cost(model(), df(), lambda_nat()))
    })
    
    proj_global_nt <- reactive({ 
        return(project_cost(model(), df_proj_nt(), lambda())) 
    })
    
    proj_national_nt <- reactive({ 
        return(project_cost(model(), df_proj_nt(), lambda_nat_proj())) 
    })

    proj_global_sd <- reactive({ 
        return(project_cost(model(), df_proj_sd(), lambda())) 
    })
    
    proj_national_sd <- reactive({ 
        return(project_cost(model(), df_proj_sd(), lambda_nat_proj())) 
    })
    
    # Output
    
    output$lr_best <- renderText(paste0("Learning Rate: ", lr_best()))
    
    output$lambda_best <- renderText(paste0("λ: ", lambda_best()))
    
    output$lr <- renderText(paste0("Learning Rate: ", lr()))
    
    output$lambda <- renderText(paste0("λ: ", lambda()))
    
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
