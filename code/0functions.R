# NOTES
#
# Basic learning curve model: Y_x = A*x^b
# where
#   Y = the cost of unit x (dependent variable)
#   A = the theoretical cost of unit 1 (a.k.a. T1)
#   x = the unit number (independent variable)
#   b = a constant representing the slope (slope = 2^b)
#
# Log transformation: ln(Y) = ln(A) + b*ln(x)
#           Re-write: Y'    = int   + b*x'
#
# To convert log-space estimated coefficients back to original model:
# A = exp(int)
# b = b
# Learning curve slope = 2^b
# Learning Rate        = 1 - slope
#
# Approximation of the cumulative total cost of producing N units:
# C = (A*N^(b + 1)) / (b + 1)
#
# Two factor learning curve model: Y = A * x^b * p^c
# where
#   Y = the cost of unit x at silicon price p (dependent variable)
#   A = the theoretical cost of unit 1 (a.k.a. T1)
#   x = the unit number (independent variable)
#   p = silicon price
#   b = lnCap_estimate = learning coefficient on capacity
#   c = lnSi_estimate = coefficient on silicon price

# Data formatting ----

getFutureCapRate <- function(target_capacity, cap_begin, num_years) {
  # Percent annual growth in cumulative capacity
  return(((target_capacity / cap_begin)^(1 / (num_years))) - 1)
}

getFutureCapacities <- function(
    rate, cap_begin, num_years, year_min_proj, price_si
) {
  cumCapacityKw <- rep(cap_begin, num_years + 1)
  for (i in 2:(num_years + 1)) {
      cumCapacityKw[i] <- cumCapacityKw[i-1]*(1 + rate)
  }
  # Add capacity to reach target
  result <- data.frame(
      year = year_min_proj - 1 + seq(num_years + 1),
      cumCapacityKw = cumCapacityKw) %>%
      addAnnCap() %>% 
      mutate(price_si = price_si)
  return(result)
}

addAnnCap <- function(df) {
    result <- df %>%
        mutate(
            annCapacityKw = cumCapacityKw - lag(cumCapacityKw, 1),
            annCapacityKw = ifelse(is.na(annCapacityKw), 0, annCapacityKw))
    return(result)
}

formatCapData <- function(data_nation, data_world, year_beg, year_max) {
    result <- data_nation %>%
        select(year, annCapKw_nation = annCapacityKw) %>%
        left_join(
            data_world %>%
                select(
                    year, cumCapKw_world = cumCapacityKw, 
                    annCapKw_world = annCapacityKw
                ),
            by = "year"
        ) %>%
        filter(year >= year_beg, year <= year_max) %>% 
        mutate(annCapKw_other = annCapKw_world - annCapKw_nation) %>% 
        # Add price data
        left_join(select(data_world, year, price_si), by = "year")
    return(result)
}

# Modeling ----

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

# Scenarios ----

make_lambda_national <- function(lambda_start, lambda_end, df) {
  temp <- seq(lambda_start, lambda_end, length.out = delay + 1)
  lambda_nat <- c(temp, rep(lambda_end, nrow(df) - length(temp)))
  return(lambda_nat)
}

predict_cost <- function(params, df, lambda, ci = 0.95, include_hist = TRUE) {
    df_predict <- prep_predict_data(df, lambda)
    draws <- get_y_draws(params, df_predict)
    y_sim <- lapply(draws, function(x) get_ci(x, ci))
    y_sim <- do.call(rbind, y_sim)
    names(y_sim) <- c("cost_per_kw", "cost_per_kw_lb", "cost_per_kw_ub")
    y_sim <- exp(y_sim)
    result <- cbind(year = df_predict$year, y_sim, cumCapKw = df_predict$q)
    if (include_hist) {
      result$cost_per_kw_hist <- df_predict$costPerKw
    }
    return(result)
}

prep_predict_data <- function(df, lambda) {
  q0 <- df$cumCapKw_world[1]
  temp <- df %>%
    mutate(
      q = q0 + cumsum(annCapKw_nation + (1 - lambda) * annCapKw_other),
      log_q = log(q),
      log_p = log(price_si)
    )
  return(temp)
}

get_y_draws <- function(params, df) {
  nobs <- nrow(df)
  y_sim <- list()
  for (i in 1:nobs) {
    y_sim[[i]] <- params$alpha + params$beta * df$log_q[i] + params$gamma * df$log_p[i]
  }
  return(y_sim)
}

compute_cost_diff <- function(params, df, lambda_nat, ci = 0.95) {
    # Get draws of cost for each scenarios
    df_predict_global <- prep_predict_data(df, 0)
    df_predict_national <- prep_predict_data(df, lambda_nat)
    draws_global <- get_y_draws(params, df_predict_global)
    draws_national <- get_y_draws(params, df_predict_national)
    # Compute difference in draws
    nobs <- nrow(df)
    cost_diff <- matrix(0, ncol = 3, nrow = nobs)
    for (i in 1:nobs) {
        diff <- exp(draws_national[[i]]) - exp(draws_global[[i]])
        cost_diff[i,] <- as.matrix(get_ci(diff, ci))
    }
    cost_diff <- as.data.frame(cost_diff)
    names(cost_diff) <- c("cost_per_kw", "cost_per_kw_lb", "cost_per_kw_ub")
    cost_diff$year <- df$year
    return(cost_diff)
}




# Plotting ----

make_historical_plot <- function(
    cost_global_us,
    cost_national_us,
    cost_global_china,
    cost_national_china,
    cost_global_germany,
    cost_national_germany, 
    log_scale = FALSE
) {
    plot <- combine_costs(
        cost_global_us,
        cost_national_us,
        cost_global_china,
        cost_national_china,
        cost_global_germany,
        cost_national_germany) %>% 
    mutate(
        learning = str_to_title(learning),
        learning = fct_relevel(learning, c("National", "Global")),
        year = lubridate::ymd(paste0(year, "-01-01"))) %>% 
    ggplot() +
    facet_wrap(vars(country), nrow = 1) +
    geom_point(aes(x = year, y = cost_per_kw_hist), size = 1) +
    geom_line(
        aes(
            x = year, 
            y = cost_per_kw, 
            color = learning
        ),
        alpha = 0.6, size = 1
    ) +
    geom_ribbon(
        aes(
            x = year, 
            ymin = cost_per_kw_lb, 
            ymax = cost_per_kw_ub,
            fill = learning
        ), 
        alpha = 0.22
    ) +
    scale_x_date(
        limits = lubridate::ymd(c(
            paste0(plot_min_year - 1, "-07-01"),
            paste0(plot_max_year, "-07-01"))
        ),
        date_labels = "'%y",
        date_breaks = "2 years") +
    scale_y_continuous(
      limits = c(0, 6000),
      breaks = seq(0, 6000, 1000),
      labels = scales::dollar
    ) +
    scale_color_manual("Learning", values = colors_learning) +
    scale_fill_manual("Learning", values = colors_learning) +
    theme_minimal_grid(
        font_size = 16,
        font_family = font_main
    ) +
    panel_border() +
    labs(
      title = paste0(
        "Estimated Module Cost Under <span style = 'color: ",
        colors_learning["Global"], 
        ";'>Global</span> vs. <span style = 'color: ", 
        colors_learning["National"], 
        ";'>National</span> Market Scenarios"),
        y = paste0("Cost per kW (", year_inflation, " $USD)"),
        x = "Year"
    ) + 
    theme(
        plot.title.position = "plot",
        strip.background = element_rect(fill = "grey80"), 
        panel.grid.major = element_line(size = 0.3, colour = "grey90"),
        axis.line.x = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1, size = 11, face = "italic"),
        plot.title = element_markdown(),
        legend.position = "none"
    ) 
    if (log_scale) {
        plot <- plot + 
            scale_y_log10(
                labels = function(x) scales::dollar(x, accuracy = 1)
            ) + 
            labs(y = paste0("log(Cost per kW), ", year_inflation, " $USD"))
    }
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




# General utility ----

get_ci <- function(x, ci = 0.95) {
  alpha <- (1 - ci)/2
  df <- data.frame(
    mean  = mean(x),
    lower = stats::quantile(x, alpha),
    upper = stats::quantile(x, 1 - alpha)
  )
  return(df)
}

convertToUsd <- function(df, exchangeRates) {
  if (length(exchangeRates) == 1) {
    temp <- data.frame(year = df$year)
    temp$average_of_rate <- exchangeRates
    exchangeRates <- temp
  }
  result <- df %>% 
    left_join(exchangeRates, by = "year") %>% 
    pivot_longer(
      cols = starts_with("cost"),
      names_to = "label",
      values_to = "cost"
    ) %>% 
    mutate(cost = cost / average_of_rate) %>% 
    pivot_wider(
      names_from = label, 
      values_from = cost
    ) %>% 
    select(-average_of_rate)
  return(result)
}

combine_costs <- function(
    cost_global_us,
    cost_national_us,
    cost_global_china,
    cost_national_china,
    cost_global_germany,
    cost_national_germany
) {
    cost <- rbind(
        mutate(cost_global_us, learning = "global", country = "U.S."),
        mutate(cost_national_us, learning = "national", country = "U.S."),
        mutate(cost_global_china, learning = "global", country = "China"),
        mutate(cost_national_china, learning = "national", country = "China"),
        mutate(cost_global_germany, learning = "global", country = "Germany"),
        mutate(cost_national_germany, learning = "national", country = "Germany")
    )
    return(cost)
}
