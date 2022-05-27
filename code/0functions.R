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

formatCapData_hist <- function(data_nation, data_world, year_beg, year_max) {
    data_nation <- data_nation %>% 
        filter(year >= year_beg, year <= year_max) %>% 
        addAnnCap()
    data_world <- data_world %>% 
        filter(year >= year_beg, year <= year_max) %>% 
        addAnnCap()
    result <- data_nation %>%
        select(year, annCapKw_nation = annCapacityKw) %>%
        left_join(
            data_world %>%
                filter(year >= year_beg) %>% 
                select(
                    year, cumCapKw_world = cumCapacityKw, 
                    annCapKw_world = annCapacityKw
                ),
            by = "year"
        ) %>%
        mutate(annCapKw_other = annCapKw_world - annCapKw_nation) %>% 
        # Add price data
        left_join(select(data_world, year, price_si), by = "year") %>% 
        left_join(select(data_nation, year, costPerKw), by = "year")
    return(result)
}

formatCapData_proj <- function(data_nation, data_world, year_beg, year_max) {
    result <- data_nation %>%
        select(year, annCapKw_nation = annCapacityKw) %>%
        left_join(
            data_world %>%
                filter(year >= year_beg) %>% 
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

run_model_lambda <- function(df, lambda) {
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

make_lambda_national <- function(lambda_start, lambda_end, delay, df) {
  temp <- seq(lambda_start, lambda_end, length.out = delay + 1)
  lambda_nat <- c(temp, rep(lambda_end, nrow(df) - length(temp)))
  return(lambda_nat)
}

predict_cost <- function(params, df, lambda, ci = 0.95, exchange_rate = 1) {
    df_predict <- prep_predict_data(df, lambda)
    draws <- get_y_draws(params, df_predict)
    y_sim <- lapply(draws, function(x) get_ci(x, ci))
    y_sim <- do.call(rbind, y_sim)
    names(y_sim) <- c("cost_per_kw", "cost_per_kw_lb", "cost_per_kw_ub")
    y_sim <- exp(y_sim)
    result <- cbind(year = df_predict$year, y_sim, cumCapKw = df_predict$q)
    # Add historical cost (if exists)
    result$cost_per_kw_hist <- df_predict$costPerKw
    result <- convertToUsd(result, exchange_rate)
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

compute_cost_diff <- function(
    params, 
    df, 
    lambda_nat, 
    ci = 0.95, 
    exchange_rate = 1
) {
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
    cost_diff <- convertToUsd(cost_diff, exchange_rate)
    return(cost_diff)
}

compute_savings <- function(cost_diffs, cap_additions) {

    # Now compute savings
    savings <- cost_diffs %>%
        left_join(cap_additions, by = c("year", "country")) %>%
        mutate(
            ann_savings_bil = (annCapKw_nation * cost_per_kw) / 10^9,
            ann_savings_bil_lb = (annCapKw_nation * cost_per_kw_lb) / 10^9,
            ann_savings_bil_ub = (annCapKw_nation * cost_per_kw_ub) / 10^9
        ) %>%
        select(
            year, country, ann_savings_bil, ann_savings_bil_lb,
            ann_savings_bil_ub) %>%
        group_by(country) %>%
        mutate(
            cum_savings_bil = cumsum(ann_savings_bil),
            cum_savings_bil_lb = cumsum(ann_savings_bil_lb),
            cum_savings_bil_ub = cumsum(ann_savings_bil_ub)) %>%
        ungroup()
    return(savings)
}



# Plotting ----

prep_lambda_df <- function(df) {
    lambda <- seq(0, 1, 0.2)
    q0 <- df$cumCapKw_world[1]
    results <- list()
    for (i in 1:length(lambda)) {
        results[[i]] <- df %>% 
            mutate(
                q = q0 + cumsum(annCapKw_nation + (1 - lambda[i]) * annCapKw_other),
                q_i = q0 + cumsum(annCapKw_nation), 
                p = q_i / q, 
                median_p = round(median(p), 2)
            )
    }
    result <- do.call(rbind, results)
    result$lambda <- rep(lambda, each = nrow(df))
    result <- result %>% 
        mutate(label = paste0("λ = ", lambda, ", p = ", percent(median_p, accuracy = 1)))
    return(result)
}

make_historical_plot <- function(cost, log_scale = FALSE, size = 12) {
    plot <- cost %>%
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
        scale_y_continuous(labels = scales::dollar) +
        scale_color_manual("Learning", values = colors_learning) +
        scale_fill_manual("Learning", values = colors_learning) +
        labs(
          title = paste0(
            "Estimated Module Prices Under <span style = 'color: ",
            colors_learning["Global"], 
            ";'>Global</span> vs. <span style = 'color: ", 
            colors_learning["National"], 
            ";'>National</span> Market Scenarios"),
            y = paste0("Price per kW (", year_inflation, " $USD)"),
            x = "Year"
        ) + 
        theme_minimal_grid(
            font_size = size,
            font_family = font_main
        ) +
        panel_border() +
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
            labs(y = paste0("log(Price per kW), ", year_inflation, " $USD"))
    }
    return(plot)
}

make_cum_savings_plot <- function(savings, size = 12) { 
    
    # First compute label locations
    china <- savings %>% 
        filter(country == "China", year == 2018) %>% 
        pull(cum_savings_bil)
    us <- savings %>% 
        filter(country == "U.S.", year == 2018) %>% 
        pull(cum_savings_bil)
    germany <- savings %>% 
        filter(year == 2019) %>% 
        select(country, cum_savings_bil)
    y_china <- china / 2
    y_us <- (us / 2) + china
    y_germany <- sum(germany$cum_savings_bil)
    label_data <- data.frame(
        x = c(2018, 2018, 2017), 
        y = c(y_china, y_us, y_germany), 
        label = c("China", "U.S.", "Germany")
    )
    
    # Now make the plot
    plot <- savings %>%
        mutate(country = fct_relevel(country, c("Germany", "U.S.", "China"))) %>%
        ggplot() +
        geom_area(aes(x = year, y = cum_savings_bil, fill = country)) +
        scale_fill_manual(values = colors_country) +
        scale_x_continuous(
          breaks = seq(year_savings_min, year_savings_max, 2),
          limits = c(year_savings_min, year_savings_max)) +
        scale_y_continuous(
          labels = dollar,
          breaks = seq(0, 80, 20),
          limits = c(0, 80),
          expand = expansion(mult = c(0, 0.05))) +
        theme_minimal_hgrid(font_family = font_main, font_size = size) +
        scale_color_manual(values = c("white", "black", "white")) +
        labs(
            title = "Cumulative Module Savings",
            subtitle = "Difference Between Global and National Market Scenarios (2008 - 2020)",
            x = "Year",
            y = paste0("Cumulative Savings (Billion ", year_inflation, " $USD)"),
            fill = "Country") +
        theme(
            plot.title.position = "plot",
            legend.position = "none"
        ) +
        # Add country labels
        geom_text(
            data = label_data, 
            aes(x = x, y = y, label = label, color = label), 
            size = 6, family = font_main
        )
    return(plot)
}

make_ann_savings_plot <- function(savings, size = 12) { 
    plot <- savings %>% 
        ggplot() + 
        facet_wrap(vars(country), nrow = 1) +
        geom_col(aes(x = year, y = ann_savings_bil, fill = country)) + 
        geom_errorbar(
            aes(x = year, ymin = ann_savings_bil_lb, ymax = ann_savings_bil_ub), 
            color = "grey42", width = 0.5) + 
        scale_x_continuous(breaks = seq(year_savings_min, year_savings_max, 2)) +
        scale_y_continuous(
            labels = scales::dollar, 
            expand = expansion(mult = c(0, 0.05))
        ) +
        scale_fill_manual(
            values = c(
                colors_country[3], colors_country[1], colors_country[2]
            )
        ) +
        theme_minimal_hgrid(
            font_size = size,
            font_family = font_main) +
        panel_border() +
        theme(
            plot.title.position = "plot",
            legend.position = "none",
            axis.line.x = element_blank(),
            strip.background = element_rect(fill = "grey80"), 
            panel.grid.major = element_line(
                size = 0.5, colour = "grey90")
        ) +
         labs(
            title = "Annual Module Savings Under Global vs. National Market Scenarios (2008 - 2020)",
            x = "Year",
            y = paste0("Annual Savings (Billion ", year_inflation, " $USD)"),
            fill = "Country") 
    return(plot)
}
    
make_projection_plot <- function(
  nat_trends, sus_dev, log_scale = FALSE, size = 12
) {
    plot <- rbind(nat_trends, sus_dev) %>% 
      mutate(
        learning = str_to_title(learning),
        learning = fct_relevel(learning, c("National", "Global")),
        scenario = fct_recode(scenario, 
          "National Trends" = "nat_trends", 
          "Sustainable Development" = "sus_dev"),
        year = lubridate::ymd(paste0(year, "-01-01"))) %>%
      ggplot() +
      facet_grid(scenario ~ country) +
      geom_ribbon(
        aes(x = year, ymin = cost_per_kw_lb, ymax = cost_per_kw_ub,
            fill = learning), alpha = 0.25) +
      geom_line(
        aes(x = year, y = cost_per_kw, color = learning),
        alpha = 0.6, size = 1) +
      scale_x_date(
        limits = lubridate::ymd(c("2019-07-01", "2030-07-01")),
        date_labels = "'%y",
        date_breaks = "2 years") +
      scale_y_continuous(labels = scales::dollar) +
      # expand_limits(y = 0) +
      scale_color_manual("Scenario", values = colors_learning) +
      scale_fill_manual("Scenario", values = colors_learning) +
      theme_minimal_grid(
        font_size = size,
        font_family = font_main) +
      panel_border() +
      theme(
        plot.title.position = "plot",
        plot.title = element_markdown(),
        legend.position = "none",
        strip.background = element_rect(fill = "grey80"),
        panel.grid.major = element_line(size = 0.5, colour = "grey90")
      ) +
      labs(
        y = paste0("Price per kW (", year_inflation, " $USD)"),
        x = "Year",
        title = paste0(
          "Projected Module Prices Under <span style = 'color: ",
          colors_learning["Global"], 
          ";'>Global</span> vs. <span style = 'color: ", 
          colors_learning["National"], 
          ";'>National</span> Market Scenarios (2020 - 2030)"))
    if (log_scale) {
        plot <- plot + 
            scale_y_log10(
                labels = function(x) scales::dollar(x, accuracy = 1)
            ) + 
            labs(y = paste0("log(Price per kW), ", year_inflation, " $USD"))
    }
    return(plot)
}

make_ann_savings_proj_plot <- function(nat_trends, sus_dev, size = 12) {
  
    plot <- rbind(nat_trends, sus_dev) %>% 
        mutate(
          scenario = fct_recode(scenario, 
            "National Trends" = "nat_trends", 
            "Sustainable Development" = "sus_dev")) %>% 
        ggplot() + 
        facet_grid(scenario ~ country) +
        geom_col(aes(x = year, y = ann_savings_bil, fill = country)) + 
        geom_errorbar(
            aes(x = year, ymin = ann_savings_bil_lb, ymax = ann_savings_bil_ub), 
            color = "grey42", width = 0.5) + 
        scale_x_continuous(breaks = seq(year_proj_min, year_proj_max, 2)) +
        scale_y_continuous(
            labels = scales::dollar, 
            expand = expansion(mult = c(0, 0.05))
        ) +
        scale_fill_manual(
            values = c(
                colors_country[3], colors_country[1], colors_country[2]
            )
        ) +
        theme_minimal_hgrid(
            font_size = size,
            font_family = font_main) +
        panel_border() +
        theme(
            plot.title.position = "plot",
            legend.position = "none",
            axis.line.x = element_blank(),
            strip.background = element_rect(fill = "grey80"), 
            panel.grid.major = element_line(
                size = 0.5, colour = "grey90")
        ) +
         labs(
            title = "Projected Annual Module Savings Under Global vs. National Market Scenarios (2020 - 2030)",
            x = "Year",
            y = paste0("Annual Savings (Billion ", year_inflation, " $USD)"),
            fill = "Country") 
    return(plot)
}

# Summarizing results

get_cost_summary_hist <- function(cost) {
    result <- cost %>% 
        filter(year == 2020) %>% 
        select(country, learning, cost_per_kw) %>% 
        pivot_wider(
            names_from = learning,
            values_from = cost_per_kw) %>% 
        mutate(
            diff = national - global,
            p = scales::percent(diff / global),
            global = scales::dollar(round(global)),
            national = scales::dollar(round(national)),
            summary = paste0(
                country, ": ", national, " versus ", global, ", or ", 
                p, " higher\n")
        ) 
    result <- paste(paste0("- ", result$summary), collapse = "")
    result <- paste0(
      "\n2020 solar PV module prices under national versus global market scenarios:\n\n",
      result
    )
    return(result)
}

get_savings_summary_hist <- function(savings) {
    savings <- savings %>% 
        filter(year == max(year)) %>% 
        mutate(
          summary = paste0(
            country, ": ",
            scales::dollar(round(cum_savings_bil)), " (", 
            scales::dollar(round(cum_savings_bil_lb)), ", ",
            scales::dollar(round(cum_savings_bil_ub)), ")\n"))    
    total <- savings %>% 
      summarise(
        mean = scales::dollar(round(sum(cum_savings_bil))), 
        lb = scales::dollar(round(sum(cum_savings_bil_lb))), 
        ub = scales::dollar(round(sum(cum_savings_bil_ub))))
    total <- paste0(total$mean, " (", total$lb, ", ", total$ub, ")")
    result <- paste(paste0("- ", savings$summary), collapse = "")
    result <- paste0(
      "Cumulative savings from global over national market scenarios, 2008 - 2020 ",
      "(Billions 2020 $USD):\n\n", result, "\n\nTotal: ", total
    )
    return(result)
}

get_cost_summary_proj <- function(nat_trends, sus_dev) {
    result <- rbind(nat_trends, sus_dev) %>% 
        mutate(
            learning = str_to_title(learning),
            learning = fct_relevel(learning, c("National", "Global")),
            scenario = fct_recode(scenario, 
            "National Trends" = "nat_trends", 
            "Sustainable Development" = "sus_dev")
        ) %>% 
        filter(year == year_proj_max) %>% 
        select(year, learning, country, scenario, cost_per_kw) %>% 
        pivot_wider(
            names_from = learning, 
            values_from = cost_per_kw) %>% 
        arrange(scenario) %>% 
        group_by(scenario) %>% 
        mutate(
            diff = National - Global, 
            p = scales::percent(round(diff / Global, 2)), 
            
            global = scales::dollar(round(Global)),
            national = scales::dollar(round(National)),
            summary = paste0(
                country, ": ", national, " versus ", global, ", or ", 
                p, " higher\n")
        )
    nat_trends_summary <- result %>% 
      filter(scenario == "National Trends")
    sus_dev_summary <- result %>% 
      filter(scenario == "Sustainable Development")
    nat_trends_summary <- paste(
      paste0("- ", nat_trends_summary$summary), collapse = "")
    sus_dev_summary <- paste(
      paste0("- ", sus_dev_summary$summary), collapse = "")
    result <- paste0(
      "2030 solar PV module prices under national versus global market scenarios",
      '\n\nNATIONAL TRENDS scenario:\n\n',
      nat_trends_summary,
      '\n\nSUSTAINABLE DEVELOPMENT scenario:\n\n',
      sus_dev_summary, "\n\n", sep = ""
    )
    return(result)
}

get_savings_summary_proj <- function(nat_trends, sus_dev) {
    savings <- rbind(nat_trends, sus_dev) %>% 
        mutate(
            scenario = fct_recode(scenario, 
            "National Trends" = "nat_trends", 
            "Sustainable Development" = "sus_dev")) %>% 
        filter(year == year_proj_max) %>% 
        mutate(
          summary = paste0(
            country, ": ",
            scales::dollar(round(cum_savings_bil)), " (", 
            scales::dollar(round(cum_savings_bil_lb)), ", ",
            scales::dollar(round(cum_savings_bil_ub)), ")\n"))    
    nat_trends_summary <- savings %>% 
      filter(scenario == "National Trends")
    sus_dev_summary <- savings %>% 
      filter(scenario == "Sustainable Development")
    nat_trends_summary <- paste(
      paste0("- ", nat_trends_summary$summary), collapse = "")
    sus_dev_summary <- paste(
      paste0("- ", sus_dev_summary$summary), collapse = "")
    # Compute totals
     total <- savings %>% 
      group_by(scenario) %>% 
      summarise(
        mean = scales::dollar(round(sum(cum_savings_bil))), 
        lb = scales::dollar(round(sum(cum_savings_bil_lb))), 
        ub = scales::dollar(round(sum(cum_savings_bil_ub)))) %>% 
      mutate(total = paste0(mean, " (", lb, ", ", ub, ")"))
    nat_trends_total <- total %>% 
      filter(scenario == "National Trends") %>% 
      pull(total)
    sus_dev_total <- total %>% 
      filter(scenario == "Sustainable Development") %>% 
      pull(total)
    result <- paste0(
      "Cumulative projected savings from global over national market scenarios, 2020 - 2030 (Billions 2020 $USD)\n\n",
      'NATIONAL TRENDS scenario:\n\n',
      nat_trends_summary,
      "\n\nTotal: ", nat_trends_total,
      '\n\nSUSTAINABLE DEVELOPMENT scenario:\n\n',
      sus_dev_summary,
      "\n\nTotal: ", sus_dev_total,
      sep = ""
    )
    return(result)
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

convertToUsd <- function(df, exchange_rate) {
  if (length(exchange_rate) == 1) {
    temp <- data.frame(year = df$year)
    temp$average_of_rate <- exchange_rate
    exchange_rate <- temp
  }
  result <- df %>% 
    left_join(exchange_rate, by = "year") %>% 
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

combine <- function(
    global_us,
    national_us,
    global_china,
    national_china,
    global_germany,
    national_germany
) {
    result <- rbind(
        mutate(global_us, learning = "global", country = "U.S."),
        mutate(national_us, learning = "national", country = "U.S."),
        mutate(global_china, learning = "global", country = "China"),
        mutate(national_china, learning = "national", country = "China"),
        mutate(global_germany, learning = "global", country = "Germany"),
        mutate(national_germany, learning = "national", country = "Germany")
    )
    return(result)
}

combine_cost_diffs <- function(us, china, germany, year_min, year_max) {
    cost_diffs <- rbind(
        mutate(us, country = "U.S."), 
        mutate(china, country = "China"), 
        mutate(germany, country = "Germany")) %>% 
        filter(year >= year_min, year <= year_max)
    return(cost_diffs)
}
