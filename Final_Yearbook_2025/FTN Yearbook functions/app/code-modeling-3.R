#' FTN Yearbook Time Series Modeling Functions
#' 
#' This file contains functions for time series modeling and prediction
#' for the FTN Yearbook analysis tools.
#' 

# Source required utilities if not already loaded
if (!exists("load_required_packages")) {
  source("code-utils-1.R")
}

# Source ERS theme
source("ers_theme.R")

#' Create time series models and generate predictions
#'
#' This function fits time series models to the prepared data and
#' generates predictions for future periods.
#'
#' @param yearbook_sel Prepared time series data from prepare_time_series()
#' @param predict_duration Number of years to predict
#' @param monthly_ind Whether data is monthly ("Yes" or "No")
#'
#' @return A list containing the fitted models, predictions, and processed data
model_time_series <- function(yearbook_sel, predict_duration = 5, monthly_ind = "No") {
  # Ensure required packages are loaded
  required_packages <- c("dplyr", "tidyr", "ggplot2", "stringr", "broom", "lubridate", "purrr")
  suppressMessages(load_required_packages(required_packages))
  
  # Group data by commodity to process each one
  grouped_data <- yearbook_sel %>%
    group_by(commodity_element)
  
  # --- NESTED MODELING APPROACH ---
  # Nest the data to apply modeling to each commodity group
  nested_data <- grouped_data %>%
    tidyr::nest()

  # Define modeling function to be applied to each nested data frame
  fit_and_predict <- function(df, predict_duration, monthly_ind) {
    
    # --- ROBUSTNESS: Add a tryCatch block for modeling ---
    # This ensures that if modeling fails for one commodity, the app doesn't crash.
    tryCatch({
      
      # Store start month for later use
      start_month <- na.omit(unique(df$year_start_month))
      
      if (monthly_ind == "No") {
        # --- Annual Data Modeling ---
        
        # --- ROBUSTNESS: Validate data before modeling ---
        if(nrow(df) < 3 || sum(!is.na(df$value)) < 3 || sum(!is.na(df$year_num)) < 3) {
          stop("Not enough valid data points to generate a forecast.")
        }
        
        trend_line <- lm(value ~ year_num, data = df)
        
        # Set up prediction frame
        start_num <- length(df$year_value_ex)
        max_year <- max(df$year_value_ex)
        
        predict_years <- data.frame(
          year_value_ex = seq(max_year + 1, (max_year + predict_duration), 1)
        ) %>% 
          mutate(year_num = seq(start_num + 1, start_num + predict_duration, 1))
        
        # Generate predictions with confidence intervals
        preds <- predict(trend_line, newdata = predict_years, interval = "prediction", level = 0.95) %>% as.data.frame()
        preds_80 <- predict(trend_line, newdata = predict_years, interval = "prediction", level = 0.80) %>% as.data.frame()
        
        predict_years <- predict_years %>%
          mutate(
            predict_tl = preds$fit,
            predict_lwr_95 = preds$lwr,
            predict_upr_95 = preds$upr,
            predict_lwr_80 = preds_80$lwr,
            predict_upr_80 = preds_80$upr
          )

        # Format year values
        if (unique(df$year_unit) == "Marketing year") {
          predict_years <- predict_years %>%  
            mutate(year_value = paste(year_value_ex, "/", str_extract(year_value_ex + 1, "..$"), sep = ""))
        } else {
          predict_years <- predict_years %>%  
            mutate(year_value = as.character(year_value_ex))
        }
        
        predict_years_formatted <- predict_years %>% select(year_value_ex, starts_with("predict_"))
        
        result_data <- df %>%
          full_join(predict_years_formatted, by = "year_value_ex")
        
        # Return a list with data and model summary
        return(list(
          data = result_data,
          model_summary = broom::glance(trend_line)
        ))
        
      } else {
        # --- Monthly Data Modeling ---
        
        # --- ROBUSTNESS: Validate data before modeling ---
        if(nrow(df) < 12 || sum(!is.na(df$value)) < 12) { # Heuristic check for monthly data
          stop("Not enough valid data points for a monthly forecast.")
        }

        formula <- df %>% 
          select(starts_with("month_")) %>% 
          names() %>% 
          paste(collapse = " + ") %>% 
          paste("value ~ year_value_ex +", .) %>% 
          as.formula()
        
        trend_line <- lm(formula, data = df)
        
        start_num <- df %>% summarize(max(date)) %>% pull()
        
        predict_data <- data.frame(
          date = seq.Date(from = start_num, by = "month", length.out = predict_duration * 12)
        ) %>% 
          slice(-1) %>% 
          mutate(year_value_ex = year(date), month_num = month(date), month = month.name[month_num]) %>%
          filter(month %in% unique(df$month))
        
        predict_data <- fastDummies::dummy_cols(predict_data, select_columns = "month", remove_first_dummy = FALSE)
        
        # Generate predictions with intervals
        preds <- predict(trend_line, newdata = predict_data, interval = "prediction", level = 0.95) %>% as.data.frame()
        preds_80 <- predict(trend_line, newdata = predict_data, interval = "prediction", level = 0.80) %>% as.data.frame()
        
        predictions <- predict_data %>%
          mutate(
            predict_tl = preds$fit,
            predict_lwr_95 = preds$lwr,
            predict_upr_95 = preds$upr,
            predict_lwr_80 = preds_80$lwr,
            predict_upr_80 = preds_80$upr
          )
        
        if (unique(df$year_unit) == "Marketing year") {
            # marketing year logic
        } else {
            predictions <- predictions %>% mutate(year_value = as.character(year_value_ex))
        }

        predict_tl <- predictions %>% select(date, starts_with("predict_"))
        
        result_data <- df %>%
          select(-starts_with("month_")) %>%
          full_join(predict_tl, by = "date")
        
        return(list(
          data = result_data,
          model_summary = broom::glance(trend_line)
        ))
      }
      
    }, error = function(e) {
      # If modeling fails, return the original data with a warning.
      # This prevents the entire analysis from crashing.
      warning(paste("Could not generate forecast for:", unique(df$commodity_element), "-", e$message))
      return(list(data = df, model_summary = NULL))
    })
    # --- END ROBUSTNESS BLOCK ---
  }

  # Apply the modeling function to each nested data frame
  model_outputs <- nested_data %>%
    mutate(model_fit = purrr::map(data, ~fit_and_predict(.x, predict_duration, monthly_ind)))

  # Unnest the results to get a final data frame
  final_data <- model_outputs %>%
    mutate(data = purrr::map(model_fit, "data")) %>%
    select(commodity_element, data) %>%
    tidyr::unnest(cols = c(data))

  # Extract model summaries for each commodity
  model_summaries <- model_outputs %>%
    mutate(summary = purrr::map(model_fit, "model_summary")) %>%
    select(commodity_element, summary) %>%
    tidyr::unnest(cols = c(summary))
    
  # --- END NESTED MODELING ---

  # Fill in metadata for predicted rows
  final_data <- final_data %>%
    group_by(commodity_element) %>%
    tidyr::fill(
      year_unit, year_start_month, variable, market_segment, 
      geographic_extent, unit, .direction = "downup"
    ) %>%
    ungroup()
  
  # Return results
  return(list(
    data = final_data,
    model_summaries = model_summaries,
    monthly = (monthly_ind == "Yes")
  ))
}

#' Create visualization of time series data and predictions
#'
#' This function creates plots of the original data and predictions.
#'
#' @param model_results Results from model_time_series()
#' @param commodity Commodity name (can be a vector for multiple)
#' @param variable Variable name
#' @param year_unit Year unit
#'
#' @return A ggplot object
visualize_predictions <- function(model_results,
                                commodity = NULL,
                                variable = NULL,
                                year_unit = NULL) {
  
  plot_data <- model_results$data
  is_monthly <- model_results$monthly
  
  # --- ROBUSTNESS: Check if forecast data is available ---
  has_forecast <- "predict_tl" %in% names(plot_data) && any(!is.na(plot_data$predict_tl))
  
  # Determine the primary x-axis aesthetic
  x_aes <- if (is_monthly) "date" else "year_value_ex"
  
  # Build custom hover text column
  plot_data <- plot_data %>%
    dplyr::mutate(
      hover = paste0(
        commodity_element, "<br>",
        "Time Period: ", .data[[x_aes]], "<br>",
        "Historical Value: ", round(value, 2), " ", unit
      ),
      forecast_hover = if (has_forecast) {
        paste0(
          commodity_element, "<br>",
          "Time Period: ", .data[[x_aes]], "<br>",
          "Forecast Value: ", round(predict_tl, 2), " ", unit, "<br>",
          "80% Interval: [", round(predict_lwr_80, 2), " - ", round(predict_upr_80, 2), "]<br>",
          "95% Interval: [", round(predict_lwr_95, 2), " - ", round(predict_upr_95, 2), "]"
        )
      } else {
        NULL
      }
    )
  
  # --- ROBUST PLOTTING LOGIC ---
  # Base plot with aesthetics common to all scenarios
  p <- ggplot(plot_data, aes(x = .data[[x_aes]], group = commodity_element, text = hover))
  
  # --- INTELLIGENT PLOTTING: Only add forecast layers if data exists ---
  if (has_forecast) {
    # Add prediction interval ribbons
    p <- p + geom_ribbon(aes(ymin = predict_lwr_95, ymax = predict_upr_95, fill = commodity_element), alpha = 0.2) +
             geom_ribbon(aes(ymin = predict_lwr_80, ymax = predict_upr_80, fill = commodity_element), alpha = 0.3)
    
    # Add the dashed line for the forecast with its own hover text
    p <- p + geom_line(aes(y = predict_tl, color = commodity_element, text = forecast_hover), linetype = "dashed", linewidth = 1)
  }
  
  # Add the line for the actual historical data
  p <- p + geom_line(aes(y = value, color = commodity_element), linewidth = 1)
  
  # --- PLOT STYLING ---
  # Construct title and caption
  title_text <- if (length(unique(plot_data$commodity_element)) > 1) {
    paste("Time Series Analysis for Multiple Commodities")
  } else {
    paste("Time Series Analysis for", unique(plot_data$commodity_element))
  }
  
  caption_text <- paste(
    "Data source: USDA, Economic Research Service, Fruits and Tree Nuts Yearbook.",
    "\nAnalysis by ERS.",
    if (has_forecast) {
      "\nNote: Solid lines are historical data; dashed lines are forecasts."
    } else {
      "\nNote: Forecast could not be generated for this variable."
    },
    if (has_forecast) "\nShaded areas represent 80% and 95% prediction intervals." else ""
  )
  
  # Apply theme and labels
  p <- p +
    ers_theme() +
    labs(
      title = title_text,
      subtitle = paste("Variable:", variable),
      x = "Year",
      y = unique(plot_data$unit)[1],
      caption = caption_text,
      color = "Commodity", # Legend title for color
      fill = "Commodity"  # Legend title for fill
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(hjust = 0)
    )
    
  # Use a color scale that works well for multiple lines
  if (length(unique(plot_data$commodity_element)) > 1) {
    p <- p + scale_color_brewer(palette = "Paired") +
             scale_fill_brewer(palette = "Paired")
  } else {
    # For a single commodity, we can use specific colors
    p <- p + scale_color_manual(values = c("#005398")) +
             scale_fill_manual(values = c("#005398"))
  }
  
  return(ggplotly(p, tooltip = "text"))
} 