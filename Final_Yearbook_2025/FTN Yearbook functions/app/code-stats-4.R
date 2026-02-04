#' FTN Yearbook Summary Statistics Function
#' 
#' This file contains functions to generate summary statistics for variables in the FTN Yearbook dataset.
#' 

# Load required packages and utilities
source("code-utils-1.R")

#' Calculate and format summary statistics for a range of variables
#'
#' This function calculates common descriptive statistics (mean, min, max, median, standard deviation, 
#' and count of non-missing values) for all columns between start_var and end_var in the input dataframe.
#' It can optionally group by another variable.
#'
#' @param df A data frame containing the variables to summarize
#' @param start_var The first variable in the range to summarize
#' @param end_var The last variable in the range to summarize
#' @param group_nm Optional grouping variable name (NULL for no grouping)
#' @param digits Number of decimal places for formatting (default: 2)
#' @param scientific Whether to use scientific notation (default: FALSE)
#'
#' @return A data frame of summary statistics, with one row per variable 
#'         (or group-variable combination if grouping is used)
#'
#' @examples
#' # Without grouping
#' summary_stats(mtcars, mpg, disp)
#' 
#' # With grouping by cyl
#' summary_stats(mtcars, mpg, disp, group_nm = cyl)
summary_stats <- function(df, start_var, end_var, group_nm = NULL, digits = 2, scientific = FALSE) {
  # Ensure required packages are loaded
  required_packages <- c("dplyr", "tidyr", "stringr")
  suppressMessages(load_required_packages(required_packages))
  
  # Convert inputs to symbols for tidy evaluation
  start_var <- ensym(start_var)
  end_var <- ensym(end_var)
  
  # Function to calculate statistics
  calc_stats <- function(data) {
    data %>% 
      summarize(across(
        c(!!start_var:!!end_var), 
        list(
          "mean" = ~ mean(.x, na.rm = TRUE),
          "min" = ~ min(.x, na.rm = TRUE),  
          "max" = ~ max(.x, na.rm = TRUE),
          "median" = ~ median(.x, na.rm = TRUE),
          "sd" = ~ sd(.x, na.rm = TRUE),
          "count" = ~ sum(!is.na(.x))
        )
      ))
  }
  
  # Function to format results
  format_results <- function(data, group_var = NULL) {
    # Define column separator pattern based on whether we have a group
    sep_pattern <- if (is.null(group_var)) "__" else "_\\d+__"
    
    result <- data %>%
      pivot_longer(
        cols = if (is.null(group_var)) everything() else -!!group_var, 
        names_to = c("variable_name", "statistic"), 
        values_to = "value",
        names_sep = "__"
      ) %>%
      # Clean up variable names (remove auto-generated suffixes)
      mutate(variable_name = str_remove_all(variable_name, sep_pattern)) %>%
      pivot_wider(names_from = statistic, values_from = value) %>%
      # Format numeric columns
      mutate(across(
        where(is.numeric), 
        ~ format(.x, trim = FALSE, scientific = scientific, digits = digits)
      ))
    
    return(result)
  }
  
  # Process differently based on whether grouping is provided
  if (is.null(group_nm)) {
    # No grouping - process the entire dataframe
    result <- calc_stats(df) %>% format_results()
  } else {
    # With grouping - process by group
    group_var <- ensym(group_nm)
    result <- df %>%
      group_by(!!group_var) %>%
      calc_stats() %>%
      ungroup() %>%
      format_results(group_var)
  }
  
  return(result)
}

#' Generate a well-formatted table of summary statistics
#'
#' This is a wrapper function for summary_stats that provides additional formatting
#' options and features, such as titles and footnotes.
#'
#' @param df A data frame containing the variables to summarize
#' @param start_var The first variable in the range to summarize
#' @param end_var The last variable in the range to summarize
#' @param group_nm Optional grouping variable name (NULL for no grouping)
#' @param title Optional title for the summary table
#' @param footnote Optional footnote for the summary table
#' @param output_file Optional file path to save the summary as a CSV
#'
#' @return A data frame of formatted summary statistics
#'
#' @examples
#' # Without grouping
#' create_summary_table(mtcars, mpg, disp, title = "Summary of MPG and Displacement")
create_summary_table <- function(df, start_var, end_var, group_nm = NULL, 
                                title = NULL, footnote = NULL, output_file = NULL) {
  # Get basic summary statistics
  stats_table <- summary_stats(df, {{start_var}}, {{end_var}}, {{group_nm}})
  
  # Add titles and footnotes as attributes
  if (!is.null(title)) attr(stats_table, "title") <- title
  if (!is.null(footnote)) attr(stats_table, "footnote") <- footnote
  
  # Save to file if requested
  if (!is.null(output_file)) {
    write.csv(stats_table, file = output_file, row.names = FALSE)
    message(paste("Summary statistics saved to:", output_file))
  }
  
  return(stats_table)
} 