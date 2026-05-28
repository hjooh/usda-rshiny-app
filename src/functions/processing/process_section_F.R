#' FTN Yearbook Table Processing: Section F
#' 
#' This script processes Section F tables from the FTN Yearbook.
#' 


# Source utility functions
source("functions/processing/utils.R")

#' Process all Section F tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section F data
process_section_F <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section F
  max_tables <- 10
  
  # Lists for special case handling specific to Section F
  table_variable_labels <- c(
    "1" = "Area bearing",
    "2" = "Gross returns",
    "3" = "Utilized production",
    "4" = "Price received by growers",
    "5" = "Value of production"
  )
  index_tables <- c(5, 6, 7, 8)
  forecasting_tables <- c(9, 10)
  
  # Initialize result list and combined data frame
  F_list <- list()
  F_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table F-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("F-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table F-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = table_num)
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = table_num)
    
    # Handle Section F specific processing
    if (as.character(table_num) %in% names(table_variable_labels)) {
      table_data$variable <- table_variable_labels[[as.character(table_num)]]
    }
    
    if (table_num %in% index_tables) {
      # Set appropriate unit for index data
      unit_values <- ifelse(is.na(table_data$unit), "", table_data$unit)
      if (!any(str_detect(unit_values, "index"))) {
        table_data$unit <- "index"
      }
      
      # Add base year information if available in the title
      base_year_match <- str_match(metadata$title, "\\(([0-9]{4})[ =]+100\\)")
      if (!is.na(base_year_match[1,2])) {
        table_data$base_year <- base_year_match[1,2]
      }
    }
    
    if (table_num %in% forecasting_tables) {
      # Mark forecast data
      table_data$is_forecast <- TRUE
      
      # Add forecast period information
      forecast_period_match <- str_match(metadata$title, "Forecast[s]* ([0-9]{4}-[0-9]{2,4})")
      if (!is.na(forecast_period_match[1,2])) {
        table_data$forecast_period <- forecast_period_match[1,2]
      }
    }
    
    # Store in list and append to combined data
    F_list[[table_num]] <- table_data
    
    if (is.null(F_append)) {
      F_append <- table_data
    } else {
      # If we need to add columns from the current table_data
      if ("base_year" %in% names(table_data) && !"base_year" %in% names(F_append)) {
        F_append$base_year <- NA
      }
      if ("is_forecast" %in% names(table_data) && !"is_forecast" %in% names(F_append)) {
        F_append$is_forecast <- FALSE
      }
      if ("forecast_period" %in% names(table_data) && !"forecast_period" %in% names(F_append)) {
        F_append$forecast_period <- NA
      }
      
      # If we need to add columns from F_append to table_data
      if ("base_year" %in% names(F_append) && !"base_year" %in% names(table_data)) {
        table_data$base_year <- NA
      }
      if ("is_forecast" %in% names(F_append) && !"is_forecast" %in% names(table_data)) {
        table_data$is_forecast <- FALSE
      }
      if ("forecast_period" %in% names(F_append) && !"forecast_period" %in% names(table_data)) {
        table_data$forecast_period <- NA
      }
      
      F_append <- rbind(F_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("F-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section F data
  section_path <- file.path(output_dir, "F_flat.csv")
  save_to_csv(F_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section F data to %s", section_path))
    message(sprintf("Processed %d Section F tables", length(F_list)))
  }
  
  # Return the combined data
  return(F_append)
}

