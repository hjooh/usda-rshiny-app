#' FTN Yearbook Table Processing: Section E
#' 
#' This script processes Section E tables from the FTN Yearbook.
#' 


# Source utility functions
source("FTN Yearbook functions/processing/utils.R")

#' Process all Section E tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section E data
process_section_E <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section E
  max_tables <- 12
  
  # Lists for special case handling specific to Section E
  consumption_tables <- c(1, 2, 3, 4)
  utilization_tables <- c(5, 6, 7, 8)
  market_share_tables <- c(9, 10, 11, 12)
  
  # Initialize result list and combined data frame
  E_list <- list()
  E_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table E-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("E-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table E-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = table_num)
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = table_num)
    
    # Handle Section E specific processing
    if (table_num %in% consumption_tables) {
      # Mark consumption data specifically
      if (!str_detect(table_data$variable[1], "Consumption")) {
        table_data$variable <- paste("Consumption", table_data$variable)
      }
      
      # Ensure per capita measure is properly identified
      if (str_detect(metadata$title, "per capita")) {
        table_data$unit <- paste(table_data$unit, "per capita")
      }
    }
    
    if (table_num %in% utilization_tables) {
      # Mark utilization data specifically
      if (!str_detect(table_data$variable[1], "Utilization")) {
        table_data$variable <- paste("Utilization", table_data$variable)
      }
    }
    
    if (table_num %in% market_share_tables) {
      # Ensure market share measure is properly identified
      unit_values <- ifelse(is.na(table_data$unit), "", table_data$unit)
      if (!any(str_detect(unit_values, "percent")) && !any(str_detect(unit_values, "%"))) {
        table_data$unit <- "percent"
      }
    }
    
    # Store in list and append to combined data
    E_list[[table_num]] <- table_data
    
    if (is.null(E_append)) {
      E_append <- table_data
    } else {
      E_append <- rbind(E_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("E-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section E data
  section_path <- file.path(output_dir, "E_flat.csv")
  save_to_csv(E_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section E data to %s", section_path))
    message(sprintf("Processed %d Section E tables", length(E_list)))
  }
  
  # Return the combined data
  return(E_append)
}

# If this script is run directly, process Section E tables
if (!interactive()) {
  # Get paths
  paths <- get_yearbook_paths()
  
  # Default yearbook file path
  yearbook_file <- file.path(paths$input, "Yearbook_2024_app.xlsm")
  
  # Check if file exists
  if (!file.exists(yearbook_file)) {
    stop(sprintf("Yearbook file not found at %s", yearbook_file))
  }
  
  # Process Section E
  E_data <- process_section_E(yearbook_file, paths$output, save_individual = TRUE)
  
  # Print summary
  cat(sprintf("Processed %d rows from Section E tables\n", nrow(E_data)))
} 