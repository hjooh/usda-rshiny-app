# FTN Yearbook Table Processing: Section D
# 
# This script processes Section D tables from the FTN Yearbook.
#


# Source utility functions
source("FTN Yearbook functions/processing/utils.R")

#' Process all Section D tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section D data
process_section_D <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section D
  max_tables <- 16
  
  # Lists for special case handling specific to Section D
  state_level_tables <- c(1, 2, 3, 4, 8, 9, 10, 11, 12)
  production_tables <- c(5, 6, 7, 13, 14, 15, 16)
  
  # Initialize result list and combined data frame
  D_list <- list()
  D_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table D-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("D-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table D-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = table_num)
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = table_num)
    
    # Handle Section D specific processing
    if (table_num %in% state_level_tables) {
      # These tables may have state-level data in columns rather than National
      # Override the geographic extent extraction
      if (str_detect(metadata$title, "by State")) {
        # Extract state names from variable names
        # This is a simplification - in real code you'd parse variable names for state info
        table_data$geographic_extent <- "By State"
      }
    }
    
    if (table_num %in% production_tables) {
      # Mark production data specifically
      if (!str_detect(table_data$variable[1], "Production")) {
        table_data$variable <- paste("Production", table_data$variable)
      }
    }
    
    # Store in list and append to combined data
    D_list[[table_num]] <- table_data
    
    if (is.null(D_append)) {
      D_append <- table_data
    } else {
      D_append <- rbind(D_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("D-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section D data
  section_path <- file.path(output_dir, "D_flat.csv")
  save_to_csv(D_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section D data to %s", section_path))
    message(sprintf("Processed %d Section D tables", length(D_list)))
  }
  
  # Return the combined data
  return(D_append)
}

# If this script is run directly, process Section D tables
if (!interactive()) {
  # Get paths
  paths <- get_yearbook_paths()
  
  # Default yearbook file path
  yearbook_file <- file.path(paths$input, "Yearbook_2024_app.xlsm")
  
  # Check if file exists
  if (!file.exists(yearbook_file)) {
    stop(sprintf("Yearbook file not found at %s", yearbook_file))
  }
  
  # Process Section D
  D_data <- process_section_D(yearbook_file, paths$output, save_individual = TRUE)
  
  # Print summary
  cat(sprintf("Processed %d rows from Section D tables\n", nrow(D_data)))
} 