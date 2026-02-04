#' FTN Yearbook Table Processing: Section B
#' 
#' This script processes Section B tables from the FTN Yearbook.
#' 


# Source utility functions
source("FTN Yearbook functions/processing/utils.R")

#' Process all Section B tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section B data
process_section_B <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section B
  max_tables <- 24
  
  # Lists for special case handling
  # These would be customized for Section B's specific needs
  multi_commodity_list <- c(1, 2, 9, 10, 18, 19, 20)
  multi_commodity_var_list <- c(7, 8, 14, 15)
  seasonal_tables <- c(3, 4, 5, 6)
  
  # Initialize result list and combined data frame
  B_list <- list()
  B_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table B-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("B-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table B-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = table_num)
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = table_num)
    
    # Handle Section B specific processing
    if (table_num %in% seasonal_tables) {
      # Handle seasonal tables (monthly data)
      table_data$month <- "Season average"
    }
    
    # Store in list and append to combined data
    B_list[[table_num]] <- table_data
    
    if (is.null(B_append)) {
      B_append <- table_data
    } else {
      B_append <- rbind(B_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("B-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section B data
  section_path <- file.path(output_dir, "B_flat.csv")
  save_to_csv(B_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section B data to %s", section_path))
    message(sprintf("Processed %d Section B tables", length(B_list)))
  }
  
  # Return the combined data
  return(B_append)
}

# If this script is run directly, process Section B tables
if (!interactive()) {
  # Get paths
  paths <- get_yearbook_paths()
  
  # Default yearbook file path
  yearbook_file <- file.path(paths$input, "Yearbook_2024_app.xlsm")
  
  # Check if file exists
  if (!file.exists(yearbook_file)) {
    stop(sprintf("Yearbook file not found at %s", yearbook_file))
  }
  
  # Process Section B
  B_data <- process_section_B(yearbook_file, paths$output, save_individual = TRUE)
  
  # Print summary
  cat(sprintf("Processed %d rows from Section B tables\n", nrow(B_data)))
} 