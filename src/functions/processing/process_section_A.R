#' FTN Yearbook Table Processing: Section A
#' 
#' This script processes Section A tables from the FTN Yearbook.
#' 


# Source utility functions
source("functions/processing/utils.R")

#' Process all Section A tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section A data
process_section_A <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section A
  max_tables <- 15
  
  # Lists for special case handling
  multi_commodity_list <- c(1, 2, 4, 5, 7)
  multi_commodity_var_list <- c(3)
  multi_var_mar_list <- c(6)
  market_seg_list <- c(1)
  
  # Initialize result list and combined data frame
  A_list <- list()
  A_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table A-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("A-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table A-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = table_num)
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = table_num)
    
    # Store in list and append to combined data
    A_list[[table_num]] <- table_data
    
    if (is.null(A_append)) {
      A_append <- table_data
    } else {
      A_append <- rbind(A_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("A-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section A data
  section_path <- file.path(output_dir, "A_flat.csv")
  save_to_csv(A_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section A data to %s", section_path))
    message(sprintf("Processed %d Section A tables", length(A_list)))
  }
  
  # Return the combined data
  return(A_append)
}

