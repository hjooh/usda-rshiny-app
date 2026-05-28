#' FTN Yearbook Table Processing: Section C
#' 
#' This script processes Section C tables from the FTN Yearbook.
#' 


# Source utility functions
source("FTN Yearbook functions/processing/utils.R")

#' Process all Section C tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section C data
process_section_C <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section C
  max_tables <- 19
  
  # Lists for special case handling specific to Section C
  price_tables <- c(1, 2, 3, 4, 5, 6)
  import_export_tables <- c(10, 11, 12, 13, 14, 15, 16)
  
  # Initialize result list and combined data frame
  C_list <- list()
  C_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table C-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("C-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table C-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = table_num)
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = table_num)
    
    # Handle Section C specific processing
    if (table_num %in% price_tables) {
      # Special handling for price tables
      table_data$variable <- paste("Price", table_data$variable)
    }
    
    if (table_num %in% import_export_tables) {
      # Ensure consistent geographic extent for import/export tables
      table_data$geographic_extent <- "United States"
    }
    
    # Store in list and append to combined data
    C_list[[table_num]] <- table_data
    
    if (is.null(C_append)) {
      C_append <- table_data
    } else {
      C_append <- rbind(C_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("C-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section C data
  section_path <- file.path(output_dir, "C_flat.csv")
  save_to_csv(C_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section C data to %s", section_path))
    message(sprintf("Processed %d Section C tables", length(C_list)))
  }
  
  # Return the combined data
  return(C_append)
}

