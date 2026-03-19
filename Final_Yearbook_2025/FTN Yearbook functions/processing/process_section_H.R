#' FTN Yearbook Table Processing: Section H
#' 
#' This script processes Section H tables from the FTN Yearbook.
#' 


# Source utility functions
source("FTN Yearbook functions/processing/utils.R")

#' Process all Section H tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section H data
process_section_H <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section H
  max_tables <- 5
  
  # Lists for special case handling specific to Section H
  miscellaneous_tables <- c(1, 2, 3, 4, 5)
  
  # Initialize result list and combined data frame
  H_list <- list()
  H_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table H-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("H-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table H-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data - Section H tables often have unique structures
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = paste0("H", table_num))
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = paste0("H", table_num))
    
    # Section H tables often contain miscellaneous data not covered in other sections
    # Set a category field to help classify the data
    if (str_detect(metadata$title, "nutrition|Nutrition|nutrient|Nutrient")) {
      table_data$category <- "Nutrition"
    } else if (str_detect(metadata$title, "Health|health|disease|Disease")) {
      table_data$category <- "Health"
    } else if (str_detect(metadata$title, "Consumer|consumer|consumption|Consumption")) {
      table_data$category <- "Consumer"
    } else if (str_detect(metadata$title, "demographic|Demographic|population|Population")) {
      table_data$category <- "Demographics"
    } else {
      table_data$category <- "Other"
    }
    
    # Add a note about the miscellaneous nature of Section H
    table_data$section_note <- "Miscellaneous tables related to fruit and tree nuts"
    
    # Store in list and append to combined data
    H_list[[table_num]] <- table_data
    
    if (is.null(H_append)) {
      H_append <- table_data
    } else {
      # Make sure tables have the same columns before binding
      all_columns <- unique(c(names(H_append), names(table_data)))
      
      # Add missing columns to H_append
      for (col in all_columns) {
        if (!col %in% names(H_append)) {
          H_append[[col]] <- NA
        }
        if (!col %in% names(table_data)) {
          table_data[[col]] <- NA
        }
      }
      
      H_append <- rbind(H_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("H-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section H data
  section_path <- file.path(output_dir, "H_flat.csv")
  save_to_csv(H_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section H data to %s", section_path))
    message(sprintf("Processed %d Section H tables", length(H_list)))
  }
  
  # Return the combined data
  H_append
}

# If this script is run directly, process Section H tables
if (!interactive()) {
  # Get paths
  paths <- get_yearbook_paths()
  
  # Default yearbook file path
  yearbook_file <- file.path(paths$input, "Yearbook_2024_app.xlsm")
  
  # Check if file exists
  if (!file.exists(yearbook_file)) {
    stop(sprintf("Yearbook file not found at %s", yearbook_file))
  }
  
  # Process Section H
  H_data <- process_section_H(yearbook_file, paths$output, save_individual = TRUE)
  
  # Print summary
  cat(sprintf("Processed %d rows from Section H tables\n", nrow(H_data)))
} 