#' FTN Yearbook Table Processing: Section G
#' 
#' This script processes Section G tables from the FTN Yearbook.
#' 


# Source utility functions
source("FTN Yearbook functions/processing/utils.R")

#' Process all Section G tables from the yearbook
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @return A data frame containing all processed Section G data
process_section_G <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, quiet = FALSE) {
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Number of tables in Section G
  max_tables <- 8
  
  # Lists for special case handling specific to Section G
  international_tables <- c(1, 2, 3, 4)
  country_comparison_tables <- c(5, 6, 7, 8)
  
  # Initialize result list and combined data frame
  G_list <- list()
  G_append <- NULL
  
  # Process each table
  for (table_num in 1:max_tables) {
    if (!quiet) {
      message(sprintf("Processing table G-%d...", table_num))
    }
    
    # Load the worksheet
    sheet_name <- sprintf("G-%d", table_num)
    sheet_data <- load_yearbook_sheet(sheet_name, yearbook_file)
    
    if (is.null(sheet_data)) {
      warning(sprintf("Skipping table G-%d (sheet not found or empty)", table_num))
      next
    }
    
    # Extract metadata
    metadata <- extract_table_metadata(sheet_data)
    
    # Find end of data
    end_row <- find_data_end(sheet_data, metadata$time_unit, special_case = table_num)
    
    # Process worksheet into long format
    table_data <- process_worksheet(sheet_data, metadata, end_row, special_case = table_num)
    
    # Handle Section G specific processing
    if (table_num == 3) {
      table_data$variable <- table_data$commodity_element
      table_data$commodity_element <- "Avocados"
      table_data$market_segment <- "Fresh"
      table_data$unit <- ifelse(
        table_data$variable == "Per capita availability",
        "pounds",
        "million pounds"
      )
    }

    if (table_num %in% international_tables) {
      # Set geographic extent for international data
      table_data$geographic_extent <- "International"
      
      # Extract country information from variable names
      if (any(str_detect(table_data$variable, fixed(":")), na.rm = TRUE)) {
        # Split country and variable
        country_var_split <- str_split_fixed(table_data$variable, ":", 2)
        table_data$country <- trimws(country_var_split[, 1])
        table_data$variable <- trimws(country_var_split[, 2])
      }
    }
    
    if (table_num %in% country_comparison_tables) {
      # Set comparison flag
      table_data$is_comparison <- TRUE
      
      # Extract countries from title if available
      countries_match <- str_match(metadata$title, "Comparison[s]* between ([\\w\\s,]+) and ([\\w\\s]+)")
      if (!is.na(countries_match[1, 1])) {
        # Extract country1 and country2 information
        table_data$comparison_country1 <- trimws(countries_match[1, 2])
        table_data$comparison_country2 <- trimws(countries_match[1, 3])
      }
    }
    
    # Store in list and append to combined data
    G_list[[table_num]] <- table_data
    
    if (is.null(G_append)) {
      G_append <- table_data
    } else {
      # Handle possible column additions
      
      # For international tables
      if ("country" %in% names(table_data) && !"country" %in% names(G_append)) {
        G_append$country <- NA
      }
      if ("country" %in% names(G_append) && !"country" %in% names(table_data)) {
        table_data$country <- NA
      }
      
      # For comparison tables
      if ("is_comparison" %in% names(table_data) && !"is_comparison" %in% names(G_append)) {
        G_append$is_comparison <- FALSE
      }
      if ("is_comparison" %in% names(G_append) && !"is_comparison" %in% names(table_data)) {
        table_data$is_comparison <- FALSE
      }
      
      if ("comparison_country1" %in% names(table_data) && !"comparison_country1" %in% names(G_append)) {
        G_append$comparison_country1 <- NA
      }
      if ("comparison_country1" %in% names(G_append) && !"comparison_country1" %in% names(table_data)) {
        table_data$comparison_country1 <- NA
      }
      
      if ("comparison_country2" %in% names(table_data) && !"comparison_country2" %in% names(G_append)) {
        G_append$comparison_country2 <- NA
      }
      if ("comparison_country2" %in% names(G_append) && !"comparison_country2" %in% names(table_data)) {
        table_data$comparison_country2 <- NA
      }
      
      G_append <- rbind(G_append, table_data)
    }
    
    # Save individual table if requested
    if (save_individual) {
      save_path <- file.path(output_dir, sprintf("G-%d_flat.csv", table_num))
      save_to_csv(table_data, save_path)
      
      if (!quiet) {
        message(sprintf("  Saved individual table to %s", save_path))
      }
    }
  }
  
  # Save combined Section G data
  section_path <- file.path(output_dir, "G_flat.csv")
  save_to_csv(G_append, section_path)
  
  if (!quiet) {
    message(sprintf("Saved combined Section G data to %s", section_path))
    message(sprintf("Processed %d Section G tables", length(G_list)))
  }
  
  # Return the combined data
  G_append
}

