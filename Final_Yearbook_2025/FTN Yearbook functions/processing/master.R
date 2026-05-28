#' FTN Yearbook Master Processing Script
#' 
#' This script orchestrates the processing of all sections of the FTN Yearbook.
#' 


# Source utility functions
source("FTN Yearbook functions/processing/utils.R")

# Source all section processors
source("FTN Yearbook functions/processing/process_section_A.R")
source("FTN Yearbook functions/processing/process_section_B.R")
source("FTN Yearbook functions/processing/process_section_C.R")
source("FTN Yearbook functions/processing/process_section_D.R")
source("FTN Yearbook functions/processing/process_section_E.R")
source("FTN Yearbook functions/processing/process_section_F.R")
source("FTN Yearbook functions/processing/process_section_G.R")
source("FTN Yearbook functions/processing/process_section_H.R")

#' Process all yearbook sections
#'
#' @param yearbook_file Path to the yearbook Excel file
#' @param output_dir Directory to save output files
#' @param save_individual Whether to save individual table data (default: FALSE)
#' @param quiet Whether to suppress messages (default: FALSE)
#' @param sections Which sections to process (default: all)
#' @return A list containing all processed section data
process_yearbook <- function(yearbook_file, output_dir = NULL, save_individual = FALSE, 
                             quiet = FALSE, sections = c("A", "B", "C", "D", "E", "F", "G", "H")) {
  
  # Load required packages
  load_required_packages()
  
  # Set up paths
  if (is.null(output_dir)) {
    paths <- get_yearbook_paths()
    output_dir <- paths$output
  }
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (!quiet) {
    message("Starting FTN Yearbook processing...")
    message(sprintf("Using yearbook file: %s", yearbook_file))
    message(sprintf("Output directory: %s", output_dir))
    message(sprintf("Processing sections: %s", paste(sections, collapse = ", ")))
  }
  
  # Initialize results list
  results <- list()
  
  # Process each selected section
  if ("A" %in% sections) {
    if (!quiet) message("\n--- Processing Section A ---")
    results$A <- process_section_A(yearbook_file, output_dir, save_individual, quiet)
  }
  
  if ("B" %in% sections) {
    if (!quiet) message("\n--- Processing Section B ---")
    results$B <- process_section_B(yearbook_file, output_dir, save_individual, quiet)
  }
  
  if ("C" %in% sections) {
    if (!quiet) message("\n--- Processing Section C ---")
    results$C <- process_section_C(yearbook_file, output_dir, save_individual, quiet)
  }
  
  if ("D" %in% sections) {
    if (!quiet) message("\n--- Processing Section D ---")
    results$D <- process_section_D(yearbook_file, output_dir, save_individual, quiet)
  }
  
  if ("E" %in% sections) {
    if (!quiet) message("\n--- Processing Section E ---")
    results$E <- process_section_E(yearbook_file, output_dir, save_individual, quiet)
  }
  
  if ("F" %in% sections) {
    if (!quiet) message("\n--- Processing Section F ---")
    results$F <- process_section_F(yearbook_file, output_dir, save_individual, quiet)
  }
  
  if ("G" %in% sections) {
    if (!quiet) message("\n--- Processing Section G ---")
    results$G <- process_section_G(yearbook_file, output_dir, save_individual, quiet)
  }
  
  if ("H" %in% sections) {
    if (!quiet) message("\n--- Processing Section H ---")
    results$H <- process_section_H(yearbook_file, output_dir, save_individual, quiet)
  }
  
  # Combine all sections into one master file
  if (length(results) > 0) {
    if (!quiet) message("\nCombining all processed sections...")
    
    combined_data <- NULL
    for (section_name in names(results)) {
      section_data <- results[[section_name]]
      section_data$section <- section_name
      
      if (is.null(combined_data)) {
        combined_data <- section_data
      } else {
        # Ensure consistent columns
        all_columns <- unique(c(names(combined_data), names(section_data)))
        
        for (col in all_columns) {
          if (!col %in% names(combined_data)) {
            combined_data[[col]] <- NA
          }
          if (!col %in% names(section_data)) {
            section_data[[col]] <- NA
          }
        }
        
        combined_data <- rbind(combined_data, section_data)
      }
    }
    
    # Save combined file
    combined_path <- file.path(output_dir, "FTN_Yearbook_all_sections.csv")
    save_to_csv(combined_data, combined_path)
    
    if (!quiet) {
      message(sprintf("Saved combined data to %s", combined_path))
      message(sprintf("Total rows: %d", nrow(combined_data)))
    }
    
    # Add combined data to results
    results$combined <- combined_data
  }
  
  if (!quiet) {
    message("\nFTN Yearbook processing complete!")
  }
  
  return(results)
}

# If this script is run directly
if (!interactive()) {
  # Get paths
  paths <- get_yearbook_paths()
  
  # Default yearbook file path
  yearbook_file <- file.path(paths$input, "Yearbook_2024_revised.xlsm")
  
  # Check command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Set default options
  output_dir <- paths$output
  save_individual <- FALSE
  quiet <- FALSE
  sections <- c("A", "B", "C", "D", "E", "F", "G", "H")
  
  # Parse command line arguments if provided
  if (length(args) >= 1) {
    if (file.exists(args[1])) {
      yearbook_file <- args[1]
    } else {
      stop(sprintf("Yearbook file not found: %s", args[1]))
    }
  }
  
  if (length(args) >= 2) {
    output_dir <- args[2]
  }
  
  if (length(args) >= 3) {
    save_individual <- as.logical(args[3])
  }
  
  if (length(args) >= 4) {
    sections <- unlist(strsplit(args[4], ","))
  }
  
  # Process yearbook
  results <- process_yearbook(yearbook_file, output_dir, save_individual, quiet, sections)
  
  # Print summary
  cat(sprintf("Processed %d sections from the FTN Yearbook\n", length(results) - 1))  # Subtract 1 for combined
} 
