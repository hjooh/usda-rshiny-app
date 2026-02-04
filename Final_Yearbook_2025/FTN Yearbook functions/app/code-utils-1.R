#' FTN Yearbook Analysis Utilities
#' 
#' This file contains common utility functions used across the FTN Yearbook analysis scripts.
#' 

# Load required packages with error handling
#' @description Load required packages with error handling
#' @param pkg_list Character vector of package names to load
#' @return Invisible TRUE if all packages loaded successfully, otherwise stops with error
load_required_packages <- function(pkg_list) {
  for (pkg in pkg_list) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed. Please install it with install.packages('", pkg, "')"), call. = FALSE)
    }
    library(pkg, character.only = TRUE)
  }
  return(invisible(TRUE))
}

#' @description Safely read the FTN Yearbook data with appropriate data type conversion
#' @param file_path Path to the CSV or Excel file containing the FTN Yearbook data
#' @return A data frame containing the FTN Yearbook data
read_ftn_data <- function(file_path) {
  # Determine file type based on extension
  file_ext <- tolower(tools::file_ext(file_path))
  
  normalize_yearbook_schema <- function(df) {
    # Normalize column names from processing output
    if ("time_value" %in% names(df) && !"year_value" %in% names(df)) {
      df <- df %>% dplyr::rename(year_value = time_value)
    }
    if ("time_unit" %in% names(df) && !"year_unit" %in% names(df)) {
      df <- df %>% dplyr::rename(year_unit = time_unit)
    }
    
    # Fill missing year_unit for compatibility
    if (!"year_unit" %in% names(df)) {
      df$year_unit <- "Calendar year"
    }
    
    # Normalize year_unit values from processing output
    df$year_unit <- ifelse(df$year_unit == "Year", "Calendar year", df$year_unit)
    
    # Provide a default year_start_month if missing
    if (!"year_start_month" %in% names(df)) {
      df$year_start_month <- ifelse(df$year_unit == "Marketing year", "October", "January")
    }
    
    return(df)
  }
  
  if (file_ext == "csv") {
    yearbook <- suppressWarnings(
      read.csv(file_path, header = TRUE) %>% 
        as.data.frame()
    )
  } else if (file_ext %in% c("xlsx", "xls")) {
    yearbook <- suppressWarnings(
      readxl::read_excel(file_path) %>% 
        as.data.frame()
    )
  } else {
    stop("Unsupported file format. Please provide a CSV or Excel file.")
  }
  
  yearbook <- yearbook %>%
    normalize_yearbook_schema() %>%
    mutate(
      year_value = trimws(as.character(year_value)),
      year_value = ifelse(year_value %in% c("", "NA"), NA, year_value)
    ) %>%
    mutate(
      year_value_ex = suppressWarnings(as.numeric(stringr::str_remove(year_value, "/.*"))),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    filter(!is.na(value), !is.na(year_value_ex))
  
  return(yearbook)
}

#' @description Format a plot with consistent styling for FTN Yearbook analysis
#' @param plot A ggplot object to format
#' @param title The plot title
#' @param y_unit The y-axis label
#' @param x_label The x-axis label
#' @param caption The caption text
#' @return A formatted ggplot object
format_ftn_plot <- function(plot, title, y_unit, x_label, caption) {
  plot +
    ers_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 12),
      plot.caption = element_text(hjust = 0, size = 8),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = x_label, 
      y = y_unit, 
      title = title, 
      caption = caption
    )
}

#' @description Write outputs (data and plots) to files
#' @param data Data frame to write to CSV
#' @param plot ggplot object to save
#' @param filename Base filename to use (without extension)
#' @param output_dir Directory to save files in (default: current working directory)
save_outputs <- function(data, plot, filename, output_dir = getwd()) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save data to CSV
  csv_path <- file.path(output_dir, paste0(filename, ".csv"))
  write.csv(data, file = csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Save plot
  plot_path <- file.path(output_dir, paste0(filename, ".png"))
  ggsave(filename = plot_path, plot = plot)
  
  message(paste("Results saved to:", output_dir))
  message(paste("  - Data:", csv_path))
  message(paste("  - Plot:", plot_path))
} 