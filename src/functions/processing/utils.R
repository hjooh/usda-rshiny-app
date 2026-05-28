#' FTN Yearbook Table Processing Utilities
#' 
#' This file contains utility functions used across the FTN Yearbook table processing scripts.
#' 


# Load required packages
load_required_packages <- function() {
  packages <- c(
    "tidyverse", "lubridate", "plotly", "reshape", "dplyr",
    "readxl", "writexl", "openxlsx", "stringr"
  )
  
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

#' Get standardized paths for the FTN Yearbook processing
#'
#' @param custom_base_dir Optional custom base directory path
#' @return A list containing input, output, and program directories
get_yearbook_paths <- function(custom_base_dir = NULL) {
  if (is.null(custom_base_dir)) {
    # Try to detect a sensible default location
    if (dir.exists("C:/Users/clair/OneDrive/Desktop/src")) {
      base_dir <- "C:/Users/clair/OneDrive/Desktop/src"
    } else {
      # Use the current directory as fallback
      base_dir <- getwd()
    }
  } else {
    base_dir <- custom_base_dir
  }
  
  # Create the paths
  paths <- list(
    input = file.path(base_dir, "Input"),
    output = file.path(base_dir, "Output"),
    programs = file.path(base_dir, "functions/processing")
  )
  
  # Create directories if they don't exist
  for (path in paths) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  return(paths)
}

#' Load an Excel worksheet from the Yearbook file
#'
#' @param sheet_name Name of the sheet to load (e.g., "A-1")
#' @param yearbook_file Path to the yearbook Excel file
#' @return A data frame with the raw worksheet data
load_yearbook_sheet <- function(sheet_name, yearbook_file) {
  tryCatch({
    sheet_data <- readxl::read_excel(yearbook_file, sheet = sheet_name, col_names = FALSE)
    return(sheet_data)
  }, error = function(e) {
    message(paste("Error loading sheet", sheet_name, ":", e$message))
    return(NULL)
  })
}

#' Extract title, commodity, and time unit from a worksheet
#'
#' @param sheet_data Raw worksheet data
#' @return A list containing title, commodity, time_unit, and market information
extract_table_metadata <- function(sheet_data) {
  # Extract and clean the title
  title <- as.character(sheet_data[1, 1])
  title <- str_remove(title, "[[:alnum:]], [[:alnum:]]$")
  title <- str_remove(title, "[[:digit:]]$")
  
  # Extract time unit
  time_p <- sub("[[:digit:]]$", "", as.character(sheet_data[2, 1]))
  
  # Extract commodity
  begin_commodity <- (str_locate(title, "--")[2]) + 1
  end_commodity <- (str_locate(title, ",")[2]) - 1
  
  # Handle cases where pattern doesn't match
  if (is.na(begin_commodity) || is.na(end_commodity) || end_commodity < begin_commodity) {
    crop <- "Unknown"
  } else {
    crop <- substr(title, start = begin_commodity, stop = end_commodity)
  }
  
  # Determine market segment
  market <- "All"
  if (str_detect(title, "fresh ") || str_detect(title, "Fresh ") || str_detect(title, "fresh:")) {
    market <- "Fresh"
  }
  
  return(list(
    title = title,
    commodity = crop,
    time_unit = time_p,
    market = market
  ))
}

#' Find the end of data in a worksheet
#'
#' @param sheet_data Raw worksheet data
#' @param time_unit Time unit (e.g., "Calendar year", "Marketing year")
#' @param special_case Optional parameter for special cases
#' @return The row index of the end of data
find_data_end <- function(sheet_data, time_unit, special_case = NULL) {
  # Default to looking for 2023
  year_pattern <- "2023"
  
  # Handle missing time_unit safely
  if (is.na(time_unit)) {
    time_unit <- ""
  }
  
  if (time_unit == "Marketing year") {
    year_pattern <- "2023/24"
  }
  
  # Special cases
  if (!is.null(special_case) && special_case %in% c(13, 14, 15)) {
    year_pattern <- "2022/23"
  }
  
  # Find rows containing the pattern
  matching_rows <- which(str_detect(sheet_data[[1]], year_pattern))
  
  # If no matching rows found, return the last row
  if (length(matching_rows) == 0) {
    return(nrow(sheet_data))
  }
  
  # For special cases, use the last match
  if (!is.null(special_case) && special_case %in% c(13, 14, 15)) {
    return(matching_rows[length(matching_rows)])
  }
  
  return(matching_rows[1])
}

#' Clean column names in a data frame
#'
#' @param names Vector of column names to clean
#' @param skip_simple_cleanup Boolean to skip simple cleanup steps
#' @return A vector of cleaned column names
clean_column_names <- function(names, skip_simple_cleanup = FALSE) {
  # Normalize to plain character values
  names <- as.character(names)
  names <- trimws(names)
  
  # Replace parentheses with underscores
  names <- sub(" \\(", "_", names)
  
  # Remove various suffixes
  names <- sub("\\)[[:digit:]],[[:digit:]],[[:digit:]]$", "", names)
  names <- sub("\\)[[:digit:]],[[:digit:]]$", "", names)
  names <- sub("\\)[[:digit:]][[:digit:]]$", "", names)
  names <- sub("\\)[[:digit:]]$", "", names)
  names <- sub("\\)", "", names)
  
  if (!skip_simple_cleanup) {
    names <- sub("[[:digit:]],[[:digit:]]$", "", names)
    names <- sub("[[:digit:]]", "", names)
  }
  
  # Fill missing or blank names to avoid NA columns
  missing_idx <- which(is.na(names) | names == "")
  if (length(missing_idx) > 0) {
    names[missing_idx] <- paste0("unnamed_", missing_idx)
  }
  
  # Set first column to time_value and ensure uniqueness
  names[1] <- "time_value"
  names <- make.unique(names, sep = "_")
  
  return(names)
}

#' Process a worksheet into a long format data frame
#'
#' @param sheet_data Raw worksheet data
#' @param metadata Table metadata (title, commodity, time_unit, market)
#' @param end_row Row index of the end of data
#' @param special_case Optional parameter for special cases
#' @return A processed data frame in long format
process_worksheet <- function(sheet_data, metadata, end_row, special_case = NULL) {
  # Extract data portion
  data_portion <- as.data.frame(sheet_data[2:end_row, ])
  
  # Clean and assign column names
  names <- as.character(data_portion[1, ])
  names <- clean_column_names(names, special_case == 6)
  colnames(data_portion) <- names
  
  # Remove header row
  data_portion <- data_portion[-1, ]
  
  # Convert to long format
  long_data <- pivot_longer(data_portion, cols = -time_value, names_to = "variable", values_to = "value")
  
  # Separate variable and unit
  long_data <- long_data %>% 
    separate_wider_delim(variable, delim = "_", names = c("variable", "unit"), too_few = "align_start")
  
  # Ensure expected columns exist before mutation/selection
  if (!"unit" %in% names(long_data)) {
    long_data$unit <- NA
  }
  
  # Add metadata
  long_data <- long_data %>%
    mutate(
      time_value = str_remove(time_value, "\\s[0-9],[0-9]"),
      time_value = str_remove(time_value, "\\s[0-9]*"),
      table_name = metadata$title,
      time_unit = metadata$time_unit,
      commodity_element = metadata$commodity,
      value = as.numeric(value),
      geographic_extent = "National",
      month = NA,
      market_segment = metadata$market
    ) %>%
    {
      required_cols <- c(
        "table_name", "time_value", "time_unit", "month", "variable",
        "commodity_element", "market_segment", "geographic_extent", "value", "unit"
      )
      for (col in required_cols) {
        if (!col %in% names(.)) .[[col]] <- NA
      }
      select(., all_of(required_cols))
    }
  
  # Handle special cases based on table number
  if (!is.null(special_case)) {
    long_data <- handle_special_case(long_data, special_case, metadata$title)
  }
  
  return(long_data)
}

#' Handle special cases for different table types
#'
#' @param data Processed data frame
#' @param case_number The case number (table number)
#' @param title The table title
#' @return Modified data frame
handle_special_case <- function(data, case_number, title) {
  # Ensure baseline columns exist to avoid select errors
  base_cols <- c(
    "table_name", "time_value", "time_unit", "month", "variable",
    "commodity_element", "market_segment", "geographic_extent", "value", "unit"
  )
  for (col in base_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- NA
    }
  }
  
  # Multi commodity lists (tables 1, 2, 4, 5, 7)
  if (case_number %in% c(1, 2, 4, 5, 7)) {
    begin_var <- (str_locate(title, ":")[2]) + 2
    end_var <- (str_locate(title, ",")[2]) - 1
    
    # Special handling for case 7
    if (case_number == 7) {
      begin_var <- (str_locate(title, ":")[2]) + 2
      all_commas <- str_locate_all(title, ",")[[1]]
      if (nrow(all_commas) >= 2) {
        end_var <- all_commas[2, 2] - 1
      }
    }
    
    # Extract variable
    if (!is.na(begin_var) && !is.na(end_var) && end_var > begin_var) {
      var <- substr(title, start = begin_var, stop = end_var)
    } else {
      var <- "Unknown"
    }
    
    # Swap variable and commodity
    data <- data %>%
      select(table_name, time_value, time_unit, month, variable = commodity_element, 
             commodity_element = variable, market_segment, geographic_extent, value, unit) %>%
      mutate(variable = var)
  }
  
  # Multi commodity var lists (table 3)
  else if (case_number == 3) {
    data <- data %>% 
      separate_wider_delim(variable, delim = ", ", names = c("commodity", "variable"),
                           too_many = "merge", too_few = "align_start")
    
    if (!"commodity" %in% names(data)) {
      data$commodity <- NA
    }
    if (!"variable" %in% names(data)) {
      data$variable <- NA
    }
    
    data <- data %>%
      select(table_name, time_value, time_unit, month, variable, commodity_element = commodity, 
             market_segment, geographic_extent, value, unit) %>%
      mutate(
        variable = sub("\\b(\\w)", "\\U\\1", variable, perl = TRUE),
        commodity_element = sub("\\b(\\w)", "\\U\\1", commodity_element, perl = TRUE)
      )
  }
  
  # Market segment lists (table 1)
  else if (case_number == 1) {
    data <- data %>% 
      separate_wider_delim(commodity_element, delim = ", ", names = c("commodity_element", "market"),
                           too_few = "align_start")
    
    if (!"market" %in% names(data)) {
      data$market <- NA
    }
    
    data <- data %>%
      select(table_name, time_value, time_unit, month, variable, commodity_element, 
             market_segment = market, geographic_extent, value, unit) %>%
      mutate(market_segment = sub("\\b(\\w)", "\\U\\1", market_segment, perl = TRUE))
  }
  
  # Multi var market lists (table 6)
  else if (case_number == 6) {
    begin_commodity <- (str_locate(title, "--")[2]) + 1
    end_commodity <- (str_locate(title, ":")[2]) - 1
    
    if (!is.na(begin_commodity) && !is.na(end_commodity) && end_commodity > begin_commodity) {
      crop <- substr(title, start = begin_commodity, stop = end_commodity)
    } else {
      crop <- "Unknown"
    }
    
    data <- data %>% 
      separate_wider_delim(variable, delim = ", ", names = c("variable", "market"),
                           too_many = "merge", too_few = "align_start")
    
    if (!"market" %in% names(data)) {
      data$market <- NA
    }
    if (!"variable" %in% names(data)) {
      data$variable <- NA
    }
    
    data <- data %>%
      select(table_name, time_value, time_unit, month, variable, commodity_element, 
             market_segment = market, geographic_extent, value, unit) %>%
      mutate(
        market_segment = sub("\\b(\\w)", "\\U\\1", market_segment, perl = TRUE),
        commodity_element = crop
      )
  }
  
  return(data)
}

#' Save data to CSV file
#'
#' @param data Data frame to save
#' @param file_path Path to save the file
#' @param backup_existing Whether to backup an existing file (default: TRUE)
#' @return TRUE if successful, FALSE otherwise
save_to_csv <- function(data, file_path, backup_existing = TRUE) {
  # Check if file exists and backup if requested
  if (file.exists(file_path) && backup_existing) {
    backup_path <- paste0(file_path, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(file_path, backup_path)
    message(paste("Backed up existing file to:", backup_path))
  }
  
  # Write data to CSV
  write.csv(data, file = file_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  return(TRUE)
} 