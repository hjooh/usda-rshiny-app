#' FTN Yearbook Data Preparation Functions
#' 
#' This file contains functions for preparing and filtering data from the FTN Yearbook dataset.
#' 

# Load required packages and utilities
source("code-utils-1.R")

#' Filter yearbook data based on selected criteria
#'
#' This function filters the FTN Yearbook data based on the specified commodity,
#' market segment, variable, geography, year unit, and value unit.
#'
#' @param yearbook Data frame containing the FTN Yearbook data
#' @param commodity Selected commodity
#' @param market_segment Selected market segment
#' @param variable Selected variable
#' @param geography Selected geography
#' @param year_unit Selected year unit
#' @param value_unit Selected value unit
#' @param interactive Whether to prompt the user for selections interactively
#'
#' @return A filtered data frame
filter_yearbook_data <- function(yearbook, 
                                commodity = NULL,
                                market_segment = NULL, 
                                variable = NULL, 
                                geography = NULL, 
                                year_unit = NULL, 
                                value_unit = NULL,
                                interactive = FALSE) {
  # Ensure required packages are loaded
  required_packages <- c("dplyr")
  suppressMessages(load_required_packages(required_packages))
  
  # Create a copy of the original data to be filtered
  yearbook_sel <- yearbook
  
  # Helper function to ensure console output is flushed before input
  ensure_visible_prompt <- function() {
    flush.console()
    Sys.sleep(0.1)
  }

  normalize_choice <- function(value, available, label) {
    if (is.null(value)) {
      return(list(value = NULL, used_fallback = FALSE))
    }
    value <- trimws(value)
    if (value %in% available) {
      return(list(value = value, used_fallback = FALSE))
    }

    # Geography-specific aliases
    if (label == "geography") {
      if (value %in% c("United States", "U.S.", "US", "USA") && "National" %in% available) {
        return(list(value = "National", used_fallback = TRUE))
      }
      if (value %in% c("State", "States") && "By State" %in% available) {
        return(list(value = "By State", used_fallback = TRUE))
      }
    }

    # If only one option is available, auto-select it
    if (length(available) == 1) {
      return(list(value = available[1], used_fallback = TRUE))
    }

    stop(sprintf(
      "Selected %s \"%s\" is not available. Available options include: %s",
      label, value, paste(available, collapse = ", ")
    ))
  }
  
  # Commodity Selection
  if (is.null(commodity) && interactive) {
    commodities <- levels(factor(yearbook_sel$commodity_element))
    cat("\nPlease select one of the following commodities:\n")
    cat(commodities, sep = "\n")
    cat("\n")
    ensure_visible_prompt()
    commodity <- readline(prompt = "The selected commodity is: ")
  }
  
  if (!is.null(commodity)) {
    # Support for single or multiple commodities
    if (!all(commodity %in% levels(factor(yearbook_sel$commodity_element)))) {
      stop(sprintf("One or more selected commodities are not in the Fruits and Tree Nuts Yearbook"))
    }
    yearbook_sel <- yearbook_sel %>% filter(commodity_element %in% commodity)
  }
  
  # Market Segment Selection
  if (is.null(market_segment) && interactive) {
    segments <- levels(factor(yearbook_sel$market_segment))
    cat("\nPlease select one of the following market segments:\n")
    cat(segments, sep = "\n")
    cat("\n")
    ensure_visible_prompt()
    market_segment <- readline(prompt = "The selected market segment is: ")
  }
  
  if (!is.null(market_segment)) {
    segments <- levels(factor(yearbook_sel$market_segment))
    norm <- normalize_choice(market_segment, segments, "market segment")
    market_segment <- norm$value
    if (isTRUE(norm$used_fallback)) {
      message(sprintf("Using market segment: %s", market_segment))
    }
    yearbook_sel <- yearbook_sel %>% filter(.data$market_segment == market_segment)
  }
  
  # Variable Selection
  if (is.null(variable) && interactive) {
    variables <- levels(factor(yearbook_sel$variable))
    cat("\nPlease select one of the following variables:\n")
    cat(variables, sep = "\n")
    cat("\n")
    ensure_visible_prompt()
    variable <- readline(prompt = "The selected variable is: ")
  }
  
  if (!is.null(variable)) {
    variables <- levels(factor(yearbook_sel$variable))
    norm <- normalize_choice(variable, variables, "variable")
    variable <- norm$value
    if (isTRUE(norm$used_fallback)) {
      message(sprintf("Using variable: %s", variable))
    }
    yearbook_sel <- yearbook_sel %>% filter(.data$variable == variable)
  }
  
  # Geography Selection
  if (is.null(geography) && interactive) {
    geographies <- levels(factor(yearbook_sel$geographic_extent))
    cat("\nPlease select one of the following geographies:\n")
    cat(geographies, sep = "\n")
    cat("\n")
    ensure_visible_prompt()
    geography <- readline(prompt = "The geographic extent selected is: ")
  }
  
  if (!is.null(geography)) {
    geographies <- levels(factor(yearbook_sel$geographic_extent))
    norm <- normalize_choice(geography, geographies, "geography")
    geography <- norm$value
    if (isTRUE(norm$used_fallback)) {
      message(sprintf("Using geography: %s", geography))
    }
    yearbook_sel <- yearbook_sel %>% filter(.data$geographic_extent == geography)
  }
  
  # Year Unit Selection
  if (is.null(year_unit) && interactive) {
    year_units <- levels(factor(yearbook_sel$year_unit))
    cat("\nPlease select one of the following year units:\n")
    cat(year_units, sep = "\n")
    cat("\n")
    ensure_visible_prompt()
    year_unit <- readline(prompt = "The year unit selected is: ")
  }
  
  if (!is.null(year_unit)) {
    year_units <- levels(factor(yearbook_sel$year_unit))
    norm <- normalize_choice(year_unit, year_units, "year unit")
    year_unit <- norm$value
    if (isTRUE(norm$used_fallback)) {
      message(sprintf("Using year unit: %s", year_unit))
    }
    yearbook_sel <- yearbook_sel %>% filter(.data$year_unit == year_unit)
  }
  
  # Value Unit Selection
  if (is.null(value_unit) && interactive) {
    units <- levels(factor(yearbook_sel$unit))
    cat("\nPlease select one of the following value units:\n")
    cat(units, sep = "\n")
    cat("\n")
    ensure_visible_prompt()
    value_unit <- readline(prompt = "The value unit selected is: ")
  }
  
  if (!is.null(value_unit)) {
    units <- levels(factor(yearbook_sel$unit))
    norm <- normalize_choice(value_unit, units, "value unit")
    value_unit <- norm$value
    if (isTRUE(norm$used_fallback)) {
      message(sprintf("Using value unit: %s", value_unit))
    }
    yearbook_sel <- yearbook_sel %>% filter(.data$unit == value_unit)
  }
  
  return(yearbook_sel)
}

#' Build indexed data for overlay plots
#'
#' @param yearbook Data frame containing normalized FTN Yearbook data
#' @param commodities Character vector of commodities to include
#' @param variables Character vector of variables to include
#' @param market_segment Optional market segment filter
#' @param geography Optional geography filter
#' @param year_unit Optional year unit filter
#' @param value_unit Optional value unit filter
#' @param year_min Optional minimum year
#' @param year_max Optional maximum year
#' @return A filtered data frame with series and index columns
build_overlay_data <- function(yearbook,
                               commodities,
                               variables,
                               market_segment = NULL,
                               geography = NULL,
                               year_unit = NULL,
                               value_unit = NULL,
                               year_min = NULL,
                               year_max = NULL) {
  required_packages <- c("dplyr")
  suppressMessages(load_required_packages(required_packages))

  empty_to_null <- function(value) {
    if (is.null(value) || length(value) == 0 || all(is.na(value)) || all(trimws(value) == "")) {
      return(NULL)
    }
    value
  }

  market_segment <- empty_to_null(market_segment)
  geography <- empty_to_null(geography)
  year_unit <- empty_to_null(year_unit)
  value_unit <- empty_to_null(value_unit)

  selections <- expand.grid(
    commodity = commodities,
    variable = variables,
    stringsAsFactors = FALSE
  )

  overlay_parts <- lapply(seq_len(nrow(selections)), function(i) {
    selection <- selections[i, ]

    tryCatch({
      filter_yearbook_data(
        yearbook,
        commodity = selection$commodity,
        market_segment = market_segment,
        variable = selection$variable,
        geography = geography,
        year_unit = year_unit,
        value_unit = value_unit,
        interactive = FALSE
      ) %>%
        dplyr::filter(
          (is.null(year_min) | .data$year_value_ex >= year_min),
          (is.null(year_max) | .data$year_value_ex <= year_max)
        ) %>%
        dplyr::arrange(.data$year_value_ex) %>%
        dplyr::mutate(series = paste(selection$commodity, selection$variable, sep = " - "))
    }, error = function(e) {
      data.frame()
    })
  })

  overlay_df <- dplyr::bind_rows(overlay_parts)

  if (nrow(overlay_df) == 0) {
    return(overlay_df)
  }

  overlay_df %>%
    dplyr::group_by(.data$series) %>%
    dplyr::arrange(.data$year_value_ex, .by_group = TRUE) %>%
    dplyr::mutate(index = .data$value / dplyr::first(.data$value) * 100) %>%
    dplyr::ungroup()
}

#' Prepare time series data for analysis
#'
#' This function processes the filtered yearbook data for time series analysis,
#' handling monthly data if requested and filtering by time period.
#'
#' @param yearbook_sel Filtered yearbook data from filter_yearbook_data()
#' @param monthly_ind Whether to use monthly data ("Yes" or "No")
#' @param year_min_sel Minimum year to include
#' @param year_max_sel Maximum year to include
#' @param interactive Whether to prompt the user for selections interactively
#'
#' @return A processed data frame ready for time series analysis
prepare_time_series <- function(yearbook_sel, 
                              monthly_ind = NULL, 
                              year_min_sel = NULL, 
                              year_max_sel = NULL,
                              interactive = FALSE) {
  # Ensure required packages are loaded
  required_packages <- c("dplyr", "tidyr", "lubridate", "stringr", "fastDummies")
  suppressMessages(load_required_packages(required_packages))
  
  # Helper function to ensure console output is flushed before input
  ensure_visible_prompt <- function() {
    flush.console()
    Sys.sleep(0.1)
  }
  
  # Determine available time range
  year_min <- yearbook_sel %>% 
    summarise(min = min(year_value_ex)) %>% 
    as.numeric()
  
  year_max <- yearbook_sel %>% 
    summarise(max = max(year_value_ex)) %>% 
    as.numeric()
  
  # Handle monthly data option
  if (is.null(monthly_ind) && interactive) {
    if (sum(!is.na(yearbook_sel$month)) > 0) {
      cat("\nNote: Monthly level data is available for this variable and commodity.\n")
      ensure_visible_prompt()
      monthly_ind <- readline(prompt = "Would you like to analyze monthly data (Yes or No)? ")
      if (!(monthly_ind %in% c("Yes", "No"))) {
        stop(sprintf("Your response of \"%s\" is neither Yes nor No.", monthly_ind))
      }
    } else {
      cat("\nNote: Monthly level data is not available for this variable and commodity.\n")
      monthly_ind <- "No"
    }
  }
  
  if (is.null(monthly_ind)) {
    # Default to No if not specified
    monthly_ind <- "No"
  }
  
  # Filter monthly data accordingly
  if (monthly_ind == "Yes") {
    yearbook_sel <- yearbook_sel %>% filter(!is.na(month))
  } else {
    yearbook_sel <- yearbook_sel %>% filter(is.na(month))
  }
  
  # Display information about the year type
  if (length(unique(yearbook_sel$year_start_month)) > 1) {
    smonths <- paste(unique(yearbook_sel$year_start_month)[1], "or", 
                    unique(yearbook_sel$year_start_month)[2])
    cat("\nNote: Every year is a ", unique(yearbook_sel$year_unit), 
        " starting in ", smonths, 
        " (consult the footnotes in the Excel version of the FTN yearbook for details).\n", 
        sep = "")
  } else { 
    cat("\nNote: Every year is a ", unique(yearbook_sel$year_unit), 
        " starting in ", unique(yearbook_sel$year_start_month), ".\n", 
        sep = "")
  }
  
  cat("\nData is available from ", year_min, " to ", year_max, ".\n", sep = "")
  
  # Handle year range selection
  if (is.null(year_min_sel) && interactive) {
    cat("\nPlease select a lower bound on the time series that you would like to analyze:\n")
    ensure_visible_prompt()
    year_min_sel <- as.numeric(readline(prompt = "The selected lower bound is: "))
  }
  
  if (!is.null(year_min_sel)) {
    if (year_min_sel < year_min || year_min_sel > year_max) {
      stop(sprintf("%s is outside the range of the data in this time series.", year_min_sel))
    }
  } else {
    # Default to earliest year if not specified
    year_min_sel <- year_min
  }
  
  if (is.null(year_max_sel) && interactive) {
    cat("\nPlease select an upper bound on the time series that you would like to analyze:\n")
    ensure_visible_prompt()
    year_max_sel <- as.numeric(readline(prompt = "The selected upper bound is: "))
  }
  
  if (!is.null(year_max_sel)) {
    if (year_max_sel < year_min_sel || year_max_sel < year_min || year_max_sel > year_max) {
      stop(sprintf("%s is an invalid maximum year given the data in the series of the minimum year that you have selected.", year_max_sel))
    }
  } else {
    # Default to latest year if not specified
    year_max_sel <- year_max
  }
  
  # --- CRITICAL FIX: Ensure correct data types and filtering ---
  yearbook_sel <- yearbook_sel %>% 
    filter(year_value_ex >= year_min_sel & year_value_ex <= year_max_sel) %>%
    # Ensure value is numeric and there are no NAs that will break the model
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value)) %>%
    # Arrange by the NUMERIC year to ensure correct order
    arrange(year_value_ex) %>%
    # Create the time index 'year_num' from the correctly ordered numeric year
    mutate(year_num = row_number() - 1)
  
  # Lag the value for autoregressive modeling
  yearbook_sel <- yearbook_sel %>%
    group_by(commodity_element) %>%
    mutate(value_lag = lag(value, 1)) %>%
    ungroup()
  
  return(yearbook_sel)
} 
