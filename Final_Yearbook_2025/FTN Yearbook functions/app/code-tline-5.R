#' FTN Yearbook Time Series Analysis
#' 
#' This file contains the main tline() function which combines data preparation,
#' modeling, and visualization for time series analysis.
#' 

# Load required packages and utilities
source("code-utils-1.R")
source("code-dataprep-2.R")
source("code-modeling-3.R")
source("ers_theme.R")

#' Main function for time series analysis of FTN Yearbook data
#'
#' This function performs end-to-end time series analysis on FTN Yearbook data.
#' It combines data preparation, modeling, and visualization steps.
#'
#' @param Commodity Selected commodity
#' @param Market_segment Selected market segment
#' @param Variable Selected variable
#' @param Geography Selected geography
#' @param Year_unit Selected year unit
#' @param Value_unit Selected value unit
#' @param Year_min Minimum year to include
#' @param Year_max Maximum year to include
#' @param Monthly Whether to use monthly data ("Yes" or "No")
#' @param Print Whether to save output files ("Yes" or "No")
#' @param Years_predicted Number of years to predict
#' @param Interactive Whether to prompt for user input (default: TRUE)
#' @param Data_file Path to data file (default: "Output/FTN_Yearbook_all_sections.csv")
#'
#' @return A list containing the data, plot, and query information
#' @export
tline <- function(Commodity = NULL, 
                 Market_segment = NULL, 
                 Variable = NULL, 
                 Geography = NULL, 
                 Year_unit = NULL, 
                 Value_unit = NULL, 
                 Year_min = NULL, 
                 Year_max = NULL, 
                 Monthly = NULL, 
                 Print = "No", 
                 Years_predicted = 5,
                 Interactive = TRUE,
                 Data_file = "Output/FTN_Yearbook_all_sections.csv") {
  
  # Load required packages
  required_packages <- c(
    "dplyr", "readxl", "lubridate", "stringr", "readr", 
    "tidyr", "ggplot2", "plotly", "rlang", "broom", "forcats", 
    "moderndive", "fastDummies"
  )
  suppressMessages(load_required_packages(required_packages))
  
  # Try to load the data file, and if it doesn't exist, try alternative paths
  if (!file.exists(Data_file)) {
    # Try a few alternative locations
    potential_paths <- c(
      "Output/FTN_Yearbook_all_sections.csv",              # Processing output (project root)
      "../Output/FTN_Yearbook_all_sections.csv",           # From app/ directory
      "../../Output/FTN_Yearbook_all_sections.csv",        # Two directories up
      "Fruit_Treenut_Flatfile.csv",                        # Legacy flatfile (current directory)
      "../Fruit_Treenut_Flatfile.csv",                     # Legacy flatfile (parent)
      "FTN Yearbook functions/Fruit_Treenut_Flatfile.csv", # Legacy flatfile (FTN dir)
      "../../Fruit_Treenut_Flatfile.csv"                   # Legacy flatfile (two up)
    )
    
    for (path in potential_paths) {
      if (file.exists(path)) {
        message(paste("Found data file at:", path))
        Data_file <- path
        break
      }
    }
  }
  
  # The interactive workflow is guided step by step
  # Non-interactive workflow requires all parameters to be set

  # Function to ensure input prompts are properly displayed and can be responded to
  flush_console <- function() {
    flush.console()  # This ensures the prompt is visible before waiting for input
    Sys.sleep(0.1)   # Brief pause to allow console to update
  }

  # Step 1: Load the data
  message("Loading data...")
  if (!file.exists(Data_file)) {
    stop(paste("ERROR: Could not find data file at", Data_file, 
          "or any standard locations. Please provide the correct path to the data file."))
  }
  yearbook <- read_ftn_data(Data_file)
  
  # Step 2: Filter the data based on user selections
  message("Filtering data...")
  
  # Before entering interactive mode, provide clear instructions
  if (Interactive) {
    message("\n-----------------------------------------------------------------------------")
    message("INTERACTIVE MODE: You will be prompted to make selections for your analysis.")
    message("Please respond to each prompt when it appears in the console.")
    message("-----------------------------------------------------------------------------\n")
    # This ensures the above message is visible
    flush_console()
  }
  
  yearbook_sel <- filter_yearbook_data(
    yearbook,
    commodity = Commodity,
    market_segment = Market_segment,
    variable = Variable,
    geography = Geography,
    year_unit = Year_unit,
    value_unit = Value_unit,
    interactive = Interactive
  )
  
  # Step 3: Prepare the time series
  message("Preparing time series...")
  # Add another flush to ensure console is ready for input
  if (Interactive) {
    flush_console()
  }
  
  yearbook_sel <- prepare_time_series(
    yearbook_sel,
    monthly_ind = Monthly,
    year_min_sel = Year_min,
    year_max_sel = Year_max,
    interactive = Interactive
  )
  
  # Step 4: Model the time series and generate predictions
  message("Modeling time series and generating predictions...")
  model_results <- model_time_series(
    yearbook_sel,
    predict_duration = Years_predicted,
    monthly_ind = if(is.null(Monthly)) "No" else Monthly
  )
  
  # Step 5: Create visualization
  message("Creating visualization...")
  
  # When multiple commodities, we pass the vector. When only one, we pass the string.
  current_commodities <- if (!is.null(Commodity)) {
    Commodity
  } else {
    unique(model_results$data$commodity_element)
  }
  
  plot <- visualize_predictions(
    model_results,
    commodity = current_commodities,
    variable = if(is.null(Variable)) unique(model_results$data$variable)[1] else Variable,
    year_unit = if(is.null(Year_unit)) unique(model_results$data$year_unit)[1] else Year_unit
  )
  
  # Print the plot
  print(plot)
  
  # Step 6: Create query string and arg_list for reproducibility
  query <- sprintf(
    "Commodity = \"%s\", Market_segment = \"%s\", Variable = \"%s\", Geography = \"%s\", Year_unit = \"%s\", Value_unit = \"%s\", Year_min = %s, Year_max = %s, Monthly = \"%s\", Print = \"%s\", Years_predicted = %s",
    Commodity,
    Market_segment,
    Variable,
    Geography,
    Year_unit,
    Value_unit,
    Year_min,
    Year_max,
    Monthly,
    Print,
    Years_predicted
  )
  
  message("\nNote: You can rerun this query by copying and pasting the following arguments into the tline() function.\n\n", query, "\n\nA list containing these arguments has been exported and can be passed to do.call(tline, arg_list) function.\n\n")
  
  # Create arg_list for reproducibility
  arg_list <- list(
    Commodity = Commodity,
    Market_segment = Market_segment,
    Variable = Variable,
    Geography = Geography,
    Year_unit = Year_unit,
    Value_unit = Value_unit,
    Year_min = Year_min,
    Year_max = Year_max,
    Monthly = Monthly,
    Print = Print,
    Years_predicted = Years_predicted
  )
  
  # Step 7: Save outputs if requested
  if (Print == "Yes") {
    # Create a filename based on selected parameters
    title_sub <- paste(
      if(is.null(Commodity)) unique(model_results$data$commodity_element)[1] else Commodity,
      if(is.null(Variable)) unique(model_results$data$variable)[1] else Variable,
      if(is.null(Year_unit)) unique(model_results$data$year_unit)[1] else Year_unit,
      sep = ", "
    )
    
    # Save outputs
    save_outputs(
      data = model_results$data,
      plot = plot,
      filename = title_sub
    )
  }
  
  # --- ROBUSTNESS: Remove global environment assignments ---
  # The function should return all results as a list, not assign them globally.
  # This makes the function "pure" and prevents state-related bugs in Shiny.
  
  # Return results as a list
  invisible(list(
    data = model_results$data,
    plot = plot,
    query = query,
    arg_list = arg_list,
    model_summaries = model_results$model_summaries
  ))
} 