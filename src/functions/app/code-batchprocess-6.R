#' FTN Yearbook Batch Processing Functions
#' 
#' This file contains functions for running multiple analyses in batch mode,
#' allowing users to process multiple commodity/variable combinations at once.
#' 

# Load required packages and utilities
source("code-utils-1.R")
source("code-dataprep-2.R")
source("code-modeling-3.R")
source("code-stats-4.R")
source("code-tline-5.R")
source("ers_theme.R")

#' Wrapper function to run multiple tline analyses
#'
#' This function automates running the tline function multiple times for different queries,
#' collecting and bundling the results.
#'
#' @param QUERIES Number of queries to run, or list of query specifications
#' @param PREDICTIONS Number of years to predict for each query
#' @param ARG_LIST Optional list of argument lists for tline, one per query
#' @param COMBINED_PLOT Whether to create a combined plot of all predictions (default: TRUE)
#' @param SAVE_RESULTS Whether to save all results to files (default: FALSE)
#' @param OUTPUT_DIR Directory to save results in (default: current working directory)
#'
#' @return A list containing all prediction results, plots, and combined plot
#' @export
predict_wrapper <- function(QUERIES = NULL, 
                           PREDICTIONS = 5, 
                           ARG_LIST = NULL,
                           COMBINED_PLOT = TRUE,
                           SAVE_RESULTS = FALSE,
                           OUTPUT_DIR = getwd()) {
  
  # Ensure required packages are loaded
  required_packages <- c("dplyr", "cowplot", "ggplot2")
  suppressMessages(load_required_packages(required_packages))
  
  # Initialize storage for results
  all_predictions <- list()
  all_plots <- list()
  all_query_params <- list()
  
  # Determine number of queries to run
  if (is.numeric(QUERIES)) {
    num_queries <- QUERIES
    
    # Validate QUERIES parameter
    if (num_queries <= 0) {
      stop("QUERIES must be a positive number")
    }
    
    # If ARG_LIST provided, ensure it has the right length
    if (!is.null(ARG_LIST) && length(ARG_LIST) != num_queries) {
      stop(sprintf("ARG_LIST should contain %d element(s) for %d queries", num_queries, num_queries))
    }
  } else if (is.list(QUERIES)) {
    # QUERIES is a list of specifications
    num_queries <- length(QUERIES)
    
    # Override ARG_LIST with QUERIES
    ARG_LIST <- QUERIES
  } else {
    stop("QUERIES must be either a number or a list of query specifications")
  }
  
  message(sprintf("Running %d tline analyses...", num_queries))
  
  # Run tline for each query
  for (c in 1:num_queries) {
    message(sprintf("\n--- Query %d of %d ---", c, num_queries))
    
    # Run tline with appropriate arguments
    if (is.null(ARG_LIST)) {
      result <- tline(Years_predicted = PREDICTIONS)
    } else {
      current_args <- ARG_LIST[[c]]
      
      # Override Years_predicted if not specified in ARG_LIST
      if (is.null(current_args$Years_predicted)) {
        current_args$Years_predicted <- PREDICTIONS
      }
      
      result <- do.call(tline, current_args)
    }
    
    # Extract and store the prediction results
    current_predictions <- result$data %>% filter(!is.na(predict_tl))
    
    if (c == 1) {
      Predictions <- current_predictions
    } else {
      Predictions <- rbind(Predictions, current_predictions)
    }
    
    # Store results for this query
    all_predictions[[c]] <- result$data
    all_plots[[c]] <- result$plot
    all_query_params[[c]] <- result$arg_list
    
    # Store in global environment with numbered identifiers
    output_name <- sprintf("Output_%s", c)
    assign(output_name, result$data, envir = .GlobalEnv)
    
    plot_name <- sprintf("Plot_%s", c)
    assign(plot_name, result$plot, envir = .GlobalEnv)
    
    args_name <- sprintf("arg_list_%s", c)
    assign(args_name, result$arg_list, envir = .GlobalEnv)
    
    query_name <- sprintf("Query_%s", c)
    assign(query_name, result$query, envir = .GlobalEnv)
  }
  
  # Create combined plot if requested
  if (COMBINED_PLOT && num_queries > 1) {
    message("\nCreating combined plot...")
    
    # Get plot names
    plot_names <- sprintf("Plot_%s", seq(1, num_queries, 1))
    
    # Get plots from the global environment
    plot_list <- lapply(plot_names, get)
    
    # Create combined plot using cowplot
    COMBINED <- cowplot::plot_grid(plotlist = plot_list)
    
    # Display the combined plot
    print(COMBINED)
    
    # Store in global environment
    assign("COMBINED_PLOT", COMBINED, envir = .GlobalEnv)
    
    # Save combined plot if requested
    if (SAVE_RESULTS) {
      combined_plot_path <- file.path(OUTPUT_DIR, "combined_predictions.png")
      ggsave(filename = combined_plot_path, plot = COMBINED)
      message(sprintf("Combined plot saved to: %s", combined_plot_path))
    }
  }
  
  # Store all argument lists in global environment for reuse
  arg_list_names <- sprintf("arg_list_%s", seq(1, num_queries, 1))
  ARG_LIST_ALL <- lapply(arg_list_names, get)
  assign("ARG_LIST", ARG_LIST_ALL, envir = .GlobalEnv)
  
  # Store predictions in global environment
  assign("Predictions", Predictions, envir = .GlobalEnv)
  
  # Save all results if requested
  if (SAVE_RESULTS) {
    message("\nSaving all results...")
    
    # Ensure output directory exists
    if (!dir.exists(OUTPUT_DIR)) {
      dir.create(OUTPUT_DIR, recursive = TRUE)
    }
    
    # Save combined predictions
    combined_csv_path <- file.path(OUTPUT_DIR, "combined_predictions.csv")
    write.csv(Predictions, file = combined_csv_path, row.names = FALSE)
    message(sprintf("Combined predictions saved to: %s", combined_csv_path))
    
    # Save individual results
    for (c in 1:num_queries) {
      # Create meaningful filename
      commodity <- all_query_params[[c]]$Commodity
      variable <- all_query_params[[c]]$Variable
      
      if (is.null(commodity)) commodity <- "unspecified_commodity"
      if (is.null(variable)) variable <- "unspecified_variable"
      
      filename <- paste("query", c, "-", commodity, "-", variable, sep = "")
      filename <- gsub(" ", "_", filename)
      
      # Save data
      csv_path <- file.path(OUTPUT_DIR, paste0(filename, ".csv"))
      write.csv(all_predictions[[c]], file = csv_path, row.names = FALSE)
      
      # Save plot
      plot_path <- file.path(OUTPUT_DIR, paste0(filename, ".png"))
      ggsave(filename = plot_path, plot = all_plots[[c]])
      
      message(sprintf("Query %d results saved to: %s and %s", c, csv_path, plot_path))
    }
  }
  
  # Return all results
  return(list(
    predictions = Predictions,
    all_predictions = all_predictions,
    all_plots = all_plots,
    arg_list = ARG_LIST_ALL,
    combined_plot = if (exists("COMBINED")) COMBINED else NULL
  ))
} 