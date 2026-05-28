# Simple FTN Yearbook Shiny App Launcher
# 
# This script launches the Shiny app from the correct directory

cat("FTN Yearbook Analysis Shiny App Launcher\n")
cat("========================================\n")

# Check if required packages are installed
required_packages <- c(
  "shiny", "shinydashboard", "DT", "plotly",
  "dplyr", "readxl", "lubridate", "stringr", "readr", "tidyr", "ggplot2",
  "broom", "forcats", "moderndive", "rlang", "fastDummies", "cowplot", "shinyjs"
)

check_and_install_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(missing_packages) > 0) {
    cat("The following packages need to be installed:\n")
    cat(paste(missing_packages, collapse = ", "), "\n\n")
    
    response <- readline(prompt = "Do you want to install them now? (y/n): ")
    if (tolower(response) %in% c("y", "yes")) {
      install.packages(missing_packages, repos = "https://cran.r-project.org")
      cat("Packages installed successfully!\n\n")
    } else {
      stop("Required packages are not installed. Please install them manually.")
    }
  } else {
    cat("All required packages are already installed.\n\n")
  }
}

cat("Checking required packages...\n")
check_and_install_packages(required_packages)

# Method 1: Try to get script directory using rstudioapi
script_dir <- NULL

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  tryCatch({
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(script_path) && script_path != "") {
      script_dir <- dirname(script_path)
      cat("✓ Found script directory using rstudioapi:", script_dir, "\n")
    }
  }, error = function(e) {
    cat("✗ rstudioapi method failed:", e$message, "\n")
  })
}

# Method 2: Try using current working directory if it contains our files
if (is.null(script_dir)) {
  current_wd <- getwd()
  cat("Trying current working directory:", current_wd, "\n")
  
  if (file.exists(file.path(current_wd, "shiny_app.R"))) {
    script_dir <- current_wd
    cat("✓ Found files in current directory\n")
  }
}

# Method 3: Look for the app directory
if (is.null(script_dir)) {
  cat("Searching for the app directory...\n")
  
  possible_dirs <- c(
    ".",
    "functions/app",
    "../functions/app", 
    "app",
    "C:/Users/clair/Desktop/src/functions/app"
  )
  
  for (dir in possible_dirs) {
    if (file.exists(file.path(dir, "shiny_app.R"))) {
      script_dir <- normalizePath(dir)
      cat("✓ Found files in:", script_dir, "\n")
      break
    }
  }
}

# If we still can't find it, ask the user
if (is.null(script_dir)) {
  cat("\n✗ Could not automatically find the app directory.\n")
  cat("Please manually navigate to the directory containing shiny_app.R\n")
  cat("Current working directory:", getwd(), "\n")
  cat("Contents of current directory:\n")
  print(list.files())
  
  response <- readline("Enter the full path to the app directory (or press Enter to try current directory): ")
  
  if (response == "") {
    script_dir <- getwd()
  } else {
    script_dir <- response
  }
}

# Try to set the working directory
cat("\nTrying to set working directory to:", script_dir, "\n")

tryCatch({
  setwd(script_dir)
  cat("✓ Working directory set successfully\n")
}, error = function(e) {
  cat("✗ Failed to set working directory:", e$message, "\n")
  cat("Please check that the path exists and you have permission to access it\n")
  stop("Cannot proceed without setting the correct working directory")
})

cat("Current working directory:", getwd(), "\n\n")

# Check for required files
cat("Checking for required files...\n")
required_files <- c("shiny_app.R", "code-utils-1.R", "code-dataprep-2.R", 
                   "code-modeling-3.R", "code-stats-4.R", "code-tline-5.R")

missing_files <- c()
for (file in required_files) {
  if (file.exists(file)) {
    cat("✓ Found:", file, "\n")
  } else {
    cat("✗ Missing:", file, "\n")
    missing_files <- c(missing_files, file)
  }
}

if (length(missing_files) > 0) {
  cat("\nCurrent directory contents:\n")
  print(list.files())
  stop("Cannot launch app. Missing files: ", paste(missing_files, collapse = ", "))
}

# Check for data file
cat("\nChecking for data file...\n")
data_paths <- c(
  "Output/FTN_Yearbook_all_sections.csv",
  "../Output/FTN_Yearbook_all_sections.csv",
  "../../Output/FTN_Yearbook_all_sections.csv",
  "Fruit_Treenut_Flatfile.csv",
  "../Fruit_Treenut_Flatfile.csv", 
  "../../Fruit_Treenut_Flatfile.csv"
)

data_found <- FALSE
for (path in data_paths) {
  if (file.exists(path)) {
    cat("✓ Found data file at:", path, "\n")
    data_found <- TRUE
    break
  }
}

if (!data_found) {
  cat("⚠ Warning: Could not find a data file\n")
  cat("The app may not work properly without the data file.\n")
  response <- readline("Continue anyway? (y/n): ")
  if (tolower(response) != "y") {
    stop("App launch cancelled.")
  }
}

cat("\n", rep("=", 50), "\n")
cat("Launching Shiny app...\n")
cat("The app will open in your web browser.\n")
cat(rep("=", 50), "\n\n")

# Install shiny if needed
if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("Installing shiny package...\n")
  install.packages("shiny")
}

# Run the Shiny app
shiny::runApp("shiny_app.R", launch.browser = TRUE) 