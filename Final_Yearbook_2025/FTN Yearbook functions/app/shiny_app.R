# FTN Yearbook Analysis Shiny App
# 
# This Shiny app provides a web interface for the FTN Yearbook analysis tools.
# Users can select commodities, market segments, variables, and other parameters
# through dropdown menus and view the resulting time series analysis and plots.

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyjs)

# Set working directory to the location of this script
# This ensures we can find the other R files and data
script_dir <- NULL

# Try to get the directory of the current script
if (exists("rstudioapi") && requireNamespace("rstudioapi", quietly = TRUE)) {
  tryCatch({
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  }, error = function(e) NULL)
}

# If that fails, check if we're in the right directory by looking for our files
if (is.null(script_dir) || !file.exists(file.path(script_dir, "code-utils-1.R"))) {
  # Look for the files in the current working directory
  if (file.exists("code-utils-1.R")) {
    script_dir <- getwd()
  } else {
    # Try to find the app directory
    possible_dirs <- c(
      ".",
      "FTN Yearbook functions/app",
      "../FTN Yearbook functions/app",
      "app"
    )
    
    for (dir in possible_dirs) {
      if (file.exists(file.path(dir, "code-utils-1.R"))) {
        script_dir <- normalizePath(dir)
        break
      }
    }
  }
}

# Set the working directory
if (!is.null(script_dir)) {
  setwd(script_dir)
  message("Working directory set to: ", script_dir)
} else {
  warning("Could not determine script directory. Using current working directory: ", getwd())
}

# Verify we can find the required files
required_files <- c("code-utils-1.R", "code-dataprep-2.R", "code-modeling-3.R", 
                   "code-stats-4.R", "code-tline-5.R")

missing_files <- c()
for (file in required_files) {
  if (!file.exists(file)) {
    missing_files <- c(missing_files, file)
  }
}

if (length(missing_files) > 0) {
  stop("Cannot find required files: ", paste(missing_files, collapse = ", "), 
       "\nCurrent working directory: ", getwd(),
       "\nPlease ensure all R files are in the same directory as the Shiny app.")
}

# Source the FTN Yearbook analysis functions
message("Sourcing required R files...")
source("code-utils-1.R")
source("code-dataprep-2.R")
source("code-modeling-3.R")
source("code-stats-4.R")
source("code-tline-5.R")
source("ers_theme.R")

# Load required packages
required_packages <- c(
  "dplyr", "readxl", "lubridate", "stringr", "readr", "tidyr", "ggplot2",
  "broom", "forcats", "moderndive", "rlang", "fastDummies", "cowplot"
)
load_required_packages(required_packages)

# Load the FTN Yearbook data
# Try to find the data file in multiple locations
data_paths <- c(
  "Output/FTN_Yearbook_all_sections.csv",
  "../Output/FTN_Yearbook_all_sections.csv",
  "../../Output/FTN_Yearbook_all_sections.csv",
  "Fruit_Treenut_Flatfile.csv",
  "../Fruit_Treenut_Flatfile.csv", 
  "FTN Yearbook functions/Fruit_Treenut_Flatfile.csv",
  "../../Fruit_Treenut_Flatfile.csv"
)

global_data <- NULL
data_file_path <- NULL

message("Searching for data file...")
for (path in data_paths) {
  if (file.exists(path)) {
    message(paste("Found data file at:", path))
    data_file_path <- path
    global_data <- read_ftn_data(path)
    break
  }
}

if (is.null(global_data)) {
  stop("Could not find a data file in any expected location. Please ensure the data file is available.")
}

message("Data loaded successfully. Number of rows: ", nrow(global_data))

# --- FEATURE: Add commodity characteristic for filtering ---
# Create a mapping from commodity to a characteristic (e.g., fruit type)
fruit_type_mapping <- list(
  "Citrus" = c("Oranges", "Grapefruit", "Lemons", "Limes", "Tangerines & mandarins"),
  "Pome Fruit" = c("Apples", "Pears"),
  "Stone Fruit" = c("Peaches", "Nectarines", "Plums and prunes", "Cherries", "Apricots"),
  "Berries" = c("Strawberries", "Blueberries", "Raspberries", "Cranberries"),
  "Tree Nuts" = c("Almonds", "Walnuts", "Pecans", "Hazelnuts", "Pistachios", "Macadamia nuts"),
  "Melons" = c("Cantaloupes", "Honeydews", "Watermelons"),
  "Tropical/Other" = c("Grapes", "Avocados", "Kiwifruit", "Olives", "Dates", "Figs", "Papayas", "Pineapples", "Bananas", "Mangoes")
)

# Convert mapping to a data frame for easier joining
map_df <- tryCatch({
  dplyr::bind_rows(lapply(fruit_type_mapping, function(x) data.frame(commodity_element = x)), .id = "fruit_type")
}, error = function(e) {
  # Fallback for older dplyr versions
  map_df <- do.call(rbind, lapply(names(fruit_type_mapping), function(name) {
    data.frame(fruit_type = name, commodity_element = fruit_type_mapping[[name]], stringsAsFactors = FALSE)
  }))
})


# Add the fruit_type to the global data
global_data <- global_data %>%
  left_join(map_df, by = "commodity_element") %>%
  mutate(fruit_type = ifelse(is.na(fruit_type), "Other", as.character(fruit_type)))

message("Added 'fruit_type' characteristic to the data.")
# --- END FEATURE ---

# Get unique values for dropdowns
get_unique_values <- function(data, column) {
  unique_vals <- sort(unique(data[[column]]))
  unique_vals[!is.na(unique_vals)]
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "FTN Yearbook Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Stored Analyses", tabName = "stored", icon = icon("save"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Analysis tab
      tabItem(tabName = "analysis",
        fluidRow(
          # Parameter selection box
          box(
            title = "Analysis Parameters", status = "primary", solidHeader = TRUE,
            width = 4, height = "800px",
            
            # This div wrapper makes the content of the box scrollable,
            # preventing the "Current Selection" summary from being cut off.
            div(style = "height: 720px; overflow-y: auto; padding-right: 15px;",
            
              selectInput("fruit_type", "Commodity Type:",
                         choices = c("All", get_unique_values(global_data, "fruit_type")),
                         selected = "All"),
              
              selectizeInput("commodity", "Commodity:",
                             choices = NULL, # Populated dynamically
                             multiple = TRUE,
                             options = list(placeholder = 'Select one or more commodities...')),
              
              # Overlay mode toggle
              checkboxInput("overlay_mode", "Overlay multiple series", value = FALSE),
              
              selectInput("market_segment", "Market Segment:",
                         choices = NULL),
              
              selectInput("variable", "Variable:",
                         choices = NULL),
              
              selectInput("geography", "Geography:",
                         choices = NULL),
              
              selectInput("year_unit", "Year Unit:",
                         choices = NULL),
              
              selectInput("value_unit", "Value Unit:",
                         choices = NULL),
              
              hr(),
              
              fluidRow(
                column(6,
                  numericInput("year_min", "Start Year:", 
                             value = 2000, min = 1900, max = 2030)
                ),
                column(6,
                  numericInput("year_max", "End Year:", 
                             value = 2023, min = 1900, max = 2030)
                )
              ),
              
              fluidRow(
                column(6,
                  numericInput("years_predicted", "Years to Predict:", 
                             value = 5, min = 1, max = 20)
                ),
                column(6,
                  selectInput("monthly", "Monthly Data:",
                             choices = c("No", "Yes"), selected = "No")
                )
              ),
              
              hr(),
              
              # Analysis control buttons
              div(style = "text-align: center; margin: 15px 0;",
                fluidRow(
                  column(12,
                    actionButton("run_analysis", "Run Analysis", 
                                class = "btn-primary btn-lg", 
                                style = "font-size: 16px; padding: 10px 30px; font-weight: bold; margin-bottom: 10px; width: 100%;")
                  )
                ),
                fluidRow(
                  column(6,
                    actionButton("save_analysis", "Save to Slot", 
                                class = "btn-success", 
                                style = "font-size: 14px; padding: 8px 16px; width: 100%;")
                  ),
                  column(6,
                    div(style="display: flex; align-items: center;",
                        selectInput("save_slot", "Slot:", 
                                   choices = 1:4, 
                                   selected = 1,
                                   width = "100%"),
                        actionLink("slot_help", label = NULL, 
                                   icon = icon("question-circle"),
                                   title = "Click for help on storage slots",
                                   style = "margin-left: 10px; font-size: 1.5em;")
                    )
                  )
                )
              ),
              
              hr(),
              
              # Display current selection summary in a collapsible section
              div(id = "selection_summary",
                h5("Current Selection:", style = "margin-top: 10px;"),
                div(style = "max-height: 120px; overflow-y: auto; font-size: 12px; background-color: #f8f9fa; padding: 8px; border-radius: 4px;",
                  verbatimTextOutput("current_selection", placeholder = FALSE)
                )
              )
            ) # End of scrollable wrapper div
          ),
          
          # Results box
          box(
            title = "Analysis Results", status = "success", solidHeader = TRUE,
            width = 8, height = "800px",
            
            # Download button for current analysis
            div(style = "text-align: right; margin-bottom: 10px;",
              downloadButton("download_current_data", "Download Data (CSV)", 
                           class = "btn-success",
                           style = "font-size: 14px; padding: 6px 12px;")
            ),
            
            tabsetPanel(
              tabPanel("Plot", 
                plotlyOutput("result_plot", height = "500px")
              ),
              tabPanel("Data Table", 
                # This wrapper adds a horizontal scrollbar to the data table
                div(style = "overflow-x: auto; overflow-y: auto; max-height: 520px;",
                    DT::dataTableOutput("result_table"))
              ),
              tabPanel("Statistics",
                # This wrapper adds a horizontal scrollbar to the statistics table
                div(style = "overflow-x: auto;", DT::dataTableOutput("result_stats"))
              )
            )
          )
        ),
        
        # Status/messages row
        fluidRow(
          box(
            title = "Status", status = "info", solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("status_messages")
          )
        )
      ),
      
      # Stored Analyses tab
      tabItem(tabName = "stored",
        fluidRow(
          box(
            title = "Stored Analysis Slots", status = "primary", solidHeader = TRUE,
            width = 12,
            
            h4("Switch Between Saved Analyses"),
            p("Click on any saved analysis to view its results without re-running the analysis."),
            
            fluidRow(
              column(3,
                div(style = "text-align: center; margin: 10px;",
                  actionButton("load_slot_1", "Slot 1", 
                              class = "btn-info btn-lg", 
                              style = "width: 100%; height: 80px; font-size: 16px;"),
                  br(), br(),
                  div(id = "slot_1_info", style = "font-size: 12px; color: #666;",
                    textOutput("slot_1_summary")
                  )
                )
              ),
              column(3,
                div(style = "text-align: center; margin: 10px;",
                  actionButton("load_slot_2", "Slot 2", 
                              class = "btn-info btn-lg", 
                              style = "width: 100%; height: 80px; font-size: 16px;"),
                  br(), br(),
                  div(id = "slot_2_info", style = "font-size: 12px; color: #666;",
                    textOutput("slot_2_summary")
                  )
                )
              ),
              column(3,
                div(style = "text-align: center; margin: 10px;",
                  actionButton("load_slot_3", "Slot 3", 
                              class = "btn-info btn-lg", 
                              style = "width: 100%; height: 80px; font-size: 16px;"),
                  br(), br(),
                  div(id = "slot_3_info", style = "font-size: 12px; color: #666;",
                    textOutput("slot_3_summary")
                  )
                )
              ),
              column(3,
                div(style = "text-align: center; margin: 10px;",
                  actionButton("load_slot_4", "Slot 4", 
                              class = "btn-info btn-lg", 
                              style = "width: 100%; height: 80px; font-size: 16px;"),
                  br(), br(),
                  div(id = "slot_4_info", style = "font-size: 12px; color: #666;",
                    textOutput("slot_4_summary")
                  )
                )
              )
            ),
            
            hr(),
            
            h4("Current Analysis"),
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px;",
              verbatimTextOutput("current_analysis_info")
            ),
            
            hr(),
            
            fluidRow(
              column(4,
                actionButton("clear_slot", "Clear Selected Slot", 
                            class = "btn-warning", 
                            style = "width: 100%;")
              ),
              column(4,
                actionButton("clear_all_slots", "Clear All Slots", 
                            class = "btn-danger", 
                            style = "width: 100%;")
              ),
              column(4,
                downloadButton("download_stored_data", "Download Data (CSV)", 
                              class = "btn-success", 
                              style = "width: 100%;")
              )
            )
          )
        ),
        
        # Display current stored analysis results
        fluidRow(
          box(
            title = "Stored Analysis Results", status = "success", solidHeader = TRUE,
            width = 12, height = "600px",
            
            tabsetPanel(
              tabPanel("Plot", 
                plotlyOutput("stored_result_plot", height = "400px")
              ),
              tabPanel("Data Table", 
                # This wrapper adds a horizontal scrollbar to the data table
                div(style = "overflow-x: auto; overflow-y: auto; max-height: 420px;",
                    DT::dataTableOutput("stored_result_table"))
              ),
              tabPanel("Statistics",
                verbatimTextOutput("stored_result_stats")
              )
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store multiple analysis results
  values <- reactiveValues(
    current_data = global_data,
    analysis_result = NULL,
    status_message = "Ready. Please select parameters and click 'Run Analysis'.",
    
    # Storage for multiple analyses (up to 4 slots)
    stored_analyses = list(
      slot_1 = NULL,
      slot_2 = NULL, 
      slot_3 = NULL,
      slot_4 = NULL
    ),
    
    # Parameters for each stored analysis
    stored_params = list(
      slot_1 = NULL,
      slot_2 = NULL,
      slot_3 = NULL, 
      slot_4 = NULL
    ),
    
    # Currently displayed stored analysis
    current_stored_slot = NULL,
    current_stored_analysis = NULL
  )
  
  # --- FEATURE: ROBUST REACTIVE LOGIC ---
  # Create explicit triggers to manage the reactive cascade and prevent bugs.
  triggers <- reactiveValues(
    commodity = 0,
    market_segment = 0,
    variable = 0,
    geography = 0,
    year_unit = 0
  )
  # --- END FEATURE ---
  
  # --- FEATURE: Help modal for storage slots ---
  observeEvent(input$slot_help, {
    showModal(modalDialog(
      title = "About Storage Slots",
      p("The storage slots allow you to save up to four different analysis results."),
      p("This is useful for comparing different commodities, time periods, or variables without having to re-run the analysis each time."),
      tags$ul(
        tags$li(strong("Save to Slot:"), "After running an analysis, select a slot number and click 'Save to Slot' to store the results."),
        tags$li(strong("Load from Slot:"), "Go to the 'Stored Analyses' tab to view and load your saved results."),
        tags$li(strong("Comparison:"), "Quickly switch between loaded slots to compare plots, data, and statistics.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  # --- END FEATURE ---
  
  # --- FEATURE: ROBUST REACTIVE LOGIC ---

  # This trigger-based system ensures a reliable, one-way flow of updates,
  # preventing reactive loops and ensuring dropdowns are always in a consistent state.

  # 1. Observer for 'fruit_type'
  observeEvent(input$fruit_type, {
    # --- ROBUSTNESS: Disable run button during updates ---
    shinyjs::disable("run_analysis")
    
    filtered_commodities <- if (input$fruit_type == "All") {
      global_data
    } else {
      global_data %>% filter(fruit_type == input$fruit_type)
    }
    
    new_choices <- get_unique_values(filtered_commodities, "commodity_element")
    current_selection <- isolate(input$commodity)
    valid_selection <- current_selection[current_selection %in% new_choices]
    
    updateSelectizeInput(session, "commodity",
                         choices = new_choices,
                         selected = valid_selection,
                         server = TRUE)
                         
    # Increment the trigger for the next dropdown in the chain.
    triggers$commodity <- triggers$commodity + 1
  })

  # 2. Observer for 'commodity'. Responds to user selection OR a trigger from above.
  observeEvent({
    input$commodity
    triggers$commodity
  }, {
    if (length(input$commodity) == 1) {
      filtered_data <- global_data %>% filter(commodity_element == input$commodity)
      
      market_segment_choices <- get_unique_values(filtered_data, "market_segment")
      updateSelectInput(session, "market_segment",
                        choices = market_segment_choices,
                        selected = market_segment_choices[1])
      
      # Trigger the next observer.
      triggers$market_segment <- triggers$market_segment + 1
    } else {
      # If 0 or >1 commodities are selected, stop the cascade and clear downstream inputs.
      placeholder_choice <- c("Select a single commodity" = "")
      updateSelectInput(session, "market_segment", choices = placeholder_choice, selected = "")
      updateSelectInput(session, "variable", choices = placeholder_choice, selected = "")
      updateSelectInput(session, "geography", choices = placeholder_choice, selected = "")
      updateSelectInput(session, "year_unit", choices = placeholder_choice, selected = "")
      updateSelectInput(session, "value_unit", choices = placeholder_choice, selected = "")
      
      # --- ROBUSTNESS: Enable button only at a valid end state ---
      shinyjs::enable("run_analysis")
    }
  })

  # 3. Observer for 'market_segment'
  observeEvent({
    input$market_segment
    triggers$market_segment
  }, {
    req(length(input$commodity) == 1, input$market_segment != "")
    
    filtered_data <- global_data %>% 
      filter(commodity_element == input$commodity, market_segment == input$market_segment)
    
    variable_choices <- get_unique_values(filtered_data, "variable")
    updateSelectInput(session, "variable", choices = variable_choices, selected = variable_choices[1])
    
    triggers$variable <- triggers$variable + 1
  })

  # 4. Observer for 'variable'
  observeEvent({
    input$variable
    triggers$variable
  }, {
    req(length(input$commodity) == 1, input$variable != "")

    filtered_data <- global_data %>% 
      filter(commodity_element == input$commodity, market_segment == input$market_segment, variable == input$variable)
    
    geography_choices <- get_unique_values(filtered_data, "geographic_extent")
    updateSelectInput(session, "geography", choices = geography_choices, selected = geography_choices[1])
    
    triggers$geography <- triggers$geography + 1
  })

  # 5. Observer for 'geography'
  observeEvent({
    input$geography
    triggers$geography
  }, {
    req(length(input$commodity) == 1, input$geography != "")

    filtered_data <- global_data %>% 
      filter(commodity_element == input$commodity, market_segment == input$market_segment, 
             variable == input$variable, geographic_extent == input$geography)
    
    year_unit_choices <- get_unique_values(filtered_data, "year_unit")
    updateSelectInput(session, "year_unit", choices = year_unit_choices, selected = year_unit_choices[1])
    
    triggers$year_unit <- triggers$year_unit + 1
  })
  
  # 6. Observer for 'year_unit'
  observeEvent({
    input$year_unit
    triggers$year_unit
  }, {
    req(length(input$commodity) == 1, input$year_unit != "")

    filtered_data <- global_data %>% 
      filter(commodity_element == input$commodity, market_segment == input$market_segment, 
             variable == input$variable, geographic_extent == input$geography, 
             year_unit == input$year_unit)
    
    value_unit_choices <- get_unique_values(filtered_data, "unit")
    updateSelectInput(session, "value_unit", choices = value_unit_choices, selected = value_unit_choices[1])
    
    # --- ROBUSTNESS: This is the final step in a valid cascade, so enable the button. ---
    shinyjs::enable("run_analysis")
  })
  
  # --- END OF ROBUST REACTIVE LOGIC ---
  
  # --- FEATURE: Overlay mode UI adjustments ---
  observeEvent(input$overlay_mode, {
    if (isTRUE(input$overlay_mode)) {
      # Hide inputs not used in overlay
      shinyjs::hide("market_segment")
      shinyjs::hide("geography")
      shinyjs::hide("year_unit")
      shinyjs::hide("value_unit")
      hideTab("resultTabs", "Statistics")
    } else {
      shinyjs::show("market_segment")
      shinyjs::show("geography")
      shinyjs::show("year_unit")
      shinyjs::show("value_unit")
      showTab("resultTabs", "Statistics")
    }
  }, ignoreInit = TRUE)

  # --- END UI overlay adjustments ---
  
  # Run analysis when button is clicked
  observeEvent(input$run_analysis, {
    
    # Clear previous
    values$analysis_result <- NULL
    
    if (isTRUE(input$overlay_mode)) {
      # Overlay path
      if (length(input$commodity) == 0 || length(input$variable) == 0) {
        values$status_message <- "Error: select at least one commodity and one variable in overlay mode."; return()
      }
      sel <- expand.grid(commodity=input$commodity, variable=input$variable, stringsAsFactors = FALSE)
      overlay_df <- purrr::map2_dfr(sel$commodity, sel$variable, function(c,v){
        global_data %>%
          dplyr::filter(commodity_element==c, variable==v, geographic_extent=="United States") %>%
          dplyr::arrange(year_value_ex) %>%
          dplyr::mutate(series=paste(c,v,sep=" - "))
      })
      if(nrow(overlay_df)==0){values$status_message <- "No data found for selections.";return()}
      base_series <- unique(overlay_df$series)[1]
      base_value <- overlay_df %>% dplyr::filter(series==base_series) %>% dplyr::slice(1) %>% dplyr::pull(value)
      overlay_df <- overlay_df %>% dplyr::mutate(index = value/base_value*100)
      values$analysis_result <- list(data=overlay_df, plot=NULL, overlay=TRUE)
      values$status_message <- paste("Overlay plot ready",Sys.time())
      return()
    }
    
    # Strengthened validation
    if (length(input$commodity) == 0) {
      values$status_message <- "Error: Please select at least one commodity."
      return()
    }
    
    # Validation for single commodity selection
    if (length(input$commodity) == 1) {
      if (is.null(input$market_segment) || input$market_segment == "" ||
          is.null(input$variable) || input$variable == "" ||
          is.null(input$geography) || input$geography == "" ||
          is.null(input$year_unit) || input$year_unit == "" ||
          is.null(input$value_unit) || input$value_unit == "") {
        values$status_message <- "Error: For a single commodity, all parameters must be selected."
        return()
      }
    }
    
    values$status_message <- "Running analysis... Please wait."
    
    # Run the analysis in a safe environment
    tryCatch({
      # Call the "pure" tline function and store the entire result
      result <- tline(
        Commodity = input$commodity,
        Market_segment = if (length(input$commodity) > 1) NULL else input$market_segment,
        Variable = if (length(input$commodity) > 1) NULL else input$variable,
        Geography = if (length(input$commodity) > 1) NULL else input$geography,
        Year_unit = if (length(input$commodity) > 1) NULL else input$year_unit,
        Value_unit = if (length(input$commodity) > 1) NULL else input$value_unit,
        Year_min = input$year_min,
        Year_max = input$year_max,
        Monthly = input$monthly,
        Years_predicted = input$years_predicted,
        Interactive = FALSE,
        Print = "No",
        Data_file = data_file_path
      )
      
      # Store the complete result in the app's reactive values
      values$analysis_result <- result
      values$status_message <- paste("Analysis completed successfully at", Sys.time())
      
    }, error = function(e) {
      values$status_message <- paste("Error running analysis:", e$message)
      values$analysis_result <- NULL
    })
  })
  
  # Save analysis to selected slot
  observeEvent(input$save_analysis, {
    if (is.null(values$analysis_result)) {
      values$status_message <- "Error: No analysis to save. Please run an analysis first."
      return()
    }
    
    slot_name <- paste0("slot_", input$save_slot)
    
    # Store the analysis result
    values$stored_analyses[[slot_name]] <- values$analysis_result
    
    # Store the parameters
    values$stored_params[[slot_name]] <- list(
      commodity = input$commodity,
      market_segment = input$market_segment,
      variable = input$variable,
      geography = input$geography,
      year_unit = input$year_unit,
      value_unit = input$value_unit,
      year_min = input$year_min,
      year_max = input$year_max,
      monthly = input$monthly,
      years_predicted = input$years_predicted,
      timestamp = Sys.time()
    )
    
    values$status_message <- paste("Analysis saved to Slot", input$save_slot, "at", Sys.time())
  })
  
  # Load analysis from slots
  observeEvent(input$load_slot_1, {
    if (!is.null(values$stored_analyses$slot_1)) {
      values$current_stored_slot <- 1
      values$current_stored_analysis <- values$stored_analyses$slot_1
      values$status_message <- paste("Loaded analysis from Slot 1")
    } else {
      values$status_message <- "Slot 1 is empty. Please save an analysis to this slot first."
    }
  })
  
  observeEvent(input$load_slot_2, {
    if (!is.null(values$stored_analyses$slot_2)) {
      values$current_stored_slot <- 2
      values$current_stored_analysis <- values$stored_analyses$slot_2
      values$status_message <- paste("Loaded analysis from Slot 2")
    } else {
      values$status_message <- "Slot 2 is empty. Please save an analysis to this slot first."
    }
  })
  
  observeEvent(input$load_slot_3, {
    if (!is.null(values$stored_analyses$slot_3)) {
      values$current_stored_slot <- 3
      values$current_stored_analysis <- values$stored_analyses$slot_3
      values$status_message <- paste("Loaded analysis from Slot 3")
    } else {
      values$status_message <- "Slot 3 is empty. Please save an analysis to this slot first."
    }
  })
  
  observeEvent(input$load_slot_4, {
    if (!is.null(values$stored_analyses$slot_4)) {
      values$current_stored_slot <- 4
      values$current_stored_analysis <- values$stored_analyses$slot_4
      values$status_message <- paste("Loaded analysis from Slot 4")
    } else {
      values$status_message <- "Slot 4 is empty. Please save an analysis to this slot first."
    }
  })
  
  # Clear selected slot
  observeEvent(input$clear_slot, {
    if (!is.null(values$current_stored_slot)) {
      slot_name <- paste0("slot_", values$current_stored_slot)
      values$stored_analyses[[slot_name]] <- NULL
      values$stored_params[[slot_name]] <- NULL
      values$current_stored_analysis <- NULL
      values$status_message <- paste("Cleared Slot", values$current_stored_slot)
      values$current_stored_slot <- NULL
    } else {
      values$status_message <- "No slot selected to clear."
    }
  })
  
  # Clear all slots
  observeEvent(input$clear_all_slots, {
    values$stored_analyses <- list(slot_1 = NULL, slot_2 = NULL, slot_3 = NULL, slot_4 = NULL)
    values$stored_params <- list(slot_1 = NULL, slot_2 = NULL, slot_3 = NULL, slot_4 = NULL)
    values$current_stored_analysis <- NULL
    values$current_stored_slot <- NULL
    values$status_message <- "All slots cleared."
  })
  
  # Display current selection summary
  output$current_selection <- renderText({
    paste(
      "Commodity:", input$commodity %||% "Not selected",
      "\nMarket Segment:", input$market_segment %||% "Not selected", 
      "\nVariable:", input$variable %||% "Not selected",
      "\nGeography:", input$geography %||% "Not selected",
      "\nYear Unit:", input$year_unit %||% "Not selected",
      "\nValue Unit:", input$value_unit %||% "Not selected",
      "\nTime Period:", input$year_min, "-", input$year_max,
      "\nYears to Predict:", input$years_predicted,
      "\nMonthly Data:", input$monthly
    )
  })
  
  # Display status messages
  output$status_messages <- renderText({
    values$status_message
  })
  
  # Display the plot (current analysis)
  output$result_plot <- renderPlotly({
    if (is.null(values$analysis_result)) {
      return(plotly_empty() %>% layout(title="No plot available. Please run an analysis."))
    }
    if (isTRUE(values$analysis_result$overlay)) {
      df <- values$analysis_result$data
      plt <- ggplot(df, aes(x = year_value_ex,
                            y = index,
                            colour = series,
                            group = series,
                            text = paste0("Series: ", series,
                                           "<br>Year: ", year_value_ex,
                                           "<br>Index: ", sprintf("%.1f", index)))) +
        geom_line() +
        labs(y = "Index (first series = 100)", x = "Year", colour = "Series") +
        ers_theme()
      return(ggplotly(plt, tooltip = "text"))
    } else if(!is.null(values$analysis_result$plot)) {
      return(ggplotly(values$analysis_result$plot, tooltip = c("x","y")))
    } else {
      return(plotly_empty() %>% layout(title="No plot available."))
    }
  })
  
  # Display the data table (current analysis)
  output$result_table <- DT::renderDataTable({
    # Read data directly from the reactive values
    if (!is.null(values$analysis_result) && !is.null(values$analysis_result$data)) {
      DT::datatable(values$analysis_result$data, 
                    options = list(
                      scrollX = TRUE,
                      scrollY = "60vh",
                      scrollCollapse = TRUE,
                      pageLength = 3,  # Changed from 15 to 3
                      lengthMenu = list(c(3, 10, 15, -1), c('3', '10', '15', 'All'))
                    ))
    } else {
      DT::datatable(data.frame(Message = "No data available. Please run analysis first."))
    }
  })
  
  # Display statistics (current analysis)
  output$result_stats <- DT::renderDataTable({
    # Read model summaries directly from the reactive values
    if (!is.null(values$analysis_result) && !is.null(values$analysis_result$model_summaries)) {
      
      # Select and rename columns for clarity
      stats_data <- values$analysis_result$model_summaries %>%
        select(
          Commodity = commodity_element,
          Model = model_type,
          Equation = equation,
          R_Squared = r.squared,
          Adj_R_Squared = adj.r.squared,
          P_Value = p.value,
          AIC = AIC,
          BIC = BIC,
          Num_Obs = nobs
        ) %>%
        mutate_if(is.numeric, round, 3) # Round numeric columns
        
      DT::datatable(
        stats_data,
        options = list(
          dom = 't', # Display table only, no search or pagination
          pageLength = 10
        ),
        rownames = FALSE,
        caption = "Model Performance Metrics per Commodity"
      )
      
    } else {
      DT::datatable(
        data.frame(Message = "No statistics available. Run an analysis for a single commodity to see model performance."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # Slot summary outputs
  output$slot_1_summary <- renderText({
    if (!is.null(values$stored_params$slot_1)) {
      params <- values$stored_params$slot_1
      paste0("Saved: ", format(params$timestamp, "%m/%d %H:%M"), "\n",
             params$commodity, "\n", 
             params$variable)
    } else {
      "Empty"
    }
  })
  
  output$slot_2_summary <- renderText({
    if (!is.null(values$stored_params$slot_2)) {
      params <- values$stored_params$slot_2
      paste0("Saved: ", format(params$timestamp, "%m/%d %H:%M"), "\n",
             params$commodity, "\n", 
             params$variable)
    } else {
      "Empty"
    }
  })
  
  output$slot_3_summary <- renderText({
    if (!is.null(values$stored_params$slot_3)) {
      params <- values$stored_params$slot_3
      paste0("Saved: ", format(params$timestamp, "%m/%d %H:%M"), "\n",
             params$commodity, "\n", 
             params$variable)
    } else {
      "Empty"
    }
  })
  
  output$slot_4_summary <- renderText({
    if (!is.null(values$stored_params$slot_4)) {
      params <- values$stored_params$slot_4
      paste0("Saved: ", format(params$timestamp, "%m/%d %H:%M"), "\n",
             params$commodity, "\n", 
             params$variable)
    } else {
      "Empty"
    }
  })
  
  # Display current stored analysis summary
  output$current_analysis_info <- renderText({
    if (!is.null(values$current_stored_slot) && !is.null(values$stored_params[[paste0("slot_", values$current_stored_slot)]])) {
      params <- values$stored_params[[paste0("slot_", values$current_stored_slot)]]
      paste(
        paste("Slot", values$current_stored_slot, "- Loaded at:", Sys.time()),
        paste("Commodity:", params$commodity),
        paste("Market Segment:", params$market_segment),
        paste("Variable:", params$variable),
        paste("Geography:", params$geography),
        paste("Year Unit:", params$year_unit),
        paste("Value Unit:", params$value_unit),
        paste("Time Period:", params$year_min, "-", params$year_max),
        paste("Years to Predict:", params$years_predicted),
        paste("Monthly Data:", params$monthly),
        paste("Originally saved:", format(params$timestamp, "%Y-%m-%d %H:%M:%S")),
        sep = "\n"
      )
    } else {
      "No analysis selected. Please load an analysis from one of the slots above."
    }
  })
  
  # Display stored analysis results
  output$stored_result_plot <- renderPlotly({
    # Read plot directly from the reactive values for stored analysis
    if (!is.null(values$current_stored_analysis) && !is.null(values$current_stored_analysis$plot)) {
      ggplotly(values$current_stored_analysis$plot, tooltip = c("x","y"))
    } else {
      plotly_empty() %>% 
        layout(title = "No stored analysis loaded. Please select a slot with a saved analysis.")
    }
  })
  
  output$stored_result_table <- DT::renderDataTable({
    # Read data directly from the reactive values
    if (!is.null(values$current_stored_analysis) && !is.null(values$current_stored_analysis$data)) {
      DT::datatable(
        values$current_stored_analysis$data,
        options = list(
          scrollX = TRUE,
          scrollY = "60vh",
          scrollCollapse = TRUE,
          pageLength = 15,
          lengthMenu = list(c(15, 25, 50, -1), c('15', '25', '50', 'All'))
        )
      )
    } else {
      DT::datatable(data.frame(Message = "No stored analysis loaded. Please select a slot with a saved analysis."))
    }
  })
  
  output$stored_result_stats <- renderText({
    if (!is.null(values$current_stored_analysis) && !is.null(values$current_stored_slot)) {
      data <- values$current_stored_analysis$data
      params <- values$stored_params[[paste0("slot_", values$current_stored_slot)]]
      summaries <- values$current_stored_analysis$model_summaries
      
      # Build stats text
      stats_text <- paste(
        "Stored Analysis Summary:",
        paste("Slot:", values$current_stored_slot),
        paste("Number of observations:", nrow(data)),
        paste("Time period:", min(data$year_value_ex, na.rm = TRUE), "to", max(data$year_value_ex, na.rm = TRUE)),
        sep = "\n"
      )
      
      if (!is.null(summaries)) {
        # Format model summaries into a string
        summary_str <- summaries %>%
          mutate_if(is.numeric, round, 3) %>%
          rename(Commodity = commodity_element, R_Squared = r.squared) %>%
          select(Commodity, model_type, equation, R_Squared, p.value, AIC) %>%
          format_delim(delim = "\n")
          
        stats_text <- paste(stats_text, "\nModel Performance:\n", summary_str, sep = "")
      }

      stats_text
    } else {
      "No stored analysis statistics available. Please select a slot with saved analysis."
    }
  })
  
  # Download handlers for CSV export
  
  # Download current analysis data
  output$download_current_data <- downloadHandler(
    filename = function() {
      if (!is.null(values$analysis_result)) {
        # Create descriptive filename based on analysis parameters
        commodity <- input$commodity %||% "Unknown"
        variable <- input$variable %||% "Unknown"
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        
        # Clean commodity and variable names for filename
        commodity_clean <- gsub("[^A-Za-z0-9_]", "_", commodity)
        variable_clean <- gsub("[^A-Za-z0-9_]", "_", variable)
        
        paste0("FTN_Analysis_", commodity_clean, "_", variable_clean, "_", timestamp, ".csv")
      } else {
        paste0("FTN_Analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      }
    },
    content = function(file) {
      if (!is.null(values$analysis_result) && exists("Output_table", envir = .GlobalEnv)) {
        # Get the current analysis data
        data_to_export <- Output_table
        
        # Add metadata as comments at the top (as additional columns)
        metadata <- data.frame(
          Analysis_Type = "Current Analysis",
          Export_Timestamp = as.character(Sys.time()),
          Commodity = input$commodity %||% "Not specified",
          Market_Segment = input$market_segment %||% "Not specified",
          Variable = input$variable %||% "Not specified",
          Geography = input$geography %||% "Not specified",
          Year_Unit = input$year_unit %||% "Not specified",
          Value_Unit = input$value_unit %||% "Not specified",
          Year_Range = paste(input$year_min, "to", input$year_max),
          Years_Predicted = input$years_predicted,
          Monthly_Data = input$monthly,
          stringsAsFactors = FALSE
        )
        
        # Write metadata first, then the data
        write.csv(metadata, file, row.names = FALSE, na = "")
        write.table("", file, append = TRUE, col.names = FALSE, row.names = FALSE)
        write.table("=== ANALYSIS DATA ===", file, append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
        write.table("", file, append = TRUE, col.names = FALSE, row.names = FALSE)
        write.csv(data_to_export, file, row.names = FALSE, append = TRUE, na = "")
        
      } else {
        # No data available
        write.csv(data.frame(Message = "No analysis data available for download"), 
                 file, row.names = FALSE)
      }
    }
  )
  
  # Download stored analysis data
  output$download_stored_data <- downloadHandler(
    filename = function() {
      if (!is.null(values$current_stored_analysis) && !is.null(values$current_stored_slot)) {
        params <- values$stored_params[[paste0("slot_", values$current_stored_slot)]]
        
        # Create descriptive filename based on stored analysis parameters
        commodity <- params$commodity %||% "Unknown"
        variable <- params$variable %||% "Unknown"
        slot_num <- values$current_stored_slot
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        
        # Clean commodity and variable names for filename
        commodity_clean <- gsub("[^A-Za-z0-9_]", "_", commodity)
        variable_clean <- gsub("[^A-Za-z0-9_]", "_", variable)
        
        paste0("FTN_Stored_Slot", slot_num, "_", commodity_clean, "_", variable_clean, "_", timestamp, ".csv")
      } else {
        paste0("FTN_Stored_Analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      }
    },
    content = function(file) {
      if (!is.null(values$current_stored_analysis) && !is.null(values$current_stored_slot)) {
        # Get the stored analysis data
        data_to_export <- values$current_stored_analysis$data
        params <- values$stored_params[[paste0("slot_", values$current_stored_slot)]]
        
        # Add metadata as comments at the top (as additional columns)
        metadata <- data.frame(
          Analysis_Type = paste("Stored Analysis - Slot", values$current_stored_slot),
          Export_Timestamp = as.character(Sys.time()),
          Original_Save_Time = as.character(params$timestamp),
          Commodity = params$commodity %||% "Not specified",
          Market_Segment = params$market_segment %||% "Not specified",
          Variable = params$variable %||% "Not specified",
          Geography = params$geography %||% "Not specified",
          Year_Unit = params$year_unit %||% "Not specified",
          Value_Unit = params$value_unit %||% "Not specified",
          Year_Range = paste(params$year_min, "to", params$year_max),
          Years_Predicted = params$years_predicted,
          Monthly_Data = params$monthly,
          stringsAsFactors = FALSE
        )
        
        # Write metadata first, then the data
        write.csv(metadata, file, row.names = FALSE, na = "")
        write.table("", file, append = TRUE, col.names = FALSE, row.names = FALSE)
        write.table("=== ANALYSIS DATA ===", file, append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
        write.table("", file, append = TRUE, col.names = FALSE, row.names = FALSE)
        write.csv(data_to_export, file, row.names = FALSE, append = TRUE, na = "")
        
      } else {
        # No stored data available
        write.csv(data.frame(Message = "No stored analysis data available for download. Please select a stored analysis first."), 
                 file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server) 