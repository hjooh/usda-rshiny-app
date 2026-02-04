# FTN Yearbook Usage Guide

This guide covers all supported ways to run the code: processing only, analysis only, and the full app (processing and analysis).

## Compatibility and Requirements

### R Version
- R 4.0+ recommended (newer versions supported)

### Required R Packages
The launchers will prompt to install missing packages. Core packages include:
- App/UI: `shiny`, `shinydashboard`, `DT`, `plotly`, `shinyjs`
- Analysis: `dplyr`, `readxl`, `lubridate`, `stringr`, `readr`, `tidyr`, `ggplot2`
- Modeling: `broom`, `forcats`, `moderndive`, `rlang`, `fastDummies`, `cowplot`

## Running in RStudio (GUI)

If you prefer RStudio over the CLI:

**Processing only**
1. Open `FTN Yearbook functions/processing/master.R`
2. Set your working directory to `Final_Yearbook_2025`
3. Run:
   ```r
   source("FTN Yearbook functions/processing/master.R")
   process_yearbook(
     yearbook_file = "Input/Yearbook_2024_revised.xlsm",
     output_dir = "Output",
     save_individual = FALSE,
     quiet = FALSE,
     sections = c("A","B","C","D","E","F","G","H")
   )
   ```

**Analysis (no app)**
1. Open `FTN Yearbook functions/app/code-tline-5.R`
2. Run `tline()` in the console (see examples below).

**Shiny app**
1. Open `FTN Yearbook functions/app/launch_app.R`
2. Run `source("launch_app.R")` in the RStudio console.

## 1) Processing Only (Excel → Flatfiles)

**Goal:** Convert the yearbook Excel file into standardized CSVs.

From `Final_Yearbook_2025`:

```powershell
Rscript "FTN Yearbook functions/processing/master.R" "Input/Yearbook_2024_revised.xlsm" "Output" FALSE "A,B,C,D,E,F,G,H"
```

Outputs:
- `Output/A_flat.csv` … `Output/H_flat.csv`
- `Output/FTN_Yearbook_all_sections.csv`

## 2) Analysis from the Flatfile (No App)

**Goal:** Run a single analysis in R without the Shiny app.

From `FTN Yearbook functions/app`:

```r
source("code-tline-5.R")

result <- tline(
  Commodity = "Almonds",
  Market_segment = "All",
  Variable = "Value of production",
  Geography = "United States",
  Year_unit = "Marketing year",
  Value_unit = "index",
  Year_min = 2000,
  Year_max = 2023,
  Monthly = "No",
  Years_predicted = 5,
  Interactive = FALSE,
  Data_file = "../../Output/FTN_Yearbook_all_sections.csv"
)
```

Results:
- `result$data` (filtered + modeled data)
- `result$plot` (plotly chart)
- `result$args` and `result$query`

## 3) Launch the Shiny App (Recommended)

From `FTN Yearbook functions/app`:

```r
source("launch_app.R")
```

This:
- Checks packages
- Resolves the correct folder
- Validates required files
- Verifies the data file
- Launches `shiny_app.R`

## 4) Run the Demo Scripts

From `FTN Yearbook functions/app`:

```powershell
Rscript code-examples.R
Rscript demo_ers_theme.R
Rscript test_multiple_plots.R
```

These are for demonstrations and validation; they don’t write outputs unless explicitly enabled.

## 5) Data File Locations

The analysis code searches for the flatfile in these common locations:
- `Output/FTN_Yearbook_all_sections.csv` (preferred)
- `../Output/FTN_Yearbook_all_sections.csv`
- Legacy `Fruit_Treenut_Flatfile.csv` locations

If you want to point at a custom file, pass `Data_file = "full/path/to/file.csv"` to `tline()`.

