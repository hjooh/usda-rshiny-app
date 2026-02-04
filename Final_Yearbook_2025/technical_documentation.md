# FTN Yearbook Technical Documentation

## System Overview
The system has two major subsystems:

1. **Processing** (`FTN Yearbook functions/processing/`)
   - Reads the Excel yearbook.
   - Normalizes tables into standardized flat CSVs.
   - Produces section files and a combined flatfile for analysis.

2. **Analysis App** (`FTN Yearbook functions/app/`)
   - Loads the flatfile output.
   - Filters, models, and visualizes time series.
   - Provides a Shiny UI and programmatic functions.

## Compatibility and Requirements

### R Version
- R 4.0+ recommended (newer versions supported)

### Required R Packages
- App/UI: `shiny`, `shinydashboard`, `DT`, `plotly`, `shinyjs`
- Analysis: `dplyr`, `readxl`, `lubridate`, `stringr`, `readr`, `tidyr`, `ggplot2`
- Modeling: `broom`, `forcats`, `moderndive`, `rlang`, `fastDummies`, `cowplot`

### RStudio Notes
- All scripts can be run from RStudio by setting the working directory appropriately.
- `launch_app.R` handles package checks and path resolution for the Shiny app.

## Data Flow
```
Input/Yearbook_2024_revised.xlsm
  └─ processing/master.R
       ├─ process_section_A.R … process_section_H.R
       └─ utils.R (shared helpers)
            ↓
Output/A_flat.csv … Output/H_flat.csv
Output/FTN_Yearbook_all_sections.csv
            ↓
app/code-tline-5.R + app/shiny_app.R
```

## Processing Subsystem

### Entry Point
- `processing/master.R` orchestrates all sections.
- CLI usage:
  - `Rscript "FTN Yearbook functions/processing/master.R" "Input/Yearbook_2024_revised.xlsm" "Output" FALSE "A,B,C,D,E,F,G,H"`

### Section Scripts
Each `process_section_*.R`:
- Reads a worksheet by name (e.g., `A-1`, `B-12`).
- Extracts metadata (table title, time unit, market).
- Locates the data end row.
- Converts wide table data into long-format rows.

### Shared Utilities (`processing/utils.R`)
Key responsibilities:
- `load_yearbook_sheet()` reads worksheets with `readxl`.
- `extract_table_metadata()` parses the title for commodity, time unit, market.
- `find_data_end()` finds where valid rows end by year patterns.
- `process_worksheet()` cleans headers, pivots long, splits variable/unit.
- `handle_special_case()` handles table-specific formatting quirks.
- `save_to_csv()` writes outputs (with optional backups).

### Processing Output Schema
Processing output files use these fields:
- `table_name`
- `time_value`
- `time_unit`
- `month`
- `variable`
- `commodity_element`
- `market_segment`
- `geographic_extent`
- `value`
- `unit`
- `section` (combined file)
- `is_forecast`, `is_comparison`, `category`, `section_note` (combined file)

## Analysis / App Subsystem

### Core Pipeline
1. `code-utils-1.R`
   - `read_ftn_data()` loads CSV/XLSX and normalizes schema.
   - Normalizes processing output (`time_*` → `year_*`).
   - Ensures `year_start_month` exists for modeling.

2. `code-dataprep-2.R`
   - `filter_yearbook_data()` applies commodity/segment/variable/geography filters.
   - Handles aliases (e.g., `United States` → `National`).
   - `prepare_time_series()` handles monthly/annual selection and ranges.

3. `code-modeling-3.R`
   - Fits time-series models and builds prediction intervals.
   - `visualize_predictions()` builds ggplot + plotly charts.

4. `code-stats-4.R`
   - Produces model summary stats for reporting.

5. `code-tline-5.R`
   - The main orchestrator function `tline()`.
   - Loads data → filters → prepares → models → visualizes.
   - Returns a structured result list.

### Shiny App
`app/shiny_app.R` uses the same pipeline functions:
- Loads data once into `global_data`.
- Drives filters via UI inputs.
- Runs `tline()` for the analysis tab.
- Stores multiple analyses in reactive slots.

### App Launchers
`launch_app.R` is the recommended launcher:
- Checks required packages.
- Resolves paths and validates files.
- Verifies data file availability.
- Runs `shiny::runApp()`.

## Schema Normalization

The analysis system expects the legacy schema:
- `year_value`, `year_unit`, `year_start_month`

The processing output uses:
- `time_value`, `time_unit`

`read_ftn_data()` bridges this by renaming fields and filling defaults:
- `time_value` → `year_value`
- `time_unit` → `year_unit`
- `year_start_month` defaulted (Marketing year → October, Calendar year → January)
- `year_unit` value `"Year"` normalized to `"Calendar year"`

## Known Assumptions

- The Excel yearbook uses consistent sheet naming (e.g., `A-1`, `B-2`).
- Table titles contain commodity and time unit cues.
- Missing/blank header cells are normalized during processing.
- The analysis layer expects numeric year values derived from `year_value`.

## Extension Points

- Add new tables: implement `process_section_X.R`.
- Add new variables: handled automatically if present in the data.
- Add new app features: extend `shiny_app.R` and reuse pipeline functions.

