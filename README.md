# FTN Yearbook App

This repository contains the FTN Yearbook processing scripts and a Shiny app for filtering, modeling, forecasting, and visualizing the processed fruit and tree nut data.

## Project Layout

- `src/Input/Yearbook_2024_revised.xlsm` - source Excel workbook
- `src/functions/processing/master.R` - processing entry point
- `src/functions/app/launch_app.R` - recommended Shiny app launcher
- `src/functions/app/shiny_app.R` - Shiny app
- `src/Output/FTN_Yearbook_all_sections.csv` - combined processed flatfile used by the app

## Requirements

Use R 4.0 or newer.

The app and processing scripts use these R packages:

- App/UI: `shiny`, `shinydashboard`, `DT`, `plotly`, `shinyjs`
- Data and analysis: `dplyr`, `readxl`, `lubridate`, `stringr`, `readr`, `tidyr`, `ggplot2`
- Modeling: `broom`, `forcats`, `moderndive`, `rlang`, `fastDummies`, `cowplot`

`src/functions/app/launch_app.R` checks for missing packages and offers to install them.

## Run the Shiny App

### Option 1: RStudio Console

1. Open the project folder in RStudio.
2. Set the working directory to the app folder:

```r
setwd("src/functions/app")
```

3. Launch the app:

```r
source("launch_app.R")
```

If you opened `src/functions/app/launch_app.R` directly in RStudio and used **Session > Set Working Directory > To Source File Location**, only the `source("launch_app.R")` step is needed.

The launcher checks packages, validates required app files, finds the data file, and starts the Shiny app.

### Option 2: RStudio Terminal

From the project root:

```powershell
cd src/functions/app
Rscript -e "source('launch_app.R')"
```

Alternative from the project root without changing folders:

```powershell
Rscript -e "setwd('src/functions/app'); source('launch_app.R')"
```

### Option 3: Another Terminal

Open PowerShell, Command Prompt, Windows Terminal, or another shell at the project root, then run:

```powershell
cd src/functions/app
Rscript -e "source('launch_app.R')"
```

For a headless/local-server launch without opening a browser:

```powershell
cd src/functions/app
Rscript -e "shiny::runApp('shiny_app.R', launch.browser=FALSE, host='127.0.0.1', port=3867)"
```

Then open `http://127.0.0.1:3867` in a browser.

## Rebuild the Processed Data

From the project root:

```powershell
cd src
Rscript "functions/processing/master.R" "Input/Yearbook_2024_revised.xlsm" "Output" FALSE "A,B,C,D,E,F,G,H"
```

This writes:

- `src/Output/A_flat.csv` through `src/Output/H_flat.csv`
- `src/Output/FTN_Yearbook_all_sections.csv`

The processing script backs up existing output files before overwriting them.

## Run an Analysis Without the App

From the project root:

```powershell
cd src/functions/app
Rscript -e "source('code-tline-5.R'); result <- tline(Commodity='Pistachios', Market_segment='All', Variable='Utilized production', Geography='National', Year_unit='Marketing year', Value_unit='thousand pounds, shelled basis', Year_min=2000, Year_max=2023, Monthly='No', Years_predicted=5, Interactive=FALSE, Data_file='../../Output/FTN_Yearbook_all_sections.csv'); print(result[['query']])"
```

In an interactive R session:

```r
setwd("src/functions/app")
source("code-tline-5.R")

result <- tline(
  Commodity = "Pistachios",
  Market_segment = "All",
  Variable = "Utilized production",
  Geography = "National",
  Year_unit = "Marketing year",
  Value_unit = "thousand pounds, shelled basis",
  Year_min = 2000,
  Year_max = 2023,
  Monthly = "No",
  Years_predicted = 5,
  Interactive = FALSE,
  Data_file = "../../Output/FTN_Yearbook_all_sections.csv"
)
```

The result includes the filtered/model data, plot object, model summaries, and query arguments.

## Quick Health Checks

From the project root:

```powershell
Rscript -e "setwd('src/functions/app'); app <- source('shiny_app.R')`$value; print(class(app))"
```

Expected output includes:

```text
[1] "shiny.appobj"
```

To confirm the app serves HTTP locally:

```powershell
cd src/functions/app
Rscript -e "shiny::runApp('shiny_app.R', launch.browser=FALSE, host='127.0.0.1', port=3867)"
```

Then visit `http://127.0.0.1:3867`.
