# USDA FTN Yearbook Analysis App

This repository contains:
- processing scripts that flatten FTN Yearbook Excel tables into analysis-ready CSV files, and
- an R Shiny app for filtering, forecasting, and visualizing the resulting time-series data.

## Modeling method used in the app

The Shiny app currently uses **linear regression (`lm`)**, not ARIMA.

- Annual mode (`Monthly Data = "No"`):  
  `value ~ year_num`
- Monthly mode (`Monthly Data = "Yes"`):  
  `value ~ year_value_ex + month_<dummy variables>`

Forecasts are generated from these linear models with 80% and 95% prediction intervals.

## Where to see model details

- In the app’s **Statistics** tab, each commodity now shows:
  - model type,
  - fitted equation, and
  - performance metrics (`R²`, adjusted `R²`, p-value, AIC, BIC, observations).

