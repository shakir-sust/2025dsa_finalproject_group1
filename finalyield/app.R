# app.R

library(shiny)
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(tidymodels)
library(yardstick)
library(readr)
library(forcats)

#───────────────────────────────────────────────────────────────────────────────
# Data imports (adjust paths as needed)
#───────────────────────────────────────────────────────────────────────────────
final_xgb_fit  <- read_rds("final_xgb_fit.rds")    # not used directly but kept for consistency
weather_xgb    <- read_csv("weather_xgb.csv")      # for Soil & Weather EDA
test_preds_xgb <- read_csv("test_preds_xgb.csv")   # for Pred vs Observed
yield_df       <- read_csv("yield_df.csv")         # for Yield distribution
vi_top20       <- read_csv("vi_top20.csv")         # precomputed top 20 VI

#───────────────────────────────────────────────────────────────────────────────
# UI
#───────────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Corn Trial EDA & XGBoost Dashboard"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.main_tabs == 'Soil EDA'",
        selectInput(
          "soil_var", "Choose soil variable:",
          choices = c(
            "soil pH"             = "soilpH",
            "soil organic matter" = "om_pct"
          )
        )
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Sept Weather EDA'",
        selectInput(
          "sep_var", "Choose Sept. weather var:",
          choices = c(
            "Mean solar radiation" = "mean_srad.wm2_Sep",
            "Mean max temperature" = "mean_tmax.c_Sep"
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Pred vs Observed",    plotOutput("predObsPlot")),
        tabPanel("Variable Importance", plotOutput("vipPlot")),
        tabPanel("Soil EDA",            plotOutput("soilPlot")),
        tabPanel("Sept Weather EDA",    plotOutput("sepPlot")),
        tabPanel("Yield Dist.",         plotOutput("yieldDist"))
      )
    )
  )
)

