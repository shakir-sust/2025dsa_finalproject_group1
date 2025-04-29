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

#───────────────────────────────────────────────────────────────────────────────
# SERVER
#───────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # 1) Predicted vs Observed
  output$predObsPlot <- renderPlot({
    r2_val   <- rsq_vec(truth = test_preds_xgb$yield, estimate = test_preds_xgb$.pred)
    rmse_val <- rmse_vec(truth = test_preds_xgb$yield, estimate = test_preds_xgb$.pred)
    
    ggplot(test_preds_xgb, aes(x = yield, y = .pred)) +
      geom_abline() +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title    = "XGBoost: Predicted vs Observed Yield",
        subtitle = paste0("R² = ", round(r2_val, 3),
                          "   RMSE = ", round(rmse_val, 3)),
        x = "Observed Yield",
        y = "Predicted Yield"
      ) +
      theme_minimal()
  })
  
  # 2) Variable Importance (from precomputed vi_top20)
  output$vipPlot <- renderPlot({
    ggplot(vi_top20, aes(
      x = Importance,
      y = fct_reorder(Variable, Importance)
    )) +
      geom_col() +
      labs(
        title = "XGBoost Variable Importance",
        x     = "Importance",
        y     = "Feature"
      ) +
      theme_minimal()
  })
  
  # 3) Soil EDA ridge plot
  output$soilPlot <- renderPlot({
    req(input$soil_var)
    var   <- input$soil_var
    label <- if (var == "soilpH") "soil pH" else "soil organic matter"
    
    df        <- weather_xgb %>% mutate(val = .data[[var]])
    dens      <- density(df$val, na.rm = TRUE)
    scale_val <- 1 / max(dens$y, na.rm = TRUE)
    
    ggplot(df, aes(x = val, y = label, fill = stat(x))) +
      geom_density_ridges_gradient(
        scale          = scale_val,
        rel_min_height = 0.01
      ) +
      scale_fill_viridis_c(option = "C") +
      labs(title = paste("Distribution of", label), x = NULL, y = NULL) +
      theme_ridges() +
      theme(legend.position = "none", axis.text.y = element_blank())
  })
  
  # 4) September Weather EDA ridge plot
  output$sepPlot <- renderPlot({
    req(input$sep_var)
    var   <- input$sep_var
    label <- switch(
      var,
      mean_srad.wm2_Sep = "Mean solar radiation in September",
      mean_tmax.c_Sep   = "Mean maximum temperature in September"
    )
    
    df        <- weather_xgb %>% mutate(val = .data[[var]])
    dens      <- density(df$val, na.rm = TRUE)
    scale_val <- 1 / max(dens$y, na.rm = TRUE)
    
    ggplot(df, aes(x = val, y = label, fill = stat(x))) +
      geom_density_ridges_gradient(
        scale          = scale_val,
        rel_min_height = 0.01
      ) +
      scale_fill_viridis_c(option = "C") +
      scale_x_continuous(position = "top") +
      labs(title = paste("Distribution of", label), x = NULL, y = NULL) +
      theme_ridges() +
      theme(legend.position = "none", axis.text.y = element_blank())
  })
  
  # 5) Yield distribution train vs test
  output$yieldDist <- renderPlot({
    ggplot(yield_df, aes(x = yield, color = dataset)) +
      geom_density(size = 1) +
      scale_color_manual(
        values = c(Training = "red", Test = "blue"),
        name   = "Data split"
      ) +
      labs(
        title = 'Comparing the distribution of "yield" in training vs. test sets',
        x     = "Yield (Mg/ha)",
        y     = "Density"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
}

#───────────────────────────────────────────────────────────────────────────────
# Run the app
#───────────────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
