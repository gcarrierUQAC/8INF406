# =====================================================================
# app.R
# =====================================================================
library(shiny)
library(plotly)
library(dplyr)
library(here)
library(ggplot2)
library(gganimate)
library(purrr)
library(tidyr)
library(janitor)
library(readr)
library(stringr)
library(lazyeval)

# ---- Fichiers externes (packages, données, fonctions, modules) -------
source("global.R")            # charge df     et df_map
source("R/bubble_chart.R")    # -> bubble_map()
source("R/utils.R")
source("R/mod_bubble.R")      

# ------------------------------- UI ----------------------------------
ui <- fluidPage(
  titlePanel("Centrales électriques – 2 jeux de données"),
  
  tabsetPanel(
    # ------- Onglet 1 : carte long/lat --------------------------------
    tabPanel(
      "Carte",
      plotly::plotlyOutput("map_plot", height = 650)
    ),
    
    # ------- Onglet 2 : animation cumulative --------------------------
    tabPanel(
      "Capacité cumulative",
      mod_bubble_ui("cum_bubble")
    )
  )
)

# ----------------------------- SERVER --------------------------------
server <- function(input, output, session) {
  
  # ---- Carte interactive (df_map) ----
  output$map_plot <- plotly::renderPlotly({
    bubble_map(
      data   = df_map,
      x      = "longitude",
      y      = "latitude",
      size   = "capacity_mw",
      color  = "primary_fuel",
      symbol = "primary_fuel"
    )
  })
  
  # ---- Animation cumulative (df) ----
  mod_bubble_server(
    id      = "cum_bubble",
    data_r  = df_time
  )
}

# ---------------------------- LANCEMENT ------------------------------
shinyApp(ui, server)
