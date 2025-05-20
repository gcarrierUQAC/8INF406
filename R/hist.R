library(plotly)
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

couleurs_marker <- c(
  "Hydro"  = "#00fa25", "Gas" = "#B8860B", "Oil" = "#B8860B", "Other" = "#FF69B4", 
  "Nuclear" = "#d6d318", "Coal" = "#A52A2A", "Wind" = "#00fa25", "Biomass" = "#B8860B", 
  "Waste" = "#B8860B", "Solar" = "#00fa25", "Geothermal" = "#00fa25", "Storage" = "#00fa25", 
  "Cogeneration" = "#FF1493", "Petcoke" = "#3d3d3d", "Wave And Tidal" = "#00fa25"
)

animated_bar <- function(data) {
  plot_ly(
    data,
    x = ~capacity_mw,
    y = ~country,
    color = ~primary_fuel,
    colors = couleurs_marker,
    frame = ~frame,
    type = 'bar',
    orientation = 'h',
    text = ~paste("Énergie:", primary_fuel, "<br>MW:", round(capacity_mw)),
    hoverinfo = "text"
  ) %>%
    layout(
      title = "Évolution de la capacité installée par pays et type d'énergie",
      xaxis = list(title = "Capacité installée (MW)"),
      yaxis = list(title = "Pays"),
      barmode = 'stack'
    )%>% arrange(desc(capacity_mw)) %>%
    group_by(country) %>%
    mutate(y = reorder(country, capacity_mw)) %>%
    ungroup() %>%
    plotly::layout(
      yaxis = list(title = "Pays", tickvals = ~y, ticktext = ~country)
    ) %>%
    animation_opts(
      frame = 1000,
      transition = 0,      
      redraw = FALSE,
      mode = "immediate"
    )
}

source("R/ETL.R")
# Test de la fonction animated_bar
df <- etl_powerplants("data/global_power_plant_database.csv")
# Grouper les données par pays et type d'énergie, en sommans toutes les années précédantes à chaque année
# si aucune année copier la précédante
df_agg <- df %>%
  filter(!is.na(commissioning_year)) %>%
  mutate(commissioning_year = as.integer(commissioning_year)) %>%
  group_by(country, primary_fuel, commissioning_year) %>%
  summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop') %>%
  
  # Étendre à toutes les années possibles par groupe
  complete(
    country, primary_fuel, commissioning_year = full_seq(commissioning_year, 1),
    fill = list(capacity_mw = 0)
  ) %>%
  
  arrange(country, primary_fuel, commissioning_year) %>%
  group_by(country, primary_fuel) %>%
  mutate(capacity_mw = cumsum(capacity_mw)) %>%
  ungroup() %>%
  mutate(frame = paste0("Année: ", commissioning_year)) %>%
  arrange(commissioning_year, desc(capacity_mw))

df_top10 <- df_agg %>%
  group_by(commissioning_year, country) %>%
  summarise(total_capacity = sum(capacity_mw), .groups = 'drop') %>%
  group_by(commissioning_year) %>%
  slice_max(order_by = total_capacity, n = 10, with_ties = FALSE) %>%
  ungroup()

df_agg <- df_agg %>%
  semi_join(df_top10, by = c("commissioning_year", "country"))

animated_bar(df_agg)
