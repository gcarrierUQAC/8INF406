library(dplyr)
library(tidyr)
library(plotly)

source("R/ETL.R")
df <- etl_powerplants("data/global_power_plant_database.csv")

selected_country <- "United States of America"

# Step 1: Aggregate and complete missing years/fuels for full line traces
df_agg <- df %>%
  filter(!is.na(commissioning_year)) %>%
  filter(country_long == selected_country) %>%
  group_by(primary_fuel, commissioning_year) %>%
  summarise(total_capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop')

# Complete to ensure every primary_fuel has all years in the range
all_years <- seq(min(df_agg$commissioning_year), max(df_agg$commissioning_year))
all_fuels <- unique(df_agg$primary_fuel)
df_complete <- expand_grid(primary_fuel = all_fuels, commissioning_year = all_years) %>%
  left_join(df_agg, by = c("primary_fuel", "commissioning_year")) %>%
  mutate(total_capacity_mw = ifelse(is.na(total_capacity_mw), 0, total_capacity_mw)) %>%
  arrange(primary_fuel, commissioning_year) %>%
  group_by(primary_fuel) %>%
  mutate(cum_capacity_mw = cumsum(total_capacity_mw)) %>%
  ungroup()

# Step 2: Get last points for annotation
last_points <- df_complete %>%
  group_by(primary_fuel) %>%
  filter(commissioning_year == max(commissioning_year)) %>%
  ungroup()

# Step 3: Animated cumulative line plot with persistent traces and labels
fig <- plot_ly(
  df_complete,
  x = ~commissioning_year,
  y = ~cum_capacity_mw,
  color = ~primary_fuel,
  frame = ~commissioning_year,
  type = 'scatter',
  mode = 'lines+markers',
  hoverinfo = 'text',
  text = ~paste(primary_fuel, "<br>Cumulative:", round(cum_capacity_mw), "MW")
) %>%
  layout(
    title = paste("Capacité cumulée par type d'énergie en", selected_country),
    xaxis = list(title = "Année"),
    yaxis = list(title = "Capacité cumulée (MW)"),
    legend = list(title = list(text = "Type d'énergie"))
  ) %>%
  add_annotations(
    data = last_points,
    x = ~commissioning_year,
    y = ~cum_capacity_mw,
    text = ~primary_fuel,
    showarrow = FALSE,
    xanchor = "left",
    yanchor = "middle",
    font = list(size = 12)
  ) %>%
  animation_opts(
    frame = 600,
    redraw = TRUE
  )

fig
