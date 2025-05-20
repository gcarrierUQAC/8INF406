library(dplyr)
library(plotly)
library(tidyr)

source("R/ETL.R")

df <- etl_powerplants("data/global_power_plant_database.csv")

df_heatmap <- df %>%
  group_by(country_long, primary_fuel) %>%
  summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop')

top_countries <- df_heatmap %>%
  group_by(country_long) %>%
  summarise(total = sum(capacity_mw)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 15) %>%
  pull(country_long)

heatmap_mat <- df_heatmap %>%
  filter(country_long %in% top_countries) %>%
  pivot_wider(names_from = primary_fuel, values_from = capacity_mw, values_fill = 0)

# Normalisation pour un meilleur affichage
norm_mat <- as.data.frame(heatmap_mat)
fuel_types <- setdiff(names(norm_mat), "country_long")
for (f in fuel_types) {
  col <- norm_mat[[f]]
  rng <- range(col, na.rm = TRUE)
  if (diff(rng) == 0) {
    norm_mat[[f]] <- 0 # tous les pays = 0 si pas de variation
  } else {
    norm_mat[[f]] <- (col - rng[1]) / diff(rng)
  }
}

mat_values <- as.matrix(norm_mat[, fuel_types])
y_countries <- norm_mat$country_long
x_fuels <- fuel_types



fig <- plot_ly(
  x = x_fuels,
  y = y_countries,
  z = mat_values,
  type = "heatmap",
  colorscale = "YlGnBu",
  colorbar = list(title = "Normalisé par type")
) %>%
  layout(
    title = "Capacité installée normalisée par type d'énergie",
    xaxis = list(title = "Type d'énergie"),
    yaxis = list(title = "Pays")
  )
  

fig
