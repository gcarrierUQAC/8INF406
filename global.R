# global.R
# -*- coding: UTF-8 -*-

# Packages nécessaires
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(gganimate)
library(purrr)
library(tidyr)
library(janitor)
library(readr)
library(stringr)
library(lazyeval)
library(countrycode)
library(lubridate)

################################################################################
# Charger les fonctions et modules
source("www/heatmap.R", encoding = "UTF-8")  
source("www/hist.R", encoding = "UTF-8")         
source("www/bubble_map.R", encoding = "UTF-8")
source("www/global_energy_capacity.R", encoding = "UTF-8")
source("www/ETL.R", encoding = "UTF-8")

################################################################################
# Chargement et prétraitement des données principales
df <- etl_powerplants("www/global_power_plant_database.csv")

# DataFrame pays + énergie (pour choropleth et autres viz)
df_country <- df %>%
  group_by(country_long, primary_fuel) %>%
  summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop')

# Ajout code ISO-3 pour les cartes
df_country$iso3 <- countrycode(df_country$country_long, "country.name", "iso3c")

# Nettoyage de base (pour animations, filtres, etc.)
df_clean <- df %>%
  filter(!is.na(commissioning_year), !is.na(capacity_mw)) %>%
  mutate(
    commissioning_year = as.integer(commissioning_year),
    primary_fuel = str_to_title(str_trim(primary_fuel)),
    plant_age = year(Sys.Date()) - commissioning_year
  ) %>%
  filter(commissioning_year >= 1910)

################################################################################
# Fonctions utilitaires

prepare_data_for_bar <- function(df) {
  df <- df %>% filter(!is.na(commissioning_year))
  if (nrow(df) == 0) return(tibble())
  years <- unique(df$commissioning_year)
  if (length(years) == 0 || all(is.na(years))) return(tibble())
  
  df %>%
    mutate(commissioning_year = as.integer(commissioning_year)) %>%
    group_by(country_long, primary_fuel, commissioning_year) %>%    
    summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop') %>%
    complete(country_long, primary_fuel, commissioning_year = full_seq(commissioning_year, 1), fill = list(capacity_mw = 0)) %>%
    arrange(country_long, primary_fuel, commissioning_year) %>%
    group_by(country_long, primary_fuel) %>%
    mutate(capacity_mw = cumsum(capacity_mw)) %>%
    ungroup() %>%
    mutate(frame = paste0("Année: ", commissioning_year)) %>%
    arrange(commissioning_year, desc(capacity_mw)) %>%
    group_by(commissioning_year, country_long) %>%                 
    mutate(total_capacity = sum(capacity_mw)) %>%
    ungroup()
}


# Fonction pour pad les pays pour la choropleth (tous fuels ou filtré)
pad_countries <- function(df_country, country_ref, fuel = NULL) {
  if (is.null(fuel)) {
    d <- country_ref %>%
      left_join(
        df_country %>%
          group_by(country_long, iso3) %>%
          summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop'),
        by = c("country_long", "iso3")
      )
  } else {
    d <- country_ref %>%
      left_join(
        df_country %>%
          filter(primary_fuel %in% fuel) %>%
          group_by(country_long, iso3) %>%
          summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop'),
        by = c("country_long", "iso3")
      )
  }
  d %>% mutate(capacity_mw = ifelse(is.na(capacity_mw), 0, capacity_mw))
}

# Résumé global pour la série temporelle animée par type d'énergie
energy_production_per_fuel_type <- df %>%
  filter(!is.na(commissioning_year)) %>%
  mutate(commissioning_year = as.integer(commissioning_year)) %>%
  group_by(commissioning_year, primary_fuel) %>%
  summarise(year_total_capacity = sum(capacity_mw, na.rm = TRUE), .groups = "drop") %>%
  complete(commissioning_year = unique(.$commissioning_year),
           primary_fuel = unique(.$primary_fuel),
           fill = list(year_total_capacity = 0)) %>%
  group_by(primary_fuel) %>%
  mutate(cumulative_capacity = cumsum(year_total_capacity)) %>%
  ungroup()

################################################################################
# Listes pour les selectInput de l’UI
liste_pays <- sort(unique(df$country_long))
liste_fuel <- sort(unique(df$primary_fuel))

################################################################################
# Dataframes et listes pour accueil
top_10_countries <- df_country %>%
  group_by(country_long) %>%
  summarise(total_capacity_mw = sum(capacity_mw, na.rm = TRUE)) %>%
  slice_max(order_by = total_capacity_mw, n = 10) %>%
  arrange(desc(total_capacity_mw))

top_countries_per_fuel_type <- df_country %>%
  group_by(primary_fuel, country_long) %>%
  summarise(total_capacity_mw = sum(capacity_mw, na.rm = TRUE)) %>%
  group_by(primary_fuel) %>%
  slice_max(order_by = total_capacity_mw, n = 3, with_ties = FALSE) %>%
  mutate(primary_fuel = factor(primary_fuel, levels = c("Hydro","Solar","Wind","Storage",
                                                        "Geothermal","Wave And Tidal","Biomass","Waste",
                                                        "Nuclear","Coal","Gas","Oil",
                                                        "Petcoke","Cogeneration","Other"))) %>%
  arrange(primary_fuel)

fuel_icons <- list(
  "Coal" = icon("fire", style = paste0("color:", couleurs_marker["Coal"])),
  "Hydro" = icon("water", style = paste0("color:", couleurs_marker["Hydro"])),
  "Gas" = icon("gas-pump", style = paste0("color:", couleurs_marker["Gas"])),
  "Oil" = icon("oil-well", style = paste0("color:", couleurs_marker["Oil"])),
  "Solar" = icon("solar-panel", style = paste0("color:", couleurs_marker["Solar"])),
  "Wind" = icon("wind", style = paste0("color:", couleurs_marker["Wind"])),
  "Nuclear" = icon("circle-radiation", style = paste0("color:", couleurs_marker["Nuclear"])),
  "Biomass" = icon("leaf", style = paste0("color:", couleurs_marker["Biomass"])),
  "Geothermal" = icon("volcano", style = paste0("color:", couleurs_marker["Geothermal"])),
  "Waste" = icon("dumpster", style = paste0("color:", couleurs_marker["Waste"])),
  "Wave And Tidal" = icon("bridge-water", style = paste0("color:", couleurs_marker["Wave And Tidal"])),
  "Storage" = icon("warehouse", style = paste0("color:", couleurs_marker["Storage"])),
  "Petcoke" = icon("hill-rockslide", style = paste0("color:", couleurs_marker["Petcoke"])),
  "Cogeneration" = icon("bolt-lightning", style = paste0("color:", couleurs_marker["Cogeneration"])),
  "Other" = icon("bolt-lightning", style = paste0("color:", couleurs_marker["Other"]))
)
