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
library(countrycode)

################################################################################
source(here("global.R"))             
source(here("bubble_chart.R"))     
source(here("heatmap.R"))          
source(here("hist.R"))           
source(here("bubble_map1.R"))
source(here("box_plot_module.R"))  
source(here("ETL.R"), local = TRUE)

# Load Standard des données
df <- etl_powerplants("global_power_plant_database.csv")

#########################################################################
# Fonction pour créer un graphique à barres animé
prepare_data_for_bar <- function(df) {
  df <- df %>% filter(!is.na(commissioning_year))
  if (nrow(df) == 0) return(tibble())
  years <- unique(df$commissioning_year)
  if (length(years) == 0 || all(is.na(years))) return(tibble())
  
  df %>%
    mutate(commissioning_year = as.integer(commissioning_year)) %>%
    group_by(country, primary_fuel, commissioning_year) %>%
    summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop') %>%
    complete(country, primary_fuel, commissioning_year = full_seq(commissioning_year, 1), fill = list(capacity_mw = 0)) %>%
    arrange(country, primary_fuel, commissioning_year) %>%
    group_by(country, primary_fuel) %>%
    mutate(capacity_mw = cumsum(capacity_mw)) %>%
    ungroup() %>%
    mutate(frame = paste0("Année: ", commissioning_year)) %>%
    arrange(commissioning_year, desc(capacity_mw)) %>%
    group_by(commissioning_year, country) %>%
    mutate(total_capacity = sum(capacity_mw)) %>%
    ungroup()
}


# Si on veut grouper pour les top 10
plot_histogram <- function(df) {
  if (nrow(df) == 0) return(NULL)
  df_prepared <- prepare_data_for_bar(df)
  df_top10 <- df_prepared %>%
    group_by(commissioning_year) %>%
    slice_max(order_by = total_capacity, n = 10, with_ties = FALSE)
  df_final <- semi_join(df_prepared, df_top10, by = c("commissioning_year", "country"))
  animated_bar(df_final)
}
#########################################################################



#########################################################################
# Pour les données de la carte heatmap.R
df_country <- df %>%
  group_by(country_long, primary_fuel) %>%
  summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop')

# Ajoute le code ISO-3
df_country$iso3 <- countrycode(df_country$country_long, "country.name", "iso3c")

pad_countries <- function(df_country, country_ref, fuel = NULL) {
  if (is.null(fuel)) {
    # Toutes énergies confondues
    d <- country_ref %>%
      left_join(
        df_country %>%
          group_by(country_long, iso3) %>%
          summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = 'drop'),
        by = c("country_long", "iso3")
      )
  } else {
    # Lorsqu'on filtre
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


#########################################################################

summary_by_fuel <- function(df, year_max) {
  df %>%
    filter(!is.na(commissioning_year), commissioning_year <= year_max) %>%
    group_by(primary_fuel) %>%
    summarise(`Capacité Totale (MW)` = sum(capacity_mw, na.rm = TRUE)) %>%
    arrange(desc(`Capacité Totale (MW)`))
}
#########################################################################

# Preprocess data: clean and prepare for animation
df_clean <- df %>%
  filter(!is.na(commissioning_year), !is.na(capacity_mw)) %>%
  mutate(
    commissioning_year = as.integer(commissioning_year),
    primary_fuel = str_to_title(str_trim(primary_fuel)),
    plant_age = lubridate::year(Sys.Date()) - commissioning_year
  ) %>%
  filter(commissioning_year >= 1950)
#########################################################################
