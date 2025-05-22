library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(tidyr)

etl_powerplants <- function(path = "global_power_plant_database.csv", col_drop_na = "capacity_mw") {
  df <- read_csv(path, show_col_types = FALSE) %>%
    clean_names() %>%
    select(name, country, country_long, capacity_mw, latitude, longitude, primary_fuel, commissioning_year) %>%
    mutate(
      name = str_trim(name),
      primary_fuel = str_to_title(str_trim(primary_fuel)),
      country_long = str_trim(country_long),
      commissioning_year = if_else(commissioning_year < 1901 | commissioning_year > year(Sys.Date()), NA_real_, commissioning_year),
      plant_age = year(Sys.Date()) - commissioning_year
    ) %>%
    drop_na(all_of(col_drop_na))
  return(df)
}
