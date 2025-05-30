generate_bubble_map <- function(data, year) {
  df_clean <- data %>%
    filter(!is.na(commissioning_year), !is.na(capacity_mw),
           !is.na(longitude), !is.na(latitude)) %>%
    mutate(
      commissioning_year = as.integer(commissioning_year),
      primary_fuel = str_to_title(str_trim(primary_fuel)),
      plant_age = year(Sys.Date()) - commissioning_year
    ) %>%
    filter(commissioning_year >= 1900, commissioning_year <= year)
  
  bubble_map(
    data   = df_clean,
    x      = "longitude",
    y      = "latitude",
    size   = "capacity_mw",
    color  = "primary_fuel",
    symbol = "primary_fuel"
  )
}
