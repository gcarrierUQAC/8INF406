couleurs_marker <- c(
  "Hydro"  = "#00fa25", "Gas" = "#B8860B", "Oil" = "#B8860B", "Other" = "#FF69B4", 
  "Nuclear" = "#d6d318", "Coal" = "#A52A2A", "Wind" = "#00fa25", "Biomass" = "#B8860B", 
  "Waste" = "#B8860B", "Solar" = "#00fa25", "Geothermal" = "#00fa25", "Storage" = "#00fa25", 
  "Cogeneration" = "#FF1493", "Petcoke" = "#3d3d3d", "Wave And Tidal" = "#00fa25"
)

global_energy_production_animated_bar <- function(data) {

  data$color_code <- couleurs_marker[data$primary_fuel]
  
  plot_ly(
    data = data,
    x = ~factor(primary_fuel, levels = c("Hydro","Solar","Wind","Storage",
                                         "Geothermal","Wave And Tidal","Biomass","Waste",
                                         "Nuclear","Coal","Gas","Oil",
                                         "Petcoke","Cogeneration","Other")),
    y = ~cumulative_capacity,
    frame = ~commissioning_year,
    type = 'bar',
    color = ~primary_fuel,
    colors = couleurs_marker
  ) %>%
    layout(
      title = "Évolution de la capacité installée totale (MW) par type d'énergie",
      xaxis = list(title = "Type d'énergie"),
      yaxis = list(title = "Capacité installée totale (MW)"),
      legend = list(
        tracegroupgap = 10,
        groupclick = "toggleitem"
      ),
      paper_bgcolor = "transparent"
    ) %>%
    animation_opts(
      frame      = 200,
      transition = 0,
      redraw     = FALSE,
      mode       = "immediate"
    ) %>%
    animation_slider(
      currentvalue = list(prefix = "Année ")
    )
}
