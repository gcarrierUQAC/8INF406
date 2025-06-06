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
      currentvalue = list(prefix = enc2utf8("Année "))
    )
}
