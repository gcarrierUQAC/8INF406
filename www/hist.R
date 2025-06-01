couleurs_marker <- c(
  "Hydro"  = "#00fa25", "Gas" = "#B8860B", "Oil" = "#B8860B", "Other" = "#FF69B4", 
  "Nuclear" = "#d6d318", "Coal" = "#A52A2A", "Wind" = "#00fa25", "Biomass" = "#B8860B", 
  "Waste" = "#B8860B", "Solar" = "#00fa25", "Geothermal" = "#00fa25", "Storage" = "#00fa25", 
  "Cogeneration" = "#FF1493", "Petcoke" = "#3d3d3d", "Wave And Tidal" = "#00fa25"
)

animated_bar <- function(data) {
  data$color_code <- couleurs_marker[data$primary_fuel]
  
  plot_ly(
    data,
    x = ~capacity_mw,
    y = ~factor(primary_fuel, levels = c("Hydro","Solar","Wind","Storage",
                                         "Geothermal","Wave And Tidal","Biomass","Waste",
                                         "Nuclear","Coal","Gas","Oil",
                                         "Petcoke","Cogeneration","Other")),
    color = ~primary_fuel,
    colors = couleurs_marker,
    frame = ~frame,
    type = 'bar',
    orientation = 'h',
    text = ~paste("Énergie:", primary_fuel, "<br>MW:", round(capacity_mw)),
    hoverinfo = "text"
  ) %>%
    layout(
      title = list(text = "Évolution de la capacité installée par pays et type d'énergie",
                   font = list(family = "Roboto", size = 18)),
      xaxis = list(title = "Capacité installée (MW)"),
      yaxis = list(title = "Pays"),
      barmode = 'group',
      paper_bgcolor = "transparent"#,
      #legendgroup = ~color_code
      ) %>%
    animation_opts(
      frame = 200,
      transition = 0,      
      redraw = FALSE,
      mode = "immediate"
    )
}
