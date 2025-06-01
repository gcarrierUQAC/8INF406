g <- list(
  scope      = "world",
  projection = list(type = "mercator"),
  showland   = TRUE,
  landcolor  = plotly::toRGB("white"),
  showocean  = TRUE,
  oceancolor = plotly::toRGB("lightblue"),
  subunitwidth  = 1,
  countrywidth  = 1,
  subunitcolor  = plotly::toRGB("gray85"),
  countrycolor  = plotly::toRGB("gray85"),
  lataxis = list(showgrid = TRUE, gridcolor = plotly::toRGB("white"), gridwidth = 0.5),
  lonaxis = list(showgrid = TRUE, gridcolor = plotly::toRGB("white"), gridwidth = 0.5)
)

couleurs_marker <- c(
  "Hydro"  = "#00fa25", "Gas" = "#B8860B", "Oil" = "#B8860B", "Other" = "#FF69B4", 
  "Nuclear" = "#d6d318", "Coal" = "#A52A2A", "Wind" = "#00fa25", "Biomass" = "#B8860B", 
  "Waste" = "#B8860B", "Solar" = "#00fa25", "Geothermal" = "#00fa25", "Storage" = "#00fa25", 
  "Cogeneration" = "#FF1493", "Petcoke" = "#3d3d3d", "Wave And Tidal" = "#00fa25"
)

symbol_marker <- c(
  "Hydro" = "circle", "Gas" = "square", "Oil" = "diamond", "Other" = "cross",
  "Nuclear" = "x", "Coal" = "triangle-up", "Wind" = "triangle-down", "Biomass" = "star",
  "Waste" = "hexagon", "Solar" = "triangle-left", "Geothermal" = "triangle-right",
  "Storage" = "pentagon", "Cogeneration" = "hourglass", "Petcoke" = "bowtie",
  "Wave And Tidal" = "104"
)

bubble_map <- function(
    data, x = "longitude", y = "latitude", 
    size = "capacity_mw", color = "primary_fuel", symbol = "primary_fuel") {
  data$color_code <- couleurs_marker[data$primary_fuel]
  
  plot_ly(
    data = data,
    lat = ~get(y),
    lon = ~get(x),
    type = "scattergeo",
    mode = "markers",
    size = ~get(size),
    color = ~get(color),
    colors = couleurs_marker,
    symbol = ~get(symbol),
    symbols = symbol_marker,
    marker = list(
      opacity = 0.8,
      sizemode = 'area',
      sizeref = 7.0 * max(data[[size]], na.rm = TRUE) / (100^2),
      line = NULL
    ),
    text = ~paste("Nom:", name, enc2utf8("<br>Capacité:"), capacity_mw, "MW<br>Pays:", country, "<br>Type:", primary_fuel)
  ) %>%
    layout(
      title = enc2utf8("Cartographie des types d'énergie dans le monde"),
      geo = g,
      showlegend = TRUE,
      legend = list(orientation = "v", xanchor = "r", yanchor = "top"),
      font = list(family = "Arial, sans-serif", size = 12, color = "#000"),
      hoverlabel = list(bgcolor = "white", bordercolor = "black"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"#,
      #legendgroup = ~color_code
    )
}

