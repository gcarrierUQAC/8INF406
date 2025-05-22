plot_choropleth <- function(df) {
  plot_ly(
    data = df,
    type = 'choropleth',
    locations = ~iso3,
    z = ~capacity_mw,
    text = ~country_long,
    colorscale = "Portland",
    marker = list(line = list(color = toRGB("grey"), width = 0.5)),
    colorbar = list(title = 'Capacité MW'),
    locationmode = 'ISO-3',
    hoverinfo = "text",
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "Capacité installée: %{z} MW<br>"
    )
  ) %>%
    layout(
      title = 'Capacité installée (MW) par pays',
      geo = list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'equirectangular')
      )
    )
}

