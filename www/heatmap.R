plot_choropleth <- function(df) {
  plot_ly(
    data = df,
    type = "choropleth",
    locations = ~iso3,
    z = ~capacity_mw,
    text = ~country_long,
    colorscale = "PuBu",
    marker = list(line = list(color = toRGB("grey"), width = 0.5)),
    colorbar = list(title = enc2utf8("Capacité MW")),
    locationmode = "ISO-3",
    hoverinfo = "text",
    hovertemplate = enc2utf8(paste(
      "<b>%{text}</b><br>",
      "Capacité installée: %{z} MW<br>"
    )
  )
  ) %>%
    layout(
      title = enc2utf8("Capacité installée (MW) par pays"),
      geo = list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = "equirectangular")
      )
    )
}

