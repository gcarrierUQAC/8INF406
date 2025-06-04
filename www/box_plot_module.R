library(shiny)
library(plotly)
library(dplyr)

# UI function for violin plot module
title <- "Distribution de la capacité par type de combustible"
mod_boxplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             sliderInput(
               ns("year_range"),
               "Plage d'années de mise en service :",
               min = min(df$commissioning_year, na.rm = TRUE),
               max = max(df$commissioning_year, na.rm = TRUE),
               value = c(min(df$commissioning_year, na.rm = TRUE), max(df$commissioning_year, na.rm = TRUE)),
               step = 1,
               sep = ""
             )
      ),
      column(4,
             selectInput(
               ns("fuel_types"),
               "Type d'énergie primaire :",
               choices = sort(unique(df$primary_fuel)),
               selected = unique(df$primary_fuel),
               multiple = TRUE
             )
      )
    ),
    plotlyOutput(ns("capacity_violin"))
  )
}

# Server function for violin plot module
mod_boxplot_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # reactive subset of data based on inputs
    filtered_data <- reactive({
      req(data())
      data() %>%
        filter(
          !is.na(commissioning_year),
          commissioning_year >= input$year_range[1],
          commissioning_year <= input$year_range[2],
          primary_fuel %in% input$fuel_types
        )
    })
    
    # render interactive violin plot
    output$capacity_violin <- renderPlotly({
      req(filtered_data())
      plot_ly(
        data = filtered_data(),
        x = ~primary_fuel,
        y = ~capacity_mw,
        type = "violin",
        color = ~primary_fuel,
        points = "outliers"
      ) %>%
        layout(
          title = title,
          xaxis = list(title = enc2utf8("Type d'énergie")),
          yaxis = list(title = enc2utf8("Capacité (MW)"))
        )
    })
  })
}
