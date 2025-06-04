library(shiny)
library(plotly)
library(bslib)
library(thematic)
source("global.R")

thematic_shiny(font = "auto")
Sys.setlocale("LC_CTYPE", "fr_CA.UTF-8")
options(encoding = "UTF-8")

ui <- navbarPage(
  tags$head(tags$meta(charset = "UTF-8")),
  title = "Consommation Énergétique Mondiale",
  input_dark_mode(id = "mode"),
  theme = bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Roboto")),
  windowTitle = "Shiny GPPD",
  
  tabPanel("Carte mondiale",
           fluidRow(
             column(12, card(
               status = "primary", full_screen = TRUE,
               card_header("Carte des centrales"),
               card_body(plotlyOutput("bubbleMap",  height = "100%", width = "100%"))
             ))
           )
  ),
  tabPanel("Histogramme animé",
           fluidRow(
             column(3, card(
               status = "info",
               card_header("Filtres"),
               card_body(
                 selectInput("country_choice", "Choisir un pays :",
                             choices = sort(unique(df$country_long)), selected = "Canada"
                 )
               )
             )),
             column(9, card(
               status = "info",
               card_header("Histogramme par année"),
               card_body(plotlyOutput("histPlot",  height = "100%", width = "100%"))
             ))
           )
  ),
  tabPanel("Énergie par pays",
           fluidRow(
             column(3, card(
               status = "warning",
               card_header("Filtres"),
               card_body(
                 checkboxGroupInput("choropleth_fuel", "Type de source :",
                                    choices = c("Tous", sort(unique(df$primary_fuel))), selected = "Tous")
               )
             )),
             column(9, card(
               status = "warning",
               card_header("Sources par pays"),
               card_body(plotlyOutput("choroplethPlot"))
             ))
           )
  ),
  tabPanel("Évolution globale",
           fluidRow(
             column(12, card(
               status = "success", full_screen = TRUE,
               card_header("Évolution capacité mondiale"),
               card_body(plotlyOutput("globalEnergyCapacity"))
             ))
           )
  )
)

server <- function(input, output, session){
  
  output$bubbleMap <- renderPlotly({
    bubble_map(df)
  })
  
  data_hist <- reactive({
    req(input$country_choice)
    df_filtered <- df %>%
      filter(country_long == input$country_choice) %>%
      mutate(commissioning_year = ifelse(is.na(commissioning_year), 2023, commissioning_year))
    
    # Notification seulement si il y a des NA
    if (any(is.na(df$commissioning_year))) {
      showNotification("Certaines années sont manquantes et ont été imputées vers l'année 2023.", type = "warning")
    }
    prepare_data_for_bar(df_filtered)
  })
  
  output$histPlot <- renderPlotly({
    req(data_hist())
    animated_bar(data_hist())
  })
  
  output$choroplethPlot <- renderPlotly({
    req(input$choropleth_fuel)
    country_ref <- df_country %>% select(country_long, iso3) %>% distinct()
    fuels <- if ("Tous" %in% input$choropleth_fuel) unique(df_country$primary_fuel) else input$choropleth_fuel
    d <- pad_countries(df_country, country_ref, fuels)
    d <- d %>% mutate(imputed = is.na(capacity_mw))
    plot_choropleth(d)
  })
  
  output$globalEnergyCapacity <- renderPlotly({
    req(energy_production_per_fuel_type)
    global_energy_production_animated_bar(energy_production_per_fuel_type)
  })
}

shinyApp(ui, server)
