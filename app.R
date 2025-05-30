#### Optimiser les filtrations dans les réactives
library(shiny)
library(plotly)

##########################################################
# ------------ UI --------- #
##########################################################
ui <- fluidPage(
  titlePanel("Consommation Énergétique Mondiale"),
  mainPanel(
    tabsetPanel(
      tabPanel("Emplacement Énergitique", plotlyOutput("bubbleMap")),
      tabPanel("Histogramme Animé",
               selectInput("country_choice", "Choisir un pays :", choices = sort(unique(df$country_long)),selected = "Canada"),
               plotlyOutput("histPlot")
      ),
      tabPanel("Source Énergitique Par Pays",
               selectInput("choropleth_fuel","Type d'énergie :", choices = c("Tous", sort(unique(df$primary_fuel))), selected = "Tous"),
               plotlyOutput("choroplethPlot")),
      tabPanel("Évolution des sources énergitique mondiale", plotlyOutput("globalEnergyCapacity")),
    )
  )
)
##########################################################
# --------- SERVER -------- #
##########################################################
server <- function(input, output) {
  data_bubble <- reactive({
    df
  })
  
  data_hist <- reactive({
    req(input$country_choice)
    df %>% filter(country_long == input$country_choice)
  })
  
  output$bubbleMap <- renderPlotly({
    bubble_map(data_bubble())
  })
  
  output$histPlot <- renderPlotly({
    plot_histogram(data_hist())
  })
  
  
  output$choroplethPlot <- renderPlotly({
    country_ref <- df_country %>% select(country_long, iso3) %>% distinct()
    fuel <- if ("Tous" %in% input$choropleth_fuel) unique(df_country$primary_fuel) else input$choropleth_fuel
    d <- pad_countries(df_country, country_ref, fuel)
    plot_choropleth(d)
    
  })
  
  output$globalEnergyCapacity <- renderPlotly({
    global_energy_production_animated_bar(energy_production_per_fuel_type)
  })
}

shinyApp(ui = ui, server = server)
