library(shiny)
library(plotly)

source("global.R")
source("R/bubble_chart.R")
source("R/heatmap.R")
source("R/hist.R")
source("R/linechart.R")

##########################################################
              # ------------ UI --------- #
##########################################################
ui <- fluidPage(
  titlePanel("Consommation Énergétique Mondiale"),
  mainPanel(
    tabsetPanel(
      tabPanel("Emplacement Énergitique", plotlyOutput("bubbleMap")),
      tabPanel("Histogramme Animé",
               selectInput("country_choice", "Choisir un pays :", choices = sort(unique(df$country_long)),selected = "China"),
               plotlyOutput("histPlot")),
      tabPanel("Source Énergitique Par Pays",
               selectInput("choropleth_fuel","Type d'énergie :", choices = c("Tous", sort(unique(df$primary_fuel))), selected = "Tous"),
               plotlyOutput("choroplethPlot"))
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
  
}

shinyApp(ui = ui, server = server)
