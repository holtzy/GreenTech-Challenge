library(shiny)
library(ECharts2Shiny)

load("DATA/env_greentech.R")
# On prépare les données : ici le top 20 des occurences des molécules dans la base de donnée moy_pest
top20 <- sort(table(pest_station$LB_PARAMETRE), decreasing = TRUE)[1:20]
top20 <- round(top20 / top20[20]*100)
sample_data_for_wordcloud <- unlist(lapply(names(top20), function(i)rep(i,top20[i])))

# Server function -------------------------------------------
server <- function(input, output) {
  renderWordcloud("test", data =sample_data_for_wordcloud)
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
}

# UI layout -------------------------------------------------
ui <- fluidPage(
  # We MUST load the ECharts javascript library in advance
  loadEChartsLibrary(),
  tags$div(id="test", style="width:100%;height:500px;"),
  deliverChart(div_id = "test")
)

# Run the application --------------------------------------
shinyApp(ui = ui, server = server)
