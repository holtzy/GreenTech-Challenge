library(shiny)
library(rWordCloud)
ui <- fluidPage(
  headerPanel(""),
  mainPanel(
    d3CloudOutput("plot", width = "100%", height = 500),
    h1(htmlOutput("text1")))
)

library(shiny)
library(rWordCloud)
server <- function(input, output, session) {
  
  top20 <- sort(table(pest_station$LB_PARAMETRE), decreasing = TRUE)[1:20]
  top20 <- round(top20 / top20[20]*100)
  sample_data_for_wordcloud <- unlist(lapply(names(top20), function(i)rep(i,top20[i])))
  
  
  output$plot <- renderd3Cloud({
    #d3Cloud(text = names(table(sample_data_for_wordcloud))[2:10], size = unname(table(sample_data_for_wordcloud))[2:10])
    d3Cloud(text = rownames(mtcars), size = mtcars$hp)
  })
  output$text1 <- renderText({
    if(!any(names(input)=='d3word')) return ("You havent clicked")
    paste ("You have clicked ",input$d3word)
  })
}


# Run the application --------------------------------------
shinyApp(ui = ui, server = server)
