library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cosmo"),

  
  fluidRow(width = 12, style = "margin = 20px",
           column(12, includeHTML("three-state.html"))
  )
  
)

server <- function(input, output) {
   
}

shinyApp(ui = ui, server = server)

